# Data Download
# Download and save IMDB files 
#  Data file info https://www.imdb.com/interfaces/
#  Data files https://datasets.imdbws.com/  
# This program combines the loadlad of IMDB data files
# and the ripping of vote deails and metascores.
# It is intended as the daily automatic data update
# and replaces the programs download.R and metascore.R

library(rvest)
library(dplyr)
library(rmutil)

options(timeout= 4000000)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB_Archive/data"

load(file=paste(DATA_DIR,"/votes.RData",sep=""))
load(file=paste(DATA_DIR,"/metascores.RData",sep=""))
load(file=paste(DATA_DIR,"/rating.history.RData",sep=""))

votes <- votes %>% group_by(tconst,Date) %>% slice(1) %>% ungroup  # Filter for unique date within movie

if (!file.exists(paste(FILE_DIR,"/basics-",Sys.Date(),".tsv.gz",sep=""))){
  download.file("https://datasets.imdbws.com/title.basics.tsv.gz",
                paste(FILE_DIR,"/basics-",Sys.Date(),".tsv.gz",sep=""))
}

if (!file.exists(paste(FILE_DIR,"/ratings-",Sys.Date(),".tsv.gz",sep=""))){
  download.file("https://datasets.imdbws.com/title.ratings.tsv.gz",
                paste(FILE_DIR,"/ratings-",Sys.Date(),".tsv.gz",sep=""))
}

if (!file.exists(paste(FILE_DIR,"/episode-",Sys.Date(),".tsv.gz",sep=""))){
  download.file("https://datasets.imdbws.com/title.episode.tsv.gz",
                paste(FILE_DIR,"/episode-",Sys.Date(),".tsv.gz",sep=""))
}

# Function to read a ratings data file and add the date
read_rat <- function(date){  
  r <- read.delim(paste(FILE_DIR,"/ratings-",date,".tsv.gz",sep=""),stringsAsFactors = FALSE)
  r$Date=as.Date(date)
  return(r)
}

#  Read new data to Ratings data frame
ratings <- read_rat(Sys.Date())
# Add ratings to Ratings History
rating.history <- rbind(rating.history,ratings) %>% 
  filter(numVotes >= 100) %>% 
  filter(Date >= Sys.Date() - 7) %>% 
  distinct()

# Replace old Basics data frame with new data
basics  <- read.delim(paste(FILE_DIR,"/basics-",Sys.Date(),".tsv.gz",sep="") ,stringsAsFactors = FALSE)
# Clean Basics
# basics <- basics[basics$titleType=="movie",]  # Only keep movies
# basics <- basics[basics$titleType %in% c("movie","tvSeries","video","tvMovie"),]  # Only keep selected types
basics <- basics[is.na(basics$titleType) == FALSE,]     # Drop unknown type
basics <- basics[is.na(basics$runtimeMinutes)==FALSE,]  # Drop unknown runtime
basics <- basics[basics$startYear <= 
                   as.numeric(substr(Sys.Date(),1,4)),]   # drop release date after this year
# Set types for columns
basics$titleType <- as.factor(basics$titleType)
basics$isAdult   <- as.numeric(basics$isAdult)
basics$startYear <- as.numeric(basics$startYear)
basics$endYear   <- as.numeric(basics$endYear)
basics$runtimeMinutes <- as.numeric(basics$runtimeMinutes)
save(basics,file=paste(DATA_DIR,"/basics.RData",sep=""))

# Replace old Episode data frame with new data
episode  <- read.delim(paste(FILE_DIR,"/episode-",Sys.Date(),".tsv.gz",sep="") ,stringsAsFactors = FALSE)


# Rip individual votes for a given movie
vote_rip <- function(tconst){
  url <- paste('https://www.imdb.com/title/',tconst,'/ratings?ref_=tt_ov_rt',sep='')
  #Reading the HTML code from the website
  webpage <- read_html(url)
  rank_html <- html_nodes(webpage,'.leftAligned')
  rank_data <- html_text(rank_html)
  ranks <- rev(as.numeric(gsub(',','',rank_data[2:11])))
  Date <- Sys.Date()
  Vote_01 <- ranks[1]
  Vote_02 <- ranks[2]
  Vote_03 <- ranks[3]
  Vote_04 <- ranks[4]
  Vote_05 <- ranks[5]
  Vote_06 <- ranks[6]
  Vote_07 <- ranks[7]
  Vote_08 <- ranks[8]
  Vote_09 <- ranks[9]
  Vote_10 <- ranks[10]
  Vote_sum <- sum(ranks)
  votes <- data.frame(tconst,Date,Vote_01,Vote_02,Vote_03,Vote_04,
                      Vote_05,Vote_06,Vote_07,Vote_08,Vote_09,Vote_10,Vote_sum)
  return(votes)
}

# Read Metacritic Score for a single movie
meta_rip <- function(tconst){
  url <- paste('https://www.imdb.com/title/',tconst,'/criticreviews?ref_=tt_ov_rt',sep='')
  #Reading the HTML code from the website
  webpage <- read_html(url)
  meta_html <- html_nodes(webpage,'.metascore_wrap')
  if (length(meta_html) >0) {
    metascore      <- as.numeric(html_text(meta_html))
  } else {
    metascore <- NA
  }
  Date <- Sys.Date()
  meta <- data.frame(tconst,Date,metascore)
  return(meta)
}

# Identify growth in votes.
# The logic here is that the votes in the ratings file is usually a day 
# behind the votes on the web site.

votes.today <- ratings %>% select(c("tconst","Date","numVotes"))
votes.dates <- sort(unique(rating.history$Date))
today     <- votes.dates[length(votes.dates)]
yesterday <- votes.dates[length(votes.dates)-1]

votes.today <- rating.history %>% 
                   filter(Date == today) %>% 
                   select(c("tconst","numVotes")) %>%
                   rename(today.rat=numVotes)
  
votes.yesterday <- rating.history %>% 
  filter(Date == yesterday) %>% 
  select(c("tconst","numVotes")) %>%
  rename(yesterday.rat=numVotes)

votes.growth <- merge(votes.today,votes.yesterday,by="tconst",all.x=T) 
votes.growth$yesterday.rat[is.na(votes.growth$yesterday.rat)] <- 0

votes.growth <- votes.growth %>%
  mutate(delta = today.rat - yesterday.rat,
         votes = today.rat+delta)

max.votes.Date       <- aggregate(Date~tconst, data=votes, max)

votes.growth <- merge(votes.growth,max.votes.Date,by="tconst",all.x=T)

# IDS with votes > 1000.  This gives ~15,000 movies to check

min.votes <- 1000  # Minimum votes to be included

#ids <- data.frame(unique(votes.growth$tconst[votes.growth$votes > min.votes]),stringsAsFactors = FALSE)
#names(ids) <- "tconst"
# Filter for IDS of movies in basics list

movie_ids <- data.frame(unique(basics$tconst),stringsAsFactors = FALSE)
names(movie_ids) <- "tconst"
#ids <- merge(ids,movie_ids,by="tconst")
#ids <- merge(basics[,c("tconst","titleType","primaryTitle")],ids,by="tconst")
votes.growth <- merge(votes.growth,movie_ids,by="tconst")
votes.growth <- merge(basics[,c("tconst","titleType","primaryTitle")],votes.growth,by="tconst")
votes.growth <- votes.growth[order(-votes.growth$votes),]          # Order by descending number of votes

# Identify movies to update
#  Compile information.  For each tconst:
#  Latest number of votes from IMDB (today votes)
#  Previous number of votes from IMDB (yesterday votes)
#  Date of most recent Vote counting.


#  Update votes for: 
#    movies where number of votes has increased by 500+ or 10% (biggest increase first)
#    Up to 1000 movies where votes not yet saved (largest votes first)
#    100 movies least recently updated (oldest first)

# max.votes.Vote_sum   <- aggregate(Vote_sum~tconst, data=votes, max)
# max.votes.Date       <- aggregate(Date~tconst, data=votes, max)
# max.ratings.numVotes <- aggregate(numVotes~tconst, data=ratings, max)

# Combine with vote counts
# ids <- merge(ids,max.ratings.numVotes,by="tconst",all.x=TRUE)
# ids <- merge(ids,max.votes.Vote_sum,by="tconst",all.x=TRUE)
# ids <- merge(ids,max.votes.Date,by="tconst",all.x=TRUE)

# ids         <- ids[order(-ids$numVotes),]          # Order by descending number of votes
# ids.novotes <- ids[(is.na(ids$Vote_sum)==TRUE),]   # No Previous Vote Count
# ids.votes   <- ids[(is.na(ids$Vote_sum)==FALSE),]  # With previous Vote count
ids.novotes <- votes.growth[(is.na(votes.growth$Date)==TRUE),]  %>% filter(votes>1000)  # No Previous Vote Count
ids.votes   <- votes.growth[(is.na(votes.growth$Date)==FALSE),] %>% filter(votes>1000) # With previous Vote count

# Votes have increased by > 500 or by 10% or more
#ids.votes.plus <- ids.votes[((ids.votes$numVotes >= ids.votes$Vote_sum+500)|(ids.votes$numVotes/ids.votes$Vote_sum>1.1)),]
ids.votes.plus <- votes.growth %>% filter(votes>1000) %>% filter(Date != Sys.Date()) %>% filter( (delta > 500)|(today.rat / yesterday.rat > 1.1) ) 
# ids.votes.plus
# Votes have not increased by > 500
#ids.votes.minus <- ids.votes[!((ids.votes$numVotes >= ids.votes$Vote_sum+500)|(ids.votes$numVotes/ids.votes$Vote_sum>1.1)),]
ids.votes.minus <- votes.growth %>% filter(!is.na(Date)) %>% filter(Date != Sys.Date()) %>% filter( !(tconst %in% unique(ids.votes.plus$tconst)))
# head(ids.votes.minus)
#ids.votes.minus <- ids[(is.na(ids$Vote_sum)==FALSE)&(ids$numVotes < ids$Vote_sum+500),]

n.plus=length(ids.votes.plus$tconst)
#n.novotes=length(ids.novotes$tconst)

update.ids <- ids.votes.plus$tconst
label <- "Increased Votes"
n.ids <- length(update.ids)
count <- 0
start.time <- Sys.time()
for(id in update.ids){
  count <- count + 1
  ETA <- Sys.time() + (n.ids-count) * (Sys.time() - start.time)/count
  print(paste(label,
              "ID:",id,"number",count,"of",n.ids,
              "Started at",format(start.time,"%H:%M:%S"),
              "ETA:",format(ETA,"%H:%M:%S")))
  votes      <- rbind(votes,vote_rip(id))
  metascores <- rbind(metascores,meta_rip(id))
}
save(votes,file=paste(DATA_DIR,"/votes.RData",sep="")) # Save Votes data after each step
save(metascores,file=paste(DATA_DIR,"/metascores.RData",sep=""))

update.ids <- head(ids.novotes,1000)$tconst
label <- "New Votes"
n.ids <- length(update.ids)
count <- 0
start.time <- Sys.time()
for(id in update.ids){
  count <- count + 1
  ETA <- Sys.time() + (n.ids-count) * (Sys.time() - start.time)/count
  print(paste(label,
              "ID:",id,"number",count,"of",n.ids,
              "Started at",format(start.time,"%H:%M:%S"),
              "ETA:",format(ETA,"%H:%M:%S")))
  votes      <- rbind(votes,vote_rip(id))
  metascores <- rbind(metascores,meta_rip(id))
}
save(votes,file=paste(DATA_DIR,"/votes.RData",sep="")) # Save Votes data after each step
save(metascores,file=paste(DATA_DIR,"/metascores.RData",sep=""))

ids.votes.minus <- ids.votes.minus[order(ids.votes.minus$Date),]
print(paste("Updating 250 of the oldest voted movies. Oldest Date is",head(ids.votes.minus$Date,1)))

update.ids <- head(ids.votes.minus,250)$tconst
label <- "Old Movie Update"
n.ids <- length(update.ids)
count <- 0
start.time <- Sys.time()
for(id in update.ids){
  count <- count + 1
  ETA <- Sys.time() + (n.ids-count) * (Sys.time() - start.time)/count
  print(paste(label,
              "ID:",id,"number",count,"of",n.ids,
              "Started at",format(start.time,"%H:%M:%S"),
              "ETA:",format(ETA,"%H:%M:%S")))
  votes      <- rbind(votes,vote_rip(id))
  metascores <- rbind(metascores,meta_rip(id))
}

# Order votes & metascores by tconst

votes      <- votes[order(as.character(votes$tconst)),]
votes      <- unique(votes)
metascores <- metascores[order(as.character(metascores$tconst)),]
metascores <- unique(metascores)

# Filter records with missing votes

votes <- votes %>% filter(!is.na(Vote_sum))

# Fit the model

load(file=paste(DATA_DIR,"/vote.model.RData",sep=""))
vote.model <- vote.model %>% filter(!is.na(Vote_sum))
vote.model <- vote.model %>% group_by(tconst,Date) %>% slice(1) %>% ungroup  # Filter for unique date within movie

bb_gof <-function(par){ # Beta Binomial model Goodness of Fit
  m       <- par[1]
  s       <- par[2]
  scale   <- par[3]
  votes   <- vote.model[movie,c("Vote_01","Vote_02","Vote_03","Vote_04",
                                "Vote_05","Vote_06","Vote_07","Vote_08",
                                "Vote_09","Vote_10")]
  model      <- vote.model$Vote_sum[movie]*scale*dbetabinom(0:9,9,m,s)
  pct_delta  <- votes - model
  pct_delta[1]  <- min(0,pct_delta$Vote_01)
  pct_delta[10] <- min(0,pct_delta$Vote_10)
  delta <- sum(pct_delta^2)
  return(delta)
}

# Update the vote model
# Add newly harvested Votes to vote.model:
new.votes  <- votes %>% anti_join(vote.model,by=c("tconst","Date"))
vote.model <- full_join(vote.model,new.votes)

# Fit the model 
modelled   <- sum(is.na(vote.model$m)==F) # Count of modelled
unmodelled <- sum(is.na(vote.model$m)==T) # Count of Unmodelled
count <- 0
start.time <- Sys.time()
for (movie in 1:length(vote.model$tconst)){
  # for (movie in 1:2){
  # if (vote.model$tconst[movie] == 'tt5870084') next # this record creates problems - or it used to
  if (is.na(vote.model$m[movie])==T){
    count <- count + 1
    ETA <- Sys.time() + (unmodelled -count) * (Sys.time() - start.time)/count
    fit <- optim(c(.5,2,0.1),bb_gof,method="L-BFGS-B",lower=c(0.001,0.001,0.001),upper=c(0.999,Inf,1) )
    print(paste("Fit Model ID:", vote.model$tconst[movie],
                "number",movie,"of",length(votes$tconst),
                "at time",format(Sys.time(),"%H:%M:%S"),
                "Start:",format(start.time,"%H:%M:%S"),
                "ETA:",format(ETA,"%H:%M:%S")))
    m        <- fit$par[1]
    s        <- fit$par[2]
    scale    <- fit$par[3]
    vote.model$m[movie]     <- m
    vote.model$s[movie]     <- s
    vote.model$scale[movie]     <- scale
    modelled.votes <- vote.model$Vote_sum[movie]*scale*dbetabinom(0:9,9,m,s)
    resid.1  <- max(0,vote.model$Vote_01[movie] - modelled.votes[1])
    resid.10 <- max(0,vote.model$Vote_10[movie] - modelled.votes[10])
    vote.model$polarity[movie]   <- (resid.1 + resid.10)/vote.model$Vote_sum[movie]
    vote.model$binary.ppn[movie] <- resid.10/(resid.1 + resid.10)
  }
}

# Calculate Means
vote.model$mean <- (1*vote.model$Vote_01 +
                      2*vote.model$Vote_02 +
                      3*vote.model$Vote_03 +
                      4*vote.model$Vote_04 +
                      5*vote.model$Vote_05 +
                      6*vote.model$Vote_06 +
                      7*vote.model$Vote_07 +
                      8*vote.model$Vote_08 +
                      9*vote.model$Vote_09 +
                      10*vote.model$Vote_10)/vote.model$Vote_sum
# Median
# vote.model$median <- as.numeric(NA)
for (movie in 1:length(votes$tconst)){
  if(is.na(vote.model$median[movie])){ # Only update unknown medians
    cume <-0
    v <- vote.model[movie,c("Vote_01","Vote_02","Vote_03","Vote_04","Vote_05","Vote_06","Vote_07","Vote_08","Vote_09","Vote_10")]
    for (j in 1:10){
      cume <- cume+v[j]
      if (cume>vote.model$Vote_sum[movie]/2){
        vote.model$median[movie] <- j
        break
      }
    }
  }
}

#Beta Mean
# vote.model$beta.mean <- as.numeric(NA)
for (movie in 1:length(votes$tconst)){
  if(is.na(vote.model$beta.mean[movie])){ # Only update unknown Beta Means
    vote.model$beta.mean[movie] <- sum(dbetabinom(0:9,9,vote.model$m[movie],vote.model$s[movie])*(1:10))
    }
}
# Binary mean
# Impute missing binary proportions
vote.model$binary.ppn[is.na(vote.model$binary.ppn)] <- vote.model$beta.mean[is.na(vote.model$binary.ppn)]/10

vote.model$binary.mean <- 1+9*vote.model$binary.ppn

# Save the Results

votes <- votes %>% group_by(tconst,Date) %>% slice(1) %>% ungroup  # Filter for unique date within movie

save(votes,file=paste(DATA_DIR,"/votes.RData",sep=""))
save(metascores,file=paste(DATA_DIR,"/metascores.RData",sep=""))
save(basics,file=paste(DATA_DIR,"/basics.RData",sep=""))
save(episode,file=paste(DATA_DIR,"/episode.RData",sep=""))
save(ratings,file=paste(DATA_DIR,"/ratings.RData",sep=""))
save(vote.model,file=paste(DATA_DIR,"/vote.model.RData",sep=""))
save(ratings,file=paste(DATA_DIR,"/ratings.RData",sep=""))
save(rating.history,file=paste(DATA_DIR,"/rating.history.RData",sep=""))
# save.image(file=paste(DATA_DIR,"/IMDB_Stats.RData",sep=""))  # Comment out - not needed.
print("Program finished")
timestamp()

