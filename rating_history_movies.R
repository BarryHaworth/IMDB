# Rating History
# Read and merge all the Rating counts into a single file by date
# Identify records which are not TV episodes and keep only those.

library(dplyr)
library(ggplot2)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

load(paste0(DATA_DIR,"/basics.RData"))

# Filter out the TV Episodes

# Initialise the Rating History file
start.date <- as.Date("2018-05-14")  # The oldest history file I have
end.date   <- Sys.Date()

# Identify the date range

latest.date <- start.date

for(d in seq.Date(start.date+1,end.date,1)){
  d.date <- as.Date(d,origin = "1970-01-01")
  if (file.exists(paste0(FILE_DIR,"/ratings-",as.Date(d,origin = "1970-01-01"),".tsv.gz"))){
    latest.date <- d.date
  }
}

end.date    <- latest.date
latest.data <- read.delim(paste(FILE_DIR,"/ratings-",latest.date,".tsv.gz",sep=""),stringsAsFactors = FALSE)
ggplot(latest.data, aes(x=numVotes)) + geom_histogram() + scale_x_log10()

summary(latest.data)

# Identify movies in the latest data

filter.movie <- basics %>% filter(titleType %in% c("movie","tvSeries","tvMovie")) %>% select(tconst)

# Update the Rating History file

# Function to read a ratings data file and add the date
read_rat <- function(date){  
  r <- read.delim(paste(FILE_DIR,"/ratings-",date,".tsv.gz",sep=""),stringsAsFactors = FALSE)
  r <- r %>% filter(numVotes >= 50) %>% 
    inner_join(filter.movie,by="tconst")  # Filter out TV Episodes
  r$Date <- as.Date(date)
  return(r)
}

rating.history.movie <- read_rat(start.date)
days <- as.numeric(end.date - start.date)
start.time <- Sys.time()

for(d in seq.Date(start.date+1,end.date,1)){
  d.date <- as.Date(d,origin = "1970-01-01")
  count <- as.numeric(d.date - start.date)
  ETA <- Sys.time() + (days-count) * (Sys.time() - start.time)/count
  if (file.exists(paste0(FILE_DIR,"/ratings-",as.Date(d,origin = "1970-01-01"),".tsv.gz"))){
    print(paste("Reading ratings for",d.date," Day",count,"of",days,"ETA:",ETA))
    latest <- read_rat(d.date)
    rating.history.movie <- rbind(rating.history.movie,latest) 
    gc()
  }
}

rating.history.movie <- rating.history.movie  %>% distinct()

save(rating.history.movie,file=paste0(DATA_DIR,"/rating.history.movie.RData"))

summary(rating.history.movie)
ggplot(rating.history.movie, aes(x=numVotes)) + geom_histogram() + scale_x_log10()
ggplot(rating.history.movie, aes(x=averageRating)) + geom_histogram() 
