#  Rip Parental Guidance details
#  23/06/2022 - first version.  Some problems where a movie did not have all five guides.
#  24/06/2022.  Updated to return blank values if not included.

# Some problem movies:
# guide_rip("tt7668842") 
# guide_rip("tt0385267") 
# guide_rip("tt0183869")
# guide_rip("tt0245803")
# guide_rip("tt0417217")

library(rvest)
library(dplyr)
library(rmutil)

options(timeout= 4000000)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

load(paste0(DATA_DIR,"/basics.RData"))
load(paste0(DATA_DIR,"/ratings.RData"))

# Read Parental Guidance details for a single movie
guide_rip <- function(tconst){
  url <- paste0('https://www.imdb.com/title/',tconst,'/parentalguide')
  #Reading the HTML code from the website
  webpage <- read_html(url)
  guide_html <- html_nodes(webpage,'.ipl-status-pill')
  if (length(guide_html)==0){
    sex       <-""
    violence  <- ""
    profanity <- ""
    drugs     <- ""
    intense   <- ""
  } else {
    i=1
    if (html_text(guide_html[i])=="") {sex <- ""                            ; i <- i+1
    } else {sex <- html_text(guide_html[i])      ; i <- i+2 }
    if (html_text(guide_html[i])=="") {violence <- ""                       ; i <- i+1
    } else {violence <- html_text(guide_html[i]) ; i <- i+2 }
    if (html_text(guide_html[i])=="") {profanity <- ""                      ; i <- i+1
    } else {profanity <- html_text(guide_html[i]); i <- i+2 }
    if (html_text(guide_html[i])=="") {drugs <- ""                          ; i <- i+1
    } else {drugs <- html_text(guide_html[i])    ; i <- i+2 }
    if (html_text(guide_html[i])=="") {intense <- ""                        ; i <- i+1
    } else {intense <- html_text(guide_html[i])  ; i <- i+2 }
  }
  guide     <- data.frame(tconst,sex,violence,profanity,drugs,intense)
  return(guide)
}

# Test movies
#guide_rip("tt0452694")
#guide_rip("tt8783930")
#guide_rip("tt1772925") # no ratings
#guide_rip("tt2574698") # some, not all ratings
guide_rip("tt1179782")
guide_rip("tt0216707")
 

# Movies to get parental guides

keeptypes <- c("movie","tvMovie","tvMiniSeries","tvSeries","videoGame")  # List of types to keep

movies <- basics %>%  filter(titleType %in% keeptypes) %>%
  left_join(ratings %>% select(tconst,averageRating,numVotes),by="tconst") %>%
  select(-endYear) %>%
  filter(numVotes>1000) %>% arrange(-numVotes)

movie_ids <- movies %>% select(tconst)

if (file.exists(paste0(DATA_DIR,"/parental.RData"))){
  load(file=paste0(DATA_DIR,"/parental.RData"))
} else {
  parental <- guide_rip(movie_ids$tconst[1])  # Initialise votes data frame
}

parent_ids <- parental %>% select(tconst)

movie_ids <- movie_ids %>% anti_join(parent_ids)

while(nrow(movie_ids)>0){
  # Identify movies to get guide
  print(paste("Movies to Rip :",nrow((movie_ids))))
  for (i in 1:min(nrow(movie_ids),100)){
    tryCatch({
      id <- movie_ids$tconst[i]
      movieTitle <- as.character(movies %>% filter(tconst==id) %>% select(primaryTitle))
      print(paste(i,"Movie",id,movieTitle))
      parental <- bind_rows(parental,guide_rip(id))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Movie Votes Data Frame")
  parental <- parental %>% unique()
  save(parental,file=paste0(DATA_DIR,"/parental.RData"))
  parent_ids <- parental %>% select(tconst)
  movie_ids <- movie_ids %>% anti_join(parent_ids)
}

rated <- movies %>% inner_join(parental,by="tconst") 
save(rated,file=paste0(DATA_DIR,"/rated.RData"))
write.csv(rated,paste0(DATA_DIR,"/rated.csv"),row.names = FALSE)

table(parental$sex)
table(parental$violence)
table(parental$profanity)
table(parental$drugs)
table(parental$intense)

