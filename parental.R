#  Rip Parental Guidance details
# 23/06/2022 - first version.  Some problems where a movie did not have all five guides.

# Some problem movies:
# tt1772925 - no ratings
# tt5988370 
# tt2574698 - some, not all
# tt7668842 
# tt0385267 
# tt0183869 
# tt0245803
# tt0417217
# tt0240462 
# tt0857191 
# tt0295721 
# tt2610768 

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
  sex       <- html_text(guide_html[1])
  violence  <- html_text(guide_html[3])
  profanity <- html_text(guide_html[5])
  drugs     <- html_text(guide_html[7])
  intense   <- html_text(guide_html[9])
  guide     <- data.frame(tconst,sex,violence,profanity,drugs,intense)
  return(guide)
}

# Test movies
#guide_rip("tt0452694")
#guide_rip("tt8783930")
#guide_rip("tt0338337")
#guide_rip("tt1092026")

# Movies to get guides

movies <- basics %>% filter(titleType=="movie") %>%
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

rated <- movies %>% inner_join(parental,by="tconst") %>% filter(intense!="")
save(rated,file=paste0(DATA_DIR,"/rated.RData"))
write.csv(rated,paste0(DATA_DIR,"/rated.csv"),row.names = FALSE)

table(parental$sex)
table(parental$violence)
table(parental$profanity)
table(parental$drugs)
table(parental$intense)

parental %>% filter(intense=="")