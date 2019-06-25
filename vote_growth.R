## Scratch code to check the growth in votes

library(dplyr)
library(ggplot2)

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

load(file=paste(DATA_DIR,"/votes.RData",sep=""))
#load(file=paste(DATA_DIR,"/ratings.RData",sep=""))
load(file=paste(DATA_DIR,"/rating.history.RData",sep=""))

movie_id <- "tt4154664"  # Captain Marvel
movie_id <- "tt3861390"  # Dumbo
movie_id <- "tt0448115"  # Shazam!
movie_id <- "tt1825683"  # Black Panther
movie_id <- "tt0076759"  # Star Wars
movie_id <- "tt0068646"  # The Godfather

single <- rating.history %>% filter(tconst == movie_id) 
single.votes <- votes %>% filter(tconst == movie_id)

single.ts <- ts(cbind(single$Date,single$numVotes,single$averageRating))

single
single.votes
single.ts

plot(single$Date,single$numVotes)

lm(single$numVotes~single$Date)
lm(single[-1:-5,]$numVotes~single[-1:-5,]$Date)

basics  %>% filter(tconst == movie_id)
ratings %>% filter(tconst == movie_id)
votes   %>% filter(tconst == movie_id)
