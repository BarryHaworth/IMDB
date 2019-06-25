# Filter the data files
# For an arbitrary number of votes, remove all movies with fewer votes from:
# votes, vote.model, metadata, basics

library(dplyr)

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

load(file=paste(DATA_DIR,"/votes.RData",sep=""))
load(file=paste(DATA_DIR,"/vote.model.RData",sep=""))
load(file=paste(DATA_DIR,"/metascores.RData",sep=""))
load(file=paste(DATA_DIR,"/basics.RData",sep=""))

min.votes = 1000000  # 27 movies with at least 10^6 votes

# filter

ids      <- unique(votes$tconst[(votes$Vote_sum > min.votes)])  # movies with at least the min number of votes

votes      <- votes      %>% filter(tconst %in% ids)
vote.model <- vote.model %>% filter(tconst %in% ids)
metascores <- metascores %>% filter(tconst %in% ids)
basics     <- basics     %>% filter(tconst %in% ids)
                       

