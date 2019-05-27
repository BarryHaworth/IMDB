# Basics
# This program downloads the title basics file from IMDB 
# and reads and processes it into the basics data frame.

library(rvest)
library(dplyr)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB_Archive/data"

if (!file.exists(paste(FILE_DIR,"/basics-",Sys.Date(),".tsv.gz",sep=""))){
  download.file("https://datasets.imdbws.com/title.basics.tsv.gz",
                paste(FILE_DIR,"/basics-",Sys.Date(),".tsv.gz",sep=""))
}

# Replace old Basics data frame with new data
basics  <- read.delim(paste(FILE_DIR,"/basics-",Sys.Date(),".tsv.gz",sep="") ,stringsAsFactors = FALSE)
# Clean Basics
# basics <- basics[basics$titleType=="movie",]  # Only keep movies
basics <- basics[basics$titleType %in% c("movie","tvSeries","video","tvMovie"),]  # Only keep selected types
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

print("Program finished")
timestamp()

