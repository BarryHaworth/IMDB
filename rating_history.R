## Rating History
## Read and merge all the Rating counts into a single file by date

library(dplyr)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

# Function to read a ratings data file and add the date
read_rat <- function(date){  
  r <- read.delim(paste(FILE_DIR,"/ratings-",date,".tsv.gz",sep=""),stringsAsFactors = FALSE, quote="")
  r$Date <- as.Date(date)
  r <- r %>% filter(numVotes >= 100)  # Filter out movies with fewer than 100 votes
  return(r)
}

# Initialise the Rating History file
end.date   <- Sys.Date()
start.date <- Sys.Date() - 100
# start.date <- max(rating.history.full$Date)
# start.date <- as.Date("2018-05-14")  # The oldest history file I have

if (file.exists(paste0(DATA_DIR,"/rating.history.full.RData"))){
  load(paste0(DATA_DIR,"/rating.history.full.RData"))
} else {
  rating.history.full <- read_rat(start.date)
}

# Update the Rating History file

days <- as.numeric(end.date - start.date)
start.time <- Sys.time()

for(d in seq.Date(start.date+1,end.date,1)){
  d.date <- as.Date(d,origin = "1970-01-01")
  count <- as.numeric(d.date - start.date)
  ETA <- Sys.time() + (days-count) * (Sys.time() - start.time)/count
  if (file.exists(paste0(FILE_DIR,"/ratings-",as.Date(d,origin = "1970-01-01"),".tsv.gz"))){
    print(paste("Reading ratings for",d.date," Day",count,"of",days,"ETA:",ETA))
    latest <- read_rat(d.date)
    rating.history.full <- rbind(rating.history.full,latest) 
  }
}

rating.history.full <- rating.history.full  %>% distinct()

save(rating.history.full,file=paste0(DATA_DIR,"/rating.history.full.RData"))

# Create Subset - latest ratings
# Most recent 30 days of data

latest.date <- max(rating.history.full$Date)

rating.history.latest <- rating.history.full %>% filter(Date >= latest.date - 30)

save(rating.history.latest,file=paste0(DATA_DIR,"/rating.history.latest.RData"))
