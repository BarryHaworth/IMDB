## Rating History
## Read and merge all the Rating counts into a single file by date

library(dplyr)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB_Archive/data"

load(file=paste(DATA_DIR,"/ratings.RData",sep=""))
load(file=paste(DATA_DIR,"/basics.RData",sep=""))
load(file=paste(DATA_DIR,"/votes.RData",sep=""))

# Function to read a ratings data file and add the date
read_rat <- function(date){  
  r <- read.delim(paste(FILE_DIR,"/ratings-",date,".tsv.gz",sep=""),stringsAsFactors = FALSE)
  r$Date <- as.Date(date)
  r <- r %>% filter(numVotes >= 100)  # Filter out movies with fewer than 100 votes
  return(r)
}

# Initialise the Rating History file
# start.date <- as.Date("2018-05-14")
# rating.history <- read_rat(start.date)

# Update the Rating History file
start.date <- Sys.Date() - 10
# start.date <- max(rating.history$Date)

end.date   <- Sys.Date()


# Initialise the Rating History file
rating.history <- read_rat(start.date)

for(d in seq.Date(start.date+1,end.date,1)){
  # print(file.exists(paste(FILE_DIR,"/ratings-",as.Date(d,origin = "1970-01-01"),".tsv.gz",sep="")))
  if (file.exists(paste(FILE_DIR,"/ratings-",as.Date(d,origin = "1970-01-01"),".tsv.gz",sep=""))){
    print(paste("Reading ratings for",as.Date(d,origin = "1970-01-01")))
    latest <- read_rat(as.Date(d,origin = "1970-01-01")) 
    rating.history <- rbind(rating.history,latest)
  }
}

rating.history <- rating.history  %>% distinct()

save(rating.history,file=paste(DATA_DIR,"/rating.history.RData",sep=""))
