# Rating History
# Read and merge all the Rating counts into a single file by date
# Identify movies which reach at least 10K votes and keep only then

library(dplyr)
library(ggplot2)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB_Archive/data"

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

# Identify movies with 10,000 or more votes in the latest data

filter.10K <- latest.data %>% filter(numVotes >= 10000) %>% select(tconst)

# Update the Rating History file

# Function to read a ratings data file and add the date
read_rat <- function(date){  
  r <- read.delim(paste(FILE_DIR,"/ratings-",date,".tsv.gz",sep=""),stringsAsFactors = FALSE)
  r <- r %>% inner_join(filter.10K,by="tconst")  # Filter out movies with < 10K votes in latest data
  r$Date <- as.Date(date)
  return(r)
}

rating.history.10K <- read_rat(start.date)
days <- as.numeric(end.date - start.date)
start.time <- Sys.time()

for(d in seq.Date(start.date+1,end.date,1)){
  d.date <- as.Date(d,origin = "1970-01-01")
  count <- as.numeric(d.date - start.date)
  ETA <- Sys.time() + (days-count) * (Sys.time() - start.time)/count
  if (file.exists(paste0(FILE_DIR,"/ratings-",as.Date(d,origin = "1970-01-01"),".tsv.gz"))){
    print(paste("Reading ratings for",d.date," Day",count,"of",days,"ETA:",ETA))
    latest <- read_rat(d.date)
    rating.history.10K <- rbind(rating.history.10K,latest) 
  }
}

rating.history.10K <- rating.history.10K  %>% distinct()

save(rating.history.10K,file=paste0(DATA_DIR,"/rating.history.10K.RData"))

summary(rating.history.10K)
ggplot(rating.history.10K, aes(x=numVotes)) + geom_histogram() + scale_x_log10()
