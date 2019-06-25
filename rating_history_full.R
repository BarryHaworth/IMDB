# Rating History
# Read and merge all the Rating counts 
# Individual ratings-(date).tsv.gz files are read and saved as individual RData files.
# Most recent 50 files are combined into rating.history.latest 

library(dplyr)
library(ggplot2)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

# Initialise the Rating History file
start.date <- as.Date("2018-05-14")  # The oldest history file I have
end.date   <- Sys.Date()
# start.date <- end.date-10  # ten days for testing

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

# Update the Rating History file

# Function to read a ratings data file and add the date adn save to an RData file
read_rat <- function(date){  
  r <- read.delim(paste(FILE_DIR,"/ratings-",date,".tsv.gz",sep=""),stringsAsFactors = FALSE)
  r$Date <- as.Date(date)
  save(r,file=paste0(DATA_DIR,"/ratings-",date,".RData"))
}

days <- as.numeric(end.date - start.date)
start.time <- Sys.time()

for(d in seq.Date(start.date+1,end.date,1)){
  d.date <- as.Date(d,origin = "1970-01-01")
  count <- as.numeric(d.date - start.date)
  ETA <- Sys.time() + (days-count) * (Sys.time() - start.time)/count
  if (file.exists(paste0(FILE_DIR,"/ratings-",as.Date(d,origin = "1970-01-01"),".tsv.gz")) & 
      !file.exists(paste0(DATA_DIR,"/ratings-",as.Date(d,origin = "1970-01-01"),".RData"))){
    print(paste("Reading file for",d.date," Day",count,"of",days,"ETA:",ETA))
     read_rat(d.date)
  }
}

# Save the latest history (30 days)

load(paste0(DATA_DIR,"/ratings-",latest.date,".RData"))
rating.history.latest <- r

days <- 30
start.time <- Sys.time()

for(d in seq.Date(latest.date-days,latest.date-1,1)){
  d.date <- as.Date(d,origin = "1970-01-01")
  count <- as.numeric(d.date - latest.date+days)
  ETA <- Sys.time() + (days-count) * (Sys.time() - start.time)/count
  if (file.exists(paste0(DATA_DIR,"/ratings-",d.date,".RData"))){
    print(paste("History file: Adding date",d.date," Day",count,"of",days,"ETA:",ETA))
    load(paste0(DATA_DIR,"/ratings-",d.date,".RData"))
    rating.history.latest <- rating.history.latest %>% 
                             bind_rows(r) %>% 
                             arrange(tconst, Date) %>% 
                             distinct()
  }
}

save(rating.history.latest,file=paste0(DATA_DIR,"/rating.history.latest.RData"))

head(rating.history.latest,20)
summary(rating.history.latest)
ggplot(rating.history.latest, aes(x=averageRating)) + geom_histogram() 
