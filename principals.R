#  Retrieve and read Principals data

library(rvest)
library(dplyr)
library(rmutil)

get_title <- function(file){
  local_file <- paste0(FILE_DIR,"/title.",file,".tsv.gz")
  print(paste("Local file:",local_file))
  remote_file <- paste0("https://datasets.imdbws.com/title.",file,".tsv.gz")
  print(paste("Remote File:",remote_file))
  if (!file.exists(local_file) |
      as.Date(file.info(local_file)$mtime) != Sys.Date()){
    download.file(remote_file,local_file)
  }
}

get_title("principals")

# Replace old Principals data frame with new data
principals  <- read.delim(paste0(FILE_DIR,"/title.principals.tsv.gz") ,stringsAsFactors = FALSE)
# Clean principals
# Set types for columns

save(principals,file=paste0(DATA_DIR,"/principals.RData"))