# Get copies of files

library(dplyr)
library(rmutil)

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

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

get_title("basics")
get_title("crew")
get_title("episode")
get_title("principals")

