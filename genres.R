# Investigation of Genres
# Genres field in basics data is a text field with up to three genres separated by commas
# Wish to investigate this field, 
# summarise the different genres, 
# Work out how to filter and highlight specific genres

library(dplyr)
library(tidyr)

PROJECT_DIR <- "c:/R/IMDB"
# PROJECT_DIR <- "/home/uca78/IMDB_modelling"
DATA_DIR <- file.path(PROJECT_DIR, "data")

load(file=paste(DATA_DIR,"/basics.RData",sep=""))

summary(basics$genres)

# test fitlering on a single genre

dplyr::filter(basics,grepl("Comedy",genres))

# Get a list of all the distinct genres
# Expand the genres into a separate file, then summarise and filter on this.

genre <- basics %>%
  mutate(genre = strsplit(genres,",")) %>%
  unnest(genre) %>%
  select(c("tconst","genre"))

summary(as.factor(genre$genre))
        