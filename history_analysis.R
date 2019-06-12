# History Trend Analysis

library(dplyr)
library(ggplot2)

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB_Archive/data"

load(paste0(DATA_DIR,"/rating.history.full.RData"))
load(paste0(DATA_DIR,"/rating.history.latest.RData"))

# Stats on rating history

history_stats <- rating.history.full %>%
  group_by(tconst) %>%
  summarise(min_vote = min(numVotes),
            max_vote = max(numVotes),
            start_date = min(Date),
            end_date = max(Date))

head(history_stats)
summary(history_stats)

hist(history_stats$max_vote)

# Create a histogram of max votes, with x on a log scale
ggplot(history_stats, aes(x=max_vote)) + geom_histogram() + scale_x_log10()

# Movies with max votes > 10^5

movie_10E4 <- history_stats %>% filter(max_vote >= 10000)   %>% select(tconst)
movie_10E5 <- history_stats %>% filter(max_vote >= 100000)  %>% select(tconst)
movie_10E6 <- history_stats %>% filter(max_vote >= 1000000) %>% select(tconst)
movie_max <- history_stats %>% filter(max_vote == max(max_vote)) %>% select(tconst)


history_max <- rating.history.full %>% inner_join(movie_max, by="tconst")
history_10E6 <- rating.history.full %>% inner_join(movie_10E6, by="tconst")
# history_10E5 <- rating.history.full %>% inner_join(movie_10E5, by="tconst")
# history_10E4 <- rating.history.full %>% inner_join(movie_10E4, by="tconst")

summary(history_max)
ggplot(history_max, aes(x=Date, y=numVotes)) + geom_line()
