# History Trend Analysis

library(dplyr)
library(ggplot2)
library(xts)
library(forecast)

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

load(paste0(DATA_DIR,"/rating.history.10K.RData"))

# Stats on rating history

history_stats <- rating.history.10K %>%
  group_by(tconst) %>%
  summarise(min_vote = min(numVotes),
            max_vote = max(numVotes),
            start_date = min(Date),
            end_date = max(Date))

head(history_stats)
summary(history_stats)

head(history_stats %>% filter(start_date > "2018-05-14"))

# Create a histogram of max votes, with x on a log scale
ggplot(history_stats %>% filter(start_date > "2018-05-14"), aes(x=start_date)) + geom_histogram()
ggplot(history_stats, aes(x=start_date)) + geom_histogram()
ggplot(history_stats, aes(x=max_vote)) + geom_histogram() + scale_x_log10()
ggplot(history_stats, aes(x=min_vote)) + geom_histogram() + scale_x_log10()
ggplot(history_stats, aes(x=min_vote, y=max_vote)) + 
  geom_point() + 
  scale_x_log10() + scale_y_log10()

# Movies with max votes > 10^5

movie_10E4 <- history_stats %>% filter(max_vote >= 10000)  %>% select(tconst)
movie_10E5 <- history_stats %>% filter(max_vote >= 100000)  %>% select(tconst)
movie_10E6 <- history_stats %>% filter(max_vote >= 1000000) %>% select(tconst)
movie_max  <- history_stats %>% filter(max_vote == max(max_vote)) %>% select(tconst)

# Create history Subsets

history_max  <- rating.history.10K %>% inner_join(movie_max, by="tconst")
history_10E6 <- rating.history.10K %>% inner_join(movie_10E6, by="tconst")
history_10E5 <- rating.history.10K %>% inner_join(movie_10E5, by="tconst")
history_10E4 <- rating.history.10K %>% inner_join(movie_10E4, by="tconst")

summary(history_max)
ggplot(history_max, aes(x=Date, y=numVotes)) + geom_line()

# Start with a single series (Max)
max_xts <- xts(history_max$numVotes,order.by = history_max$Date)
autoplot(max_xts)
autoplot(holt(max_xts,h=30)) # Holt method, 30 days forecast
autoplot(diff(max_xts))

# Forecast the series

