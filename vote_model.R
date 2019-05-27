# Vote Model
# Load the votes data harvested from IMDB and apply the beta binary model
# Use this to calculate mean rating, beta mean, polarity and binary mean
# 
# Updated December 2018 to apply the betabinomial/binary model 

library(dplyr)
library(rmutil)

PROJECT_DIR <- "c:/R/IMDB"
# PROJECT_DIR <- "/home/uca78/IMDB_modelling"
DATA_DIR <- file.path(PROJECT_DIR, "data")

load(file=paste(DATA_DIR,"/votes.RData",sep=""))
load(file=paste(DATA_DIR,"/vote.model.RData",sep=""))

votes <- votes %>% filter(!is.na(Vote_sum))
vote.model <- vote.model %>% filter(!is.na(Vote_sum))

#  Apply the mixed beta model
#  (copying code from the initial_model.R program)

# Fit the Beta Binomial Binary model

# Parameters:
#  m:      Beta-Binomial distribution parameter m
#  s:      Beta-Binomial distribution parameter s
#  Scale:   proportion of votes to which the Beta-Binomial model applies
# Outputs:
#  Polarity:   Proportion of votes which are extreme values not accounted for by the model
#  binary.ppn:  Proportion of polarised votes which are maximum

# The code below fits the model for one movie.  how to apply to all?
# This version is reworked to fit the model on three parameters
# (s, m and Scale) and let polarity and binary.ppn be derived from the others.

bb_gof <-function(par){ # Beta Binomial model Goodness of Fit
  m       <- par[1]
  s       <- par[2]
  scale   <- par[3]
  votes   <- vote.model[movie,c("Vote_01","Vote_02","Vote_03","Vote_04",
                                "Vote_05","Vote_06","Vote_07","Vote_08",
                                "Vote_09","Vote_10")]
  model      <- vote.model$Vote_sum[movie]*scale*dbetabinom(0:9,9,m,s)
  pct_delta  <- votes - model
  pct_delta[1]  <- min(0,pct_delta$Vote_01)
  pct_delta[10] <- min(0,pct_delta$Vote_10)
  delta <- sum(pct_delta^2)
  return(delta)
}
# Test the modelling - original
# movie=200
# mixed_fit   <- optim(c(1,1,0.1),mixed_gof,method="L-BFGS-B",lower=c(0,0,0))
# mixed_fit$par
# mixed_fit   <- optim(c(.5,2,0.1),bb_gof,method="L-BFGS-B",lower=c(0.001,0.001,0.001),upper=c(0.999,Inf,1) )
# mixed_fit$par
# mixed_fit$message

# Initialise the vote model for Beta binomial model - Comment this out after initial setup
# vote.model            <- votes
# vote.model$m          <- as.numeric(NA)
# vote.model$s          <- as.numeric(NA)
# vote.model$scale      <- as.numeric(NA)
# vote.model$polarity   <- as.numeric(NA)
# vote.model$binary.ppn <- as.numeric(NA)

# Update the vote model
# Add newly harvested Votes to vote.model:
# existing   <- unique(vote.model[c("tconst","Date")])
new.votes  <- votes %>% anti_join(vote.model,by=c("tconst","Date"))
vote.model <- full_join(vote.model,new.votes)

# Fit the model 
modelled   <- sum(is.na(vote.model$m)==F) # Count of modelled
unmodelled <- sum(is.na(vote.model$m)==T) # Count of Unmodelled
count <- 0
start.time <- Sys.time()
for (movie in 1:length(vote.model$tconst)){
  # for (movie in 1:2){
    # if (vote.model$tconst[movie] == 'tt5870084') next # this record creates problems - or it used to
  if (is.na(vote.model$m[movie])==T){
    count <- count + 1
    ETA <- Sys.time() + (unmodelled -count) * (Sys.time() - start.time)/count
    fit <- optim(c(.5,2,0.1),bb_gof,method="L-BFGS-B",lower=c(0.001,0.001,0.001),upper=c(0.999,Inf,1) )
    print(paste("Movie", vote.model$tconst[movie],
                "number",movie,"of",length(votes$tconst),
                "at time",format(Sys.time(),"%H:%M:%S"),
                "Start:",format(start.time,"%H:%M:%S"),
                "ETA:",format(ETA,"%H:%M:%S")))
    m        <- fit$par[1]
    s        <- fit$par[2]
    scale    <- fit$par[3]
    vote.model$m[movie]     <- m
    vote.model$s[movie]     <- s
    vote.model$scale[movie]     <- scale
    modelled.votes <- vote.model$Vote_sum[movie]*scale*dbetabinom(0:9,9,m,s)
    resid.1  <- max(0,vote.model$Vote_01[movie] - modelled.votes[1])
    resid.10 <- max(0,vote.model$Vote_10[movie] - modelled.votes[10])
    vote.model$polarity[movie]   <- (resid.1 + resid.10)/vote.model$Vote_sum[movie]
    vote.model$binary.ppn[movie] <- resid.10/(resid.1 + resid.10)
  }
}

# Polarity Check
summary(vote.model$polarity)
hist(vote.model$polarity)

# Scale Check
summary(vote.model$scale)
hist(vote.model$scale)

# Binary Check
summary(vote.model$binary.ppn)
hist(vote.model$binary.ppn)

# vote.model[(is.na(vote.model$polarity)==FALSE)&(vote.model$polarity<0.1),]

# Extreme movies
head(vote.model[order(vote.model$polarity),],10)
head(vote.model[order(-vote.model$polarity),],10)

# Calculate Means
vote.model$mean <- (1*vote.model$Vote_01 +
                    2*vote.model$Vote_02 +
                    3*vote.model$Vote_03 +
                    4*vote.model$Vote_04 +
                    5*vote.model$Vote_05 +
                    6*vote.model$Vote_06 +
                    7*vote.model$Vote_07 +
                    8*vote.model$Vote_08 +
                    9*vote.model$Vote_09 +
                    10*vote.model$Vote_10)/vote.model$Vote_sum
# Median
vote.model$median <- as.numeric(NA)
for (movie in 1:length(votes$tconst)){
  cume <-0
  v <- vote.model[movie,c("Vote_01","Vote_02","Vote_03","Vote_04","Vote_05","Vote_06","Vote_07","Vote_08","Vote_09","Vote_10")]
  for (j in 1:10){
    cume <- cume+v[j]
    if (cume>vote.model$Vote_sum[movie]/2){
      vote.model$median[movie] <- j
      break
    }
  }
}

#Beta Mean
vote.model$beta.mean <- as.numeric(NA)
for (movie in 1:length(votes$tconst)){
  vote.model$beta.mean[movie] <- sum(dbetabinom(0:9,9,vote.model$m[movie],vote.model$s[movie])*(1:10))
  }
# Binary mean
vote.model$binary.mean <- 1+9*vote.model$binary.ppn

#  Save the Results
save(vote.model,file=paste(DATA_DIR,"/vote.model.RData",sep=""))
save.image(file=paste(DATA_DIR,"/IMDB_Stats.RData",sep=""))
print("Program finished")
timestamp()

