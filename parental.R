#  Rip Parental Guidance details
#  23/06/2022 - first version.  Some problems where a movie did not have all five guides.
#  24/06/2022.  Updated to return blank values if not included.

library(rvest)
library(dplyr)
library(rmutil)
library(stringr)

options(timeout= 4000000)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/IMDB"
DATA_DIR    <- "c:/R/IMDB/data"
FILE_DIR    <- "c:/R/IMDB/data/tsv"

load(paste0(DATA_DIR,"/basics.RData"))
load(paste0(DATA_DIR,"/ratings.RData"))

# Read Parental Guidance details for a single movie
guide_rip <- function(tconst){
  url <- paste0('https://www.imdb.com/title/',tconst,'/parentalguide')
  #Reading the HTML code from the website
  webpage <- read_html(url)
  mpaa_html  <- html_nodes(webpage,'td')
  if (length(mpaa_html)==4){
    mpaa <- html_text(mpaa_html[2])  
    mpaa_words <-unlist(str_split(gsub("[()]","",mpaa), " ")) 
    mpaa_pos <- max(which(mpaa_words=="Rated"),0)
    mpaa_rating <- mpaa_words[mpaa_pos+1]
  } else {
       mpaa <- ""
       mpaa_rating <- ""}
  cert_html   <- html_nodes(webpage,'.ipl-inline-list')
  if (length(cert_html)==2){  certificate <- gsub("\n",",",html_text2(cert_html[2]))
     } else {certificate <- ""}
  guide_html <- html_nodes(webpage,'.ipl-status-pill')
  if (length(guide_html)==0){
    sex       <- ""
    violence  <- ""
    profanity <- ""
    drugs     <- ""
    intense   <- ""
  } else {
    i=1
    sex <- html_text(guide_html[i])
    if (sex==""){i=i+1}  else {i=i+2}
    violence <- html_text(guide_html[i])
    if (violence==""){i=i+1}  else {i=i+2}
    profanity <- html_text(guide_html[i])
    if (profanity==""){i=i+1}  else {i=i+2}
    drugs <- html_text(guide_html[i])
    if (drugs==""){i=i+1}  else {i=i+2}
    intense <- html_text(guide_html[i])
  }
  guide     <- data.frame(tconst,sex,violence,profanity,drugs,intense,
                          mpaa_rating,mpaa,certificate,stringsAsFactors=FALSE)
  return(guide)
}

# Test movies
#guide_rip("tt0452694")
#guide_rip("tt8783930")
# Some problem movies:
# guide_rip("tt7668842") 
# guide_rip("tt0385267") 
# guide_rip("tt0183869")
# guide_rip("tt0245803")
# guide_rip("tt0417217")
#guide_rip("tt1772925") # no ratings
#guide_rip("tt2574698") # some, not all ratings
#guide_rip("tt1179782") # table is empty
#guide_rip("tt0216707")
#guide_rip("tt2404435")
#guide_rip("tt5742374")

# Movies to get parental guides
keeptypes <- c("movie","tvMovie","tvMiniSeries","tvSeries","videoGame")  # List of types to keep

movies <- basics %>%  filter(titleType %in% keeptypes) %>%
  left_join(ratings %>% select(tconst,averageRating,numVotes),by="tconst") %>%
  select(-endYear) %>%
  filter(numVotes>200000) %>% arrange(-numVotes)

movie_ids <- movies %>% select(tconst)

if (file.exists(paste0(DATA_DIR,"/parental.RData"))){
  load(file=paste0(DATA_DIR,"/parental.RData"))
} else {
  parental <- guide_rip(movie_ids$tconst[1])  # Initialise votes data frame
}

parent_ids <- parental %>% select(tconst)

movie_ids <- movie_ids %>% anti_join(parent_ids)

while(nrow(movie_ids)>0){
  # Identify movies to get guide
  print(paste("Movies to Rip :",nrow((movie_ids))))
  for (i in 1:min(nrow(movie_ids),100)){
    tryCatch({
      id <- movie_ids$tconst[i]
      movieTitle <- as.character(movies %>% filter(tconst==id) %>% select(primaryTitle))
      print(paste(i,"Movie",id,movieTitle))
      parental <- bind_rows(parental,guide_rip(id))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Movie Votes Data Frame")
  parental <- parental %>% unique()
  save(parental,file=paste0(DATA_DIR,"/parental.RData"))
  parent_ids <- parental %>% select(tconst)
  movie_ids <- movie_ids %>% anti_join(parent_ids)
}

parental_guide <- movies %>% inner_join(parental,by="tconst") %>%
  mutate(sex_code = case_when(sex=="None"~1,
                              sex=="Mild"~2,
                              sex=="Moderate"~3,
                              sex=="Severe"~4),
         violence_code = case_when(violence =="None"~1,
                                   violence =="Mild"~2,
                                   violence =="Moderate"~3,
                                   violence =="Severe"~4),
         profanity_code = case_when(profanity=="None"~1,
                                    profanity=="Mild"~2,
                                    profanity=="Moderate"~3,
                                    profanity=="Severe"~4),
         drug_code = case_when(drugs=="None"~1,
                               drugs=="Mild"~2,
                               drugs=="Moderate"~3,
                               drugs=="Severe"~4),
         intense_code = case_when(intense=="None"~1,
                                  intense=="Mild"~2,
                                  intense=="Moderate"~3,
                                  intense=="Severe"~4)
  )

table(parental_guide$mpaa_rating)

save(parental_guide,file=paste0(DATA_DIR,"/parental_guide.Rdata"))
write.csv(parental_guide,paste0(DATA_DIR,"/IMDB_parental_guide.csv"),row.names = FALSE)

table(parental$sex)
table(parental$violence)
table(parental$profanity)
table(parental$drugs)
table(parental$intense)

