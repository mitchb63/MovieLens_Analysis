#############################
# Library Imports and Setup #
#############################

#  Load packages 
library(tidyverse)
library(data.table)
library(lubridate)

# Set Working Dir
# Home PC
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Analysis")
###################################################################

# read in the dataset and view
df_movies <- read.csv("data/movies_metadata.csv", stringsAsFactors = FALSE)
df_ratings <- read.csv("data/ratings.csv", stringsAsFactors = FALSE)
df_keywords <- read.csv("data/keywords_clean.csv", stringsAsFactors = FALSE)
df_cast <- read.csv("data/cast_clean.csv", stringsAsFactors = FALSE)
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Analysis/data")
crew_files <- list.files(pattern = 'crew_clean(?:[1-9]|[1-2][0-9]|3[0-8]).csv')
df_crew <- bind_rows(lapply(crew_files, fread))
df_crew <- df_crew[,1:3]
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Analysis")
df_collections <- read.csv("data/movies_collections.csv", stringsAsFactors = TRUE)
df_countries <- read.csv("data/movies_countries.csv", stringsAsFactors = TRUE)
df_genres <- read.csv("data/movies_genres.csv", stringsAsFactors = TRUE)
df_languages <- read.csv("data/movies_languages.csv", stringsAsFactors = TRUE)

cols <- c(6,3,8,11,15:17,19,21,23,24)
df_movies <- df_movies[, cols]
glimpse(df_movies)
summary(df_movies)

glimpse(df_ratings)
summary(df_ratings)

glimpse(df_keywords)
summary(df_keywords)

glimpse(df_cast)
summary(df_cast)

glimpse(df_crew)
summary(df_crew)

glimpse(df_collections)
summary(df_collections)

glimpse(df_countries)
summary(df_countries)

glimpse(df_genres)
summary(df_genres)

glimpse(df_languages)
summary(df_languages)


