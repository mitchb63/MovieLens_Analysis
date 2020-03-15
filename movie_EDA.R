#############################
# Library Imports and Setup #
#############################

#  Load packages 
library(tidyverse)
library(lubridate)

options(scipen = 999, digits = 6)

# Set Working Dir
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Project")
###################################################################

# read in the dataset and view
df_movies_raw <- read_csv("data/processed/df_movies.csv")
actor_list <- read_csv("data/processed/actor_list.csv")
producer_list <- read_csv("data/processed/producer_list.csv")
country_diff_list <- read_csv("data/processed/country_diff_list.csv")
genre_diff_list <- read_csv("data/processed/genre_diff_list.csv")
keyword_diff_list <- read_csv("data/processed/keyword_diff_list.csv")
language_diff_list <- read_csv("data/processed/language_diff_list.csv")

glimpse(df_movies_raw)
summary(df_movies_raw)

glimpse(actor_list)
glimpse(producer_list)
glimpse(country_diff_list)
glimpse(genre_diff_list)
glimpse(keyword_diff_list)
glimpse(language_diff_list)
