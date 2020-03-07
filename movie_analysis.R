#############################
# Library Imports and Setup #
#############################

#  Load packages 
library(tidyverse)
library(data.table)
library(lubridate)

options(scipen = 999, digits = 2)

# Set Working Dir
# Home PC
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Project")
###################################################################

# read in the dataset and view
df_movies_raw <- read.csv("data/movies_metadata.csv", stringsAsFactors = FALSE)
df_ratings <- read.csv("data/ratings.csv", stringsAsFactors = FALSE)
df_keywords <- read.csv("data/keywords_clean.csv", stringsAsFactors = FALSE)
df_cast <- read.csv("data/cast_clean.csv", stringsAsFactors = FALSE)
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Project/data")
crew_files <- list.files(pattern = 'crew_clean(?:[1-9]|[1-2][0-9]|3[0-8]).csv')
df_crew <- bind_rows(lapply(crew_files, fread))
df_crew <- as.data.frame(df_crew[,1:3])
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Project")
df_collections <- read.csv("data/movies_collections.csv", stringsAsFactors = TRUE)
df_countries <- read.csv("data/movies_countries.csv", stringsAsFactors = TRUE)
df_genres <- read.csv("data/movies_genres.csv", stringsAsFactors = TRUE)
df_languages <- read.csv("data/movies_languages.csv", stringsAsFactors = TRUE)

cols <- c(6,3,8,11,15:17,19,21,23,24)
df_movies_raw <- df_movies_raw[, cols]

#  Convert date fields
df_movies_raw$release_date <- ymd(df_movies_raw$release_date)
df_ratings$timestamp <- as.POSIXct(df_ratings$timestamp, origin="1970-01-01")

# Convert selected columns to integers
df_movie_ints <- c(2, 6,7)
df_movies_raw[,df_movie_ints] <- lapply(df_movies_raw[,df_movie_ints] , as.integer)

# Convert selected columns to doubles
df_movie_doubles <- c(4, 10)
df_movies_raw[,df_movie_doubles] <- lapply(df_movies_raw[,df_movie_doubles] , as.double)

# Convert selected columns to factors
df_movie_fctrs <- c(1,3,8,9)
df_movies_raw[,df_movie_fctrs] <- lapply(df_movies_raw[,df_movie_fctrs] , factor)

df_rating_fctrs <- c(1,2)
df_ratings[,df_rating_fctrs] <- lapply(df_ratings[,df_rating_fctrs] , factor)

df_keyword_fctrs <- c(1:21)
df_keywords[,df_keyword_fctrs] <- lapply(df_keywords[,df_keyword_fctrs] , factor)

df_cast_fctrs <- c(1:6)
df_cast[,df_cast_fctrs] <- lapply(df_cast[,df_cast_fctrs] , factor)

df_crew_fctrs <- c(1:3)
df_crew[,df_crew_fctrs] <- lapply(df_crew[,df_crew_fctrs] , factor)

df_movies_raw <- df_movies_raw %>%
  mutate(profit = revenue - budget, profit_margin = profit/revenue)

# Examine the resulting dataframes
glimpse(df_movies_raw)
summary(df_movies_raw)

df_movies_no_dups <- df_movies_raw %>% distinct(id, .keep_all = TRUE)
glimpse(df_movies_no_dups)
summary(df_movies_no_dups)

df_movies <- df_movies_no_dups %>%
  filter(status == 'Released', budget >= 10000, revenue >= 10000)%>%
  arrange(budget)
table(df_movies$budget)

movies_div <- quantile(df_movies$profit_margin, .9)

# Function to assign each movie to one of two categories
assign_movie <- function(x) {
  if (x >= .89){
    'Top_10'}
    else{
    'Bottom_90'}
  }

# Apply the function to categorize each movie based on profit margin
df_movies$profit_group <- as.factor(unlist(lapply(df_movies$profit_margin, assign_movie)))

# Function to extract unique values of a factor
examine_column <- function(x) {
  df <- as.data.frame(table(x))
  arrange(df, Freq) 
  print(df)
  return(df)
}

movie_list <- examine_column(df_movies$id)
glimpse(movie_list)
class(movie_list)

glimpse(df_movies)
summary(df_movies)

df_collections_no_dups <- df_collections %>% distinct(id, .keep_all = TRUE)
glimpse(df_collections_no_dups)
summary(df_collections_no_dups)

df_movies_w_coll <- left_join(df_movies, df_collections_no_dups, by = 'id')
glimpse(df_movies_w_coll)
summary(df_movies_w_coll)

# Function to assign each movie to one of two categories regarding collections
assign_collection <- function(x) {
  if (as.character(x) == ''){
    0}
  else{
    1}
}

# Apply the function to categorize each movie based on profit margin
df_movies_w_coll$collection_bin <- as.factor(unlist(lapply(df_movies_w_coll$collection, assign_collection)))

ggplot(df_movies_w_coll) +
  geom_bar(aes(x = collection_bin, fill = profit_group), stat = 'count', position = 'dodge')

ggplot(df_movies_w_coll) +
  geom_bar(aes(x = month(release_date), fill = profit_group), stat = 'count', position = 'dodge')

ggplot(df_movies_w_coll) + 
  geom_boxplot(aes(x = profit_group, y = budget)) +
  coord_flip()

ggplot(df_movies_w_coll) + 
  geom_boxplot(aes(x = profit_group, y = runtime)) +
  coord_flip()

ggplot(df_movies_w_coll) + 
  geom_boxplot(aes(x = profit_group, y = popularity)) +
  coord_flip()

ggplot(df_movies_w_coll) + 
  geom_boxplot(aes(x = profit_group, y = vote_average)) +
  coord_flip()



ggplot(df_movies, aes(x = budget)) +
  geom_density(bw = 50, fill = 'steelblue') 

ggplot(df_movies, aes(x = budget)) +
  geom_histogram(bins = 50) 

ggplot(df_movies, aes(x = 1, y = budget)) +
  geom_boxplot() +
  coord_flip()

ggplot(df_movies, aes(x = revenue)) +
  geom_density(bw = 50, fill = 'steelblue') 

ggplot(df_movies, aes(x = revenue)) +
  geom_histogram(bins = 50) 

ggplot(df_movies, aes(x = 1, y = revenue)) +
  geom_boxplot() +
  coord_flip()

ggplot(df_movies, aes(x = profit)) +
  geom_density(bw = 50, fill = 'steelblue') 

ggplot(df_movies, aes(x = profit)) +
  geom_histogram(bins = 50) 

ggplot(df_movies, aes(x = 1, y = profit)) +
  geom_boxplot() +
  coord_flip()

ggplot(df_movies, aes(x = profit_margin)) +
  geom_density(bw = 50, fill = 'steelblue') 

ggplot(df_movies, aes(x = profit_margin)) +
  geom_histogram(bins = 50) 

ggplot(df_movies, aes(x = 1, y = profit_margin)) +
  geom_boxplot() +
  coord_flip()

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



# Function to examine factor columns to determine if additional cleaning is necessary
examine_column <- function(x) {
  df <- data.frame(unique(x))
  arrange(df, unique.x.)
}

examine_column(df_movies$status)
examine_column(df_f1$circuitName)
examine_column(df_f1$fullName)
examine_column(df_f1$nationality)

# Function to extract unique values of a factor
examine_column <- function(x) {
  df <- as.data.frame(table(x))
  arrange(df, Freq) 
  print(df)
  return(df)
}

