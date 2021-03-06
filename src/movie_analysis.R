#############################
# Library Imports and Setup #
#############################

#  Load packages 
library(tidyverse)
library(lubridate)

options(scipen = 999, digits = 6)

###################################################################

# read in the dataset and view
df_movies_init <- read_csv("data/movies_metadata.csv")
df_ratings <- read_csv("data/ratings.csv")
df_keywords <- read_csv("data/keywords_clean.csv")
df_cast <- read_csv("data/cast_clean.csv")
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Project/data")
crew_files <- list.files(pattern = 'crew_clean(?:[1-9]|[1-2][0-9]|3[0-8]).csv')
df_crew <- bind_rows(lapply(crew_files, read_csv))
df_crew <- as.data.frame(df_crew[,1:3])
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Project")
df_collections <- read_csv("data/movies_collections.csv")
df_countries <- read_csv("data/movies_countries.csv")
df_genres <- read_csv("data/movies_genres.csv")
df_languages <- read_csv("data/movies_languages.csv")

cols <- c(6,3,8,11,15:17,19,21,23,24)
df_movies_init <- df_movies_init[, cols]

#  Convert date fields
df_movies_init$release_date <- ymd(df_movies_init$release_date)
df_ratings$timestamp <- as.POSIXct(df_ratings$timestamp, origin="1970-01-01")
df_movies_init$release_month <- month(df_movies_init$release_date, label = TRUE)
df_movies_init$release_year <- year(df_movies_init$release_date)


# Convert selected columns to integers
df_movie_ints <- c(2, 6,7,11)
df_movies_init[,df_movie_ints] <- lapply(df_movies_init[,df_movie_ints] , as.integer)

# Convert selected columns to doubles
df_movie_doubles <- c(4, 10)
df_movies_init[,df_movie_doubles] <- lapply(df_movies_init[,df_movie_doubles] , as.double)

# Convert selected columns to factors
df_movie_fctrs <- c(1,3,8,9, 13)
df_movies_init[,df_movie_fctrs] <- lapply(df_movies_init[,df_movie_fctrs] , factor)

df_rating_fctrs <- c(1,2)
df_ratings[,df_rating_fctrs] <- lapply(df_ratings[,df_rating_fctrs] , factor)

df_keyword_fctrs <- c(1:21)
df_keywords[,df_keyword_fctrs] <- lapply(df_keywords[,df_keyword_fctrs] , factor)

df_cast_fctrs <- c(1:6)
df_cast[,df_cast_fctrs] <- lapply(df_cast[,df_cast_fctrs] , factor)

df_crew_fctrs <- c(1:3)
df_crew[,df_crew_fctrs] <- lapply(df_crew[,df_crew_fctrs] , factor)

df_collections_fctrs <- c(1:2)
df_collections[,df_collections_fctrs] <- lapply(df_collections[,df_collections_fctrs] , factor)

df_countries_fctrs <- c(1:26)
df_countries[,df_countries_fctrs] <- lapply(df_countries[,df_countries_fctrs] , factor)

df_genres_fctrs <- c(1:9)
df_genres[,df_genres_fctrs] <- lapply(df_genres[,df_genres_fctrs] , factor)

df_languages_fctrs <- c(1:11)
df_languages[,df_languages_fctrs] <- lapply(df_languages[,df_languages_fctrs] , factor)

# Function to replace '0's' with NA's
replace_zeros <- function (x){
  if (is.na(x)){
    NA}
  else if (x == 0){
    NA}
  else{
    x}
}

# Apply the function to clean the zeros
df_movies_init$budget <- unlist(lapply(df_movies_init$budget, replace_zeros))
df_movies_init$revenue <- unlist(lapply(df_movies_init$revenue, replace_zeros))
df_movies_init$vote_average <- unlist(lapply(df_movies_init$vote_average, replace_zeros))
df_movies_init$runtime <- unlist(lapply(df_movies_init$runtime, replace_zeros))

# Remove duplicate rows from all dataframes

df_movies_init <- df_movies_init %>% distinct(id, .keep_all = TRUE)
df_ratings <- df_ratings %>% distinct(userId, movieId, .keep_all = TRUE)
df_keywords <- df_keywords %>% distinct(id, .keep_all = TRUE)
df_cast <- df_cast %>% distinct(id, .keep_all = TRUE)
df_crew <- df_crew %>% distinct(movie_ID, Job, .keep_all = TRUE)
df_countries <- df_countries %>% distinct(id, .keep_all = TRUE)
df_genres <- df_genres %>% distinct(id, .keep_all = TRUE)
df_languages <- df_languages %>% distinct(id, .keep_all = TRUE)
df_collections <- df_collections %>% distinct(id, .keep_all = TRUE)

glimpse(df_movies_init)
summary(df_movies_init)

###################################################################################################
# Filter the dataset to include only released movies with a budget and revenue greater than zero
df_movies_base <- df_movies_init %>%
  filter(status == 'Released')

glimpse(df_movies_base)
summary(df_movies_base)

ggplot(df_movies_base) +
  geom_boxplot(aes(x = 1, y = budget)) +
  coord_flip()

# Remove outliers
df_movies_base_no_outliers <- df_movies_base %>%
  filter(budget < 59500000)

glimpse(df_movies_base_no_outliers)
summary(df_movies_base_no_outliers)

ggplot(df_movies_base_no_outliers) +
  geom_boxplot(aes(x = 1, y = budget)) +
  coord_flip()

df_movies_raw <- df_movies_base_no_outliers

# Create new profit and profit margin variables
df_movies_raw <- df_movies_raw %>%
  mutate(profit = revenue - budget, profit_margin = profit/revenue)

movie_div <-quantile(df_movies_raw$profit_margin, probs = 0.80, na.rm = TRUE)

# Function to assign each movie to one of two categories
assign_movie <- function(x) {
  if (is.na(x)){
    'NA'}
  else if (x >= movie_div){
    'Top_20'}
  else{
    'Bottom_80'}
}

# Apply the function to categorize each movie based on profit margin
df_movies_raw$profit_group <- as.factor(unlist(lapply(df_movies_raw$profit_margin, assign_movie)))

#########################################################################################
# Add the average rating value for each movie
glimpse(df_ratings)
summary(df_ratings)

df_ratings_avg <- df_ratings %>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating))

df_movies <- df_movies_raw %>%
  left_join(df_ratings_avg, by = c('id' = 'movieId'))

##########################################################################################
# Process the keyword data
glimpse(df_keywords)
head(df_keywords)

# Select the first 10 keywords for each film
df_keywords <- df_keywords[,1:11]

# Get the id and profit group for each id
glimpse(df_movies)

# Create a dataframe with id, release year and profit_group
group_cols <- c(1,13,16)
df_groups <- df_movies[, group_cols]
df_groups <- df_groups %>% 
  filter(!is.na(release_year))
glimpse(df_groups)
summary(df_groups)

# Join the profit_group data to the keyword data
df_keywords_grps <- df_keywords %>%
  left_join(df_groups, by = 'id')

# Tidy the dataframe
df_keywords_tidy <- df_keywords_grps %>%
  pivot_longer(cols = 2:11, names_to = "keyword_num", values_to = "keyword")
df_keywords_tidy <-  df_keywords_tidy %>% 
  filter(!is.na(keyword))
summary(df_keywords_tidy)

df_keywords_top <- df_keywords_tidy %>%
  filter(profit_group == 'Top_20')

df_keywords_bottom <- df_keywords_tidy %>%
  filter(profit_group == 'Bottom_80')

glimpse(df_keywords_top)

# Function to extract number of unique values of a factor
examine_column <- function(x) {
  df <- as.data.frame(table(x)) %>%
  arrange(desc(Freq))
  print(df)
  return(df)
}

# Create lists of the keywords used in each group sorted by descending Freq
df_top_keywords <- examine_column(df_keywords_top$keyword)
df_bottom_keywords <- examine_column(df_keywords_bottom$keyword)

# Looking at the top n rows of each file
sample_rows <- c(1:50)
sample_top <- df_top_keywords[sample_rows,]
sample_bottom <- df_bottom_keywords[sample_rows,]
keyword_differences <- anti_join(sample_top, sample_bottom, by = 'x')
keyword_diff_list <- as.vector(keyword_differences$x)


# Function to flag rows with keywords from the keyword difference list
flg_keywords <- function(x) {
  if (x %in% keyword_diff_list){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_keywords_tidy$keyword_flg <- unlist(lapply(df_keywords_tidy$keyword, flg_keywords))

glimpse(df_keywords_tidy)
head(df_keywords_tidy)

key_cols <- c(1,5)
df_keywords_join <- df_keywords_tidy[,key_cols] %>%
  group_by(id) %>%
  summarize(key_sum = sum(keyword_flg))

# Function to flag movies with keywords from the keyword difference list
flg_keyword_films <- function(x) {
  if (x != 0){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_keywords_join$keyword_bin <- factor(unlist(lapply(df_keywords_join$key_sum, flg_keyword_films)))

df_movies <- df_movies %>%
  left_join(df_keywords_join, by = 'id')

glimpse(df_movies)
summary(df_movies)

##########################################################################################
# Process the cast data
glimpse(df_cast)
summary(df_cast)

# Read in the IMDB "Top Actor" list
df_top_actors <- read_csv('data/Top_actors.csv')
actor_list <- as.vector(df_top_actors$Name)
glimpse(actor_list)

# Join the profit_group data with the cast data
df_cast_grps <- df_cast %>%
  left_join(df_groups, by = 'id')

df_cast_tidy <- df_cast_grps %>%
  pivot_longer(cols = 2:6, names_to = "actor_num", values_to = "actor")

examine_column(df_cast_tidy$actor)

# Function to flag rows with actors from the top actor list
flg_actors <- function(x) {
  if (x %in% actor_list){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_cast_tidy$actor_flg <- unlist(lapply(df_cast_tidy$actor, flg_actors))

actor_cols <- c(1,5)
df_cast_join <- df_cast_tidy[,actor_cols] %>%
  group_by(id) %>%
  summarize(actor_sum = sum(actor_flg))

# Function to flag movies with keywords from the keyword difference list
flg_actor_films <- function(x) {
  if (x != 0){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_cast_join$actor_bin <- factor(unlist(lapply(df_cast_join$actor_sum, flg_actor_films)))

df_movies <- df_movies %>%
  left_join(df_cast_join, by = 'id')

glimpse(df_movies)
summary(df_movies)

##########################################################################################
# Process the crew data
glimpse(df_crew)
summary(df_crew)

# Read in the IMDB "Top Actor" list
df_top_producers <- read_csv('data/Top_producers.csv')
producer_list <- as.vector(df_top_producers$Name)
glimpse(producer_list)

# Join the profit_group data with the cast data
df_crew_grps <- df_crew %>%
  left_join(df_groups, by = c('movie_ID' = 'id'))

df_crew_tidy <- df_crew_grps %>%
  filter(str_detect(str_to_lower(Job),'producer') | str_detect(str_to_lower(Job),'director'))

# Function to clean the Job column
clean_jobs <- function(x){
  if(str_detect(str_to_lower(x),'producer')){
    'Producer'
  }else if(str_detect(str_to_lower(x),'director')){
    'Director'
  }else{'NA'}
}

# Apply the function to clean the Job variable
df_crew_tidy$Job <- factor(unlist(lapply(df_crew_tidy$Job, clean_jobs)))

# Function to flag rows with actors from the top actor list
flg_crew <- function(x) {
  if (x %in% producer_list){
    1
  }else{0}
}

# Apply the function to categorize each row in the crew dataframe based on the crew diff list
df_crew_tidy$crew_flg <- unlist(lapply(df_crew_tidy$Name, flg_crew))

crew_cols <- c(1,5)
df_crew_join <- df_crew_tidy[,crew_cols] %>%
  group_by(movie_ID) %>%
  summarize(crew_sum = sum(crew_flg))

# Function to flag movies with crew from the crew difference list
flg_crew_films <- function(x) {
  if (x != 0){
    1
  }else{0}
}

# Apply the function to categorize each row in the crew dataframe based on the crew diff list
df_crew_join$crew_bin <- factor(unlist(lapply(df_crew_join$crew_sum, flg_crew_films)))

df_movies <- df_movies %>%
  left_join(df_crew_join, by = c('id' = 'movie_ID'))

glimpse(df_movies)
summary(df_movies)

##########################################################################################
# Process the countries data
# Look at the top 5 countries for each film
df_countries <- df_countries[,1:6]
glimpse(df_countries)
summary(df_countries)

# Join the profit_group data with the country data
df_country_grps <- df_countries %>%
  left_join(df_groups, by = 'id')

df_countries_tidy <- df_country_grps %>%
  pivot_longer(cols = 2:6, names_to = "country_num", values_to = "country") %>%
  filter(!is.na(country))

df_countries_join_1 <- df_countries_tidy %>%
  group_by(id) %>%
  summarize(country_count = n())
summary(df_countries_join_1)

# Create top and Bottom dataframes
df_countries_top <- df_countries_tidy %>%
  filter(profit_group == 'Top_20')

df_countries_bottom <- df_countries_tidy %>%
  filter(profit_group == 'Bottom_80')

# Function to extract number of unique values of a factor
examine_column <- function(x) {
  df <- as.data.frame(table(x)) %>%
    arrange(desc(Freq))
  print(df)
  return(df)
}

# Create lists of the countries used in each group sorted by descending Freq
df_top_countries <- examine_column(df_countries_top$country)
df_bottom_countries <- examine_column(df_countries_bottom$country)


# Looking at the top n rows of each file
sample_rows <- c(1:25)
sample_top <- df_top_countries[sample_rows,]
sample_bottom <- df_bottom_countries[sample_rows,]
country_differences <- anti_join(sample_top, sample_bottom, by = 'x')
country_diff_list <- as.vector(country_differences$x)

# Function to flag rows with keywords from the keyword difference list
flg_countries <- function(x) {
  if (x %in% country_diff_list){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_countries_tidy$country_flg <- unlist(lapply(df_countries_tidy$country, flg_countries))

glimpse(df_countries_tidy)
head(df_countries_tidy)

country_cols <- c(1,5)
df_countries_join_2 <- df_countries_tidy[,country_cols] %>%
  group_by(id) %>%
  summarize(country_sum = sum(country_flg))

# Function to flag movies with keywords from the keyword difference list
flg_country_films <- function(x) {
  if (x != 0){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_countries_join_2$country_bin <- factor(unlist(lapply(df_countries_join_2$country_sum, flg_country_films)))

df_movies <- df_movies %>%
  left_join(df_countries_join_1, by = 'id') %>%
  left_join(df_countries_join_2, by = 'id')

glimpse(df_movies)
summary(df_movies)

##########################################################################################
# Process the genre data
glimpse(df_genres)
summary(df_genres)

# Join the profit_group data with the country data
df_genre_grps <- df_genres %>%
  left_join(df_groups, by = 'id')

df_genres_tidy <- df_genre_grps %>%
  pivot_longer(cols = 2:9, names_to = "genre_num", values_to = "genre")

df_genres_top <- df_genres_tidy %>%
  filter(profit_group == 'Top_20')

df_genres_bottom <- df_genres_tidy %>%
  filter(profit_group == 'Bottom_80')

df_genres_first <- df_genres_tidy %>%
  filter(genre_num == 'genre1')

# Create a variable with the first genre listed for each film
genre_cols <- c(1, 4)
df_genres_join_1 <- df_genres_first[, genre_cols]
  
summary(df_genres_join_1)
glimpse(df_genres_first)

# Apply the examine column function to produce the lists of top and bottom genres
df_top_genres <- examine_column(df_genres_top$genre)
df_bottom_genres <- examine_column(df_genres_bottom$genre)

sample_rows <- c(1:20)
sample_top <- df_top_genres[sample_rows,]
sample_bottom <- df_bottom_genres[sample_rows,]
genre_differences <- anti_join(sample_top, sample_bottom, by = 'x')
genre_diff_list <- as.vector(genre_differences$x)
genre_diff_list

# Function to flag rows with genres from the genre difference list
flg_genres <- function(x) {
  if (x %in% genre_diff_list){
    1
  }else{0}
}

# Apply the function to categorize each row in the genre dataframe based on the genre diff list
df_genres_tidy$genre_flg <- unlist(lapply(df_genres_tidy$genre, flg_genres))

glimpse(df_genres_tidy)
summary(df_genres_tidy)

key_cols <- c(1,5)
df_genres_join_2 <- df_genres_tidy[,key_cols] %>%
  group_by(id) %>%
  summarize(genre_sum = sum(genre_flg))

# Function to flag movies with genres from the genre difference list
flg_genre_films <- function(x) {
  if (x != 0){
    1
  }else{0}
}

# Apply the function to categorize each row in the genre dataframe based on the genre diff list
df_genres_join_2$genre_bin <- factor(unlist(lapply(df_genres_join_2$genre_sum, flg_genre_films)))

df_movies <- df_movies %>%
  left_join(df_genres_join_1, by = 'id') %>%
  left_join(df_genres_join_2, by = 'id')

glimpse(df_movies)
summary(df_movies)

##########################################################################################
# Process the languages data
glimpse(df_languages)
summary(df_languages)

# Join the profit_group data with the country data
df_language_grps <- df_languages %>%
  left_join(df_groups, by = 'id')

df_languages_tidy <- df_language_grps %>%
  pivot_longer(cols = 2:11, names_to = "language_num", values_to = "language") %>%
  filter(!is.na(language))

df_languages_join_1 <- df_languages_tidy %>%
  group_by(id) %>%
  summarize(language_count = n())
summary(df_languages_join_1)

# Create top and Bottom dataframes
df_languages_top <- df_languages_tidy %>%
  filter(profit_group == 'Top_20')

df_languages_bottom <- df_languages_tidy %>%
  filter(profit_group == 'Bottom_80')

# Create lists of the countries used in each group sorted by descending Freq
df_top_languages <- examine_column(df_languages_top$language)
df_bottom_languages <- examine_column(df_languages_bottom$language)


# Looking at the top n rows of each file
sample_rows <- c(1:5)
sample_top <- df_top_languages[sample_rows,]
sample_bottom <- df_bottom_languages[sample_rows,]
language_differences <- anti_join(sample_top, sample_bottom, by = 'x')
language_diff_list <- as.vector(language_differences$x)

# Function to flag rows with keywords from the keyword difference list
flg_languages <- function(x) {
  if (x %in% language_diff_list){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_languages_tidy$language_flg <- unlist(lapply(df_languages_tidy$language, flg_languages))

glimpse(df_languages_tidy)
head(df_languages_tidy)

language_cols <- c(1,5)
df_languages_join_2 <- df_languages_tidy[,language_cols] %>%
  group_by(id) %>%
  summarize(language_sum = sum(language_flg))

# Function to flag movies with keywords from the keyword difference list
flg_language_films <- function(x) {
  if (x != 0){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_languages_join_2$language_bin <- factor(unlist(lapply(df_languages_join_2$language_sum, flg_language_films)))

df_movies <- df_movies %>%
  left_join(df_languages_join_1, by = 'id') %>%
  left_join(df_languages_join_2, by = 'id')

glimpse(df_movies)
summary(df_movies)

##########################################################################################
# Process the collection data
glimpse(df_collections)
summary(df_collections)

# Create function to flag movies that are part of a collection
  assign_collection <- function (x){
    if (is.na(x)){
      0}
    else if (as.character(x) == ''){
      0}
      else{
        1
      }
    }
 
# Apply the function to categorize each row in the collections dataframe based on the presence of a collection
  df_collections$collection_bin <- factor(unlist(lapply(df_collections$collection, assign_collection)))
  
# Join the profit_group data with the collection data
  df_movies <- df_movies %>%
    left_join(df_collections, by = 'id')

##########################################
# Output the resulting files
print("writing movie data csv...")
write_csv(df_movies, "data/processed/df_movies.csv")

print("writing parameter lists csv...")
write_csv(as.data.frame(keyword_diff_list), "data/processed/keyword_diff_list.csv")
write_csv(as.data.frame(actor_list), "data/processed/actor_list.csv")
write_csv(as.data.frame(producer_list), "data/processed/producer_list.csv")
write_csv(as.data.frame(country_diff_list), "data/processed/country_diff_list.csv")
write_csv(as.data.frame(genre_diff_list), "data/processed/genre_diff_list.csv")
write_csv(as.data.frame(language_diff_list), "data/processed/language_diff_list.csv")

write_csv(as.data.frame(df_keywords_tidy), "data/processed/keywords_tidy.csv")

