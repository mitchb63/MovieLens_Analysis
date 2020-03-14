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
df_movies_raw <- read_csv("data/movies_metadata.csv")
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
df_movies_raw <- df_movies_raw[, cols]

#  Convert date fields
df_movies_raw$release_date <- ymd(df_movies_raw$release_date)
df_ratings$timestamp <- as.POSIXct(df_ratings$timestamp, origin="1970-01-01")

# Convert selected columns to integers
df_movie_ints <- c(2, 6,7,11)
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

df_collections_fctrs <- c(1:2)
df_collections[,df_collections_fctrs] <- lapply(df_collections[,df_collections_fctrs] , factor)

df_countries_fctrs <- c(1:26)
df_countries[,df_countries_fctrs] <- lapply(df_countries[,df_countries_fctrs] , factor)

df_genres_fctrs <- c(1:9)
df_genres[,df_genres_fctrs] <- lapply(df_genres[,df_genres_fctrs] , factor)

df_languages_fctrs <- c(1:11)
df_languages[,df_languages_fctrs] <- lapply(df_languages[,df_languages_fctrs] , factor)

# Create new profit and profit margin variables
df_movies_raw <- df_movies_raw %>%
  mutate(profit = revenue - budget, profit_margin = profit/revenue)

glimpse(df_movies_raw)
summary(df_movies_raw)

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

# Remove duplicate rows from all dataframes

df_movies_raw <- df_movies_raw %>% distinct(id, .keep_all = TRUE)
df_ratings <- df_ratings %>% distinct(userId, movieId, .keep_all = TRUE)
df_keywords <- df_keywords %>% distinct(id, .keep_all = TRUE)
df_cast <- df_cast %>% distinct(id, .keep_all = TRUE)
df_crew <- df_crew %>% distinct(movie_ID, Job, .keep_all = TRUE)
df_countries <- df_countries %>% distinct(id, .keep_all = TRUE)
df_genres <- df_genres %>% distinct(id, .keep_all = TRUE)
df_languages <- df_languages %>% distinct(id, .keep_all = TRUE)

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

df_keywords <- df_keywords[,1:11]

# Get the id and profit group for each id
glimpse(df_movies)
group_cols <- c(1, 14)
df_groups <- df_movies[, group_cols]
glimpse(df_groups)

df_keywords_grps <- df_keywords %>%
  left_join(df_groups, by = 'id')

df_keywords_tidy <- df_keywords_grps %>%
  pivot_longer(cols = 2:11, names_to = "keyword_num", values_to = "keyword")

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

df_top_keywords <- examine_column(df_keywords_top$keyword)
df_bottom_keywords <- examine_column(df_keywords_bottom$keyword)

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
summary(df_keywords_tidy)

key_cols <- c(1,5)
df_keywords_join <- df_keywords_tidy[,key_cols] %>%
  group_by(id) %>%
  summarize(key_avg = mean(keyword_flg))

# Function to flag movies with keywords from the keyword difference list
flg_keyword_films <- function(x) {
  if (x != 0){
    1
  }else{0}
}

# Apply the function to categorize each row in the keyword dataframe based on the keyword diff list
df_keywords_join$keyword_bin <- factor(unlist(lapply(df_keywords_join$key_avg, flg_keyword_films)))

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
glimpse(df_top_actors)

df_cast_grps <- df_cast %>%
  left_join(df_groups, by = 'id')

df_cast_tidy <- df_cast_grps %>%
  pivot_longer(cols = 2:6, names_to = "actor_num", values_to = "actor")

examine_column(df_cast_tidy$actor)

df_cast_top <- df_cast_tidy %>%
  filter(profit_group == 'Top_20')

df_cast_bottom <- df_cast_tidy %>%
  filter(profit_group == 'Bottom_80')

glimpse(df_cast_top)
head(df_cast_top)
glimpse(df_cast_bottom)

#####  Flag each row that has a name matching the "Top Actors" list
####
#### 


##########################################################################################
# Process the crew data
glimpse(df_crew)
summary(df_crew)

# Read in the IMDB "Top Actor" list
df_top_producers <- read_csv('data/Top_producers.csv')
glimpse(df_top_producers)

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

df_crew_top <- df_crew_tidy %>%
  filter(profit_group == 'Top_20')

df_crew_bottom <- df_crew_tidy %>%
  filter(profit_group == 'Bottom_80')

glimpse(df_crew_top)
glimpse(df_crew_bottom)


##########################################################################################
# Process the countries data
glimpse(df_countries)
summary(df_countries)
# Investigate why there is a max of 5 countries

df_country_grps <- df_countries %>%
  left_join(df_groups, by = 'id')

df_countries_tidy <- df_country_grps %>%
  pivot_longer(cols = 2:26, names_to = "country_num", values_to = "country") %>%
  filter(!is.na(country))

df_countries_num <- df_countries_tidy %>%
  group_by(id) %>%
  summarize(country_count = n())
summary(df_countries_num)
# Join this number to df_movies


df_countries_top <- df_countries_tidy %>%
  filter(profit_group == 'Top_20')

df_countries_bottom <- df_countries_tidy %>%
  filter(profit_group == 'Bottom_80')

glimpse(df_countries_top)
glimpse(df_countries_bottom)






##########################################################################################
# Process the genre data
glimpse(df_genres)
summary(df_genres)

df_genre_grps <- df_genres %>%
  left_join(df_groups, by = 'id')

df_genres_tidy <- df_genre_grps %>%
  pivot_longer(cols = 2:9, names_to = "genre_num", values_to = "genre")

df_genres_top <- df_genres_tidy %>%
  filter(profit_group == 'Top_20')

df_genres_bottom <- df_genres_tidy %>%
  filter(profit_group == 'Bottom_80')

glimpse(df_genres_top)

# Apply the examine column function to produce the lists of top and bottom genres
df_top_genres <- examine_column(df_genres_top$genre)
df_bottom_genres <- examine_column(df_genres_bottom$genre)

sample_rows <- c(1:15)
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
df_genres_join <- df_genres_tidy[,key_cols] %>%
  group_by(id) %>%
  summarize(genre_avg = mean(genre_flg))

# Function to flag movies with genres from the genre difference list
flg_genre_films <- function(x) {
  if (x != 0){
    1
  }else{0}
}

# Apply the function to categorize each row in the genre dataframe based on the genre diff list
df_genres_join$genre_bin <- factor(unlist(lapply(df_genres_join$genre_avg, flg_genre_films)))

df_movies <- df_movies %>%
  left_join(df_genres_join, by = 'id')

glimpse(df_movies)
summary(df_movies)



##########################################################################################
# Process the languages data
glimpse(df_languages)
summary(df_languages)

# Investigate why there is a max of 5 languages

df_language_grps <- df_languages %>%
  left_join(df_groups, by = 'id')

df_languages_tidy <- df_language_grps %>%
  pivot_longer(cols = 2:11, names_to = "language_num", values_to = "language") %>%
  filter(!is.na(language))

df_languages_num <- df_languages_tidy %>%
  group_by(id) %>%
  summarize(language_count = n())
summary(df_languages_num)
# Join this number to df_movies


df_languages_top <- df_languages_tidy %>%
  filter(profit_group == 'Top_20')

df_languages_bottom <- df_languages_tidy %>%
  filter(profit_group == 'Bottom_80')

glimpse(df_languages_top)
glimpse(df_languages_bottom)













# Function to examine other columns to determine if additional cleaning is necessary
examine_column <- function(x) {
  df <- data.frame(unique(x))
  arrange(df, unique.x.)
}

examine_column(df_)




glimpse(df_movies_raw)
summary(df_movies_raw)





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










# Function to examine other columns to determine if additional cleaning is necessary
examine_column <- function(x) {
  df <- data.frame(unique(x))
  arrange(df, unique.x.)
}

examine_column(df_f1$raceName)












df_movies <- df_movies_no_dups %>%
  filter(status == 'Released', budget >= 10000, revenue >= 10000)%>%
  arrange(budget)
table(df_movies$budget)



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
  geom_boxplot(aes(x = profit_group, y = vote_average)) +
  coord_flip()

ggplot(df_movies_w_coll) + 
  geom_boxplot(aes(x = profit_group, y = runtime)) +
  coord_flip()

ggplot(df_movies_w_coll) + 
  geom_boxplot(aes(x = profit_group, y = popularity)) +
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

