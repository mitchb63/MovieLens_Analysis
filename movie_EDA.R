#############################
# Library Imports and Setup #
#############################

#  Load packages 
library(tidyverse)
library(mi)
library(lubridate)
library(ggcorrplot)
library(ggthemes)

options(scipen = 999, digits = 6)

# Set Working Dir
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Project")
###################################################################

# read in the dataset and view
df_movies_raw <- read_csv("data/processed/df_movies.csv")
# actor_list <- read_csv("data/processed/actor_list.csv")
# producer_list <- read_csv("data/processed/producer_list.csv")
# country_diff_list <- read_csv("data/processed/country_diff_list.csv")
# genre_diff_list <- read_csv("data/processed/genre_diff_list.csv")
# keyword_diff_list <- read_csv("data/processed/keyword_diff_list.csv")
# language_diff_list <- read_csv("data/processed/language_diff_list.csv")

glimpse(df_movies_raw)
summary(df_movies_raw)

#  Convert date fields
df_movies_raw$release_month <- month(df_movies_raw$release_date, label = TRUE)

# Convert selected columns to integers
df_movie_ints <- c(2,6,7,11,13,17,19,21,23,24,27,29,30)
df_movies_raw[,df_movie_ints] <- lapply(df_movies_raw[,df_movie_ints] , as.integer)

# Convert selected columns to factors
df_movie_fctrs <- c(3,8,9,15,18,20,22,25,26,28,31:33)
df_movies_raw[,df_movie_fctrs] <- lapply(df_movies_raw[,df_movie_fctrs] , factor)

glimpse(df_movies_raw)
summary(df_movies_raw)

# Filter the data to remove movies with no revenue information
df_movies_filtered <- df_movies_raw %>% 
  filter(! is.na(revenue))

######################################################################################
# Examine 'Production factors' for determining success
# Budget, Revenue and Profit are interesting 
ggplot(df_movies_filtered) +
  geom_histogram(aes(x = budget), bins = 50) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_boxplot(aes(x = profit_group, y = budget)) +
  coord_flip() +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_histogram(aes(x = revenue), bins = 50) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_boxplot(aes(x = profit_group, y = revenue)) +
  coord_flip() +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_histogram(aes(x = profit), bins = 50) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_boxplot(aes(x = profit_group, y = profit)) +
  coord_flip() +
  theme_tufte()

# Runtime does not appear to be a significant factor
ggplot(df_movies_filtered) +
  geom_histogram(aes(x = runtime), bins = 50) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_boxplot(aes(x = profit_group, y = runtime)) +
  coord_flip() +
  theme_tufte()

# the number of languages a movie was filmed in does not appear to be a significant factor
ggplot(df_movies_filtered) +
  geom_bar(aes(x = language_count)) +
  theme_tufte() 

ggplot(df_movies_filtered) +
  geom_bar(aes(x = language_count, fill = profit_group), position = 'dodge') +
  theme_tufte()

# the original language a movie was filmed in does not appear to be a significant factor
ggplot(df_movies_filtered) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(original_language)))) +
  coord_flip() +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(original_language)), fill = profit_group), position = 'dodge') +
  coord_flip() +
  theme_tufte()

# the month a movie was released does not appear to be a significant factor
ggplot(df_movies_filtered) +
  geom_bar(aes(x = release_month)) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = release_month, fill = profit_group), position = 'dodge') +
  theme_tufte()

# the 'quality' of actors does not appear to be a significant factor - see the actor_list
ggplot(df_movies_filtered) +
  geom_bar(aes(x = actor_bin)) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = actor_bin, fill = profit_group), position = 'dodge') +
  theme_tufte()

# the 'quality' of the producer and/or director may have an influence - see the producer_list
ggplot(df_movies_filtered) +
  geom_bar(aes(x = crew_bin)) +
  theme_tufte() 

ggplot(df_movies_filtered) +
  geom_bar(aes(x = crew_bin, fill = profit_group), position = 'fill') +
  theme_tufte()

# the number of countries a movie was filmed in does not appear to be a significant factor
ggplot(df_movies_filtered) +
  geom_bar(aes(x = country_count)) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = country_count, fill = profit_group), position = 'dodge') +
  theme_tufte()

# The specific countries a movie was filmed in does not appear to be significant - see the country_diff_list
ggplot(df_movies_filtered) +
  geom_bar(aes(x = country_bin)) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = country_bin, fill = profit_group), position = 'dodge') +
  theme_tufte()

# The specific language a movie was filmed in does not appear to be significant - see the language_diff_list
ggplot(df_movies_filtered) +
  geom_bar(aes(x = language_bin)) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = language_bin, fill = profit_group), position = 'dodge') +
  theme_tufte()

######################################################################################
# Examine 'Story factors' for determining success
# the genre of a movie does not appear to be a significant factor
ggplot(df_movies_filtered) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(genre)))) +
  coord_flip() +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(genre)), fill = profit_group), position = 'dodge') +
  coord_flip() +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = genre_bin)) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = genre_bin, fill = profit_group), position = 'dodge') +
  theme_tufte()

# Whether a movie is part of a collection does appear to be a significant factor
ggplot(df_movies_filtered) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(collection)))) +
  coord_flip() +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(collection)), fill = profit_group), position = 'dodge') +
  coord_flip() +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = collection_bin)) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = collection_bin, fill = profit_group), position = 'fill') +
  theme_tufte()

# Whether a movie has distinguishing subject keywords does appear to be a significant factor - see keyword_diff_list
ggplot(df_movies_filtered) +
  geom_bar(aes(x = keyword_bin)) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_bar(aes(x = keyword_bin, fill = profit_group), position = 'fill') +
  theme_tufte()

######################################################################################
# Examine 'Opinion factors' for determining success
# Whether a movie has a high popularity rating may have a slight correlation
ggplot(df_movies_filtered) +
  geom_histogram(aes(x = popularity), bins = 50) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_tufteboxplot(aes(x = profit_group, y = popularity)) +
  coord_flip() +
  theme_tufte()

# Whether a movie has a high 'popularity rating'vote average' may be an indicator
ggplot(df_movies_filtered) +
  geom_histogram(aes(x = vote_average), bins = 50) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_boxplot(aes(x = profit_group, y = vote_average)) +
  coord_flip() +
  theme_tufte()

# Whether a movie has a high 'average rating' does not appear to be an indicator
ggplot(df_movies_filtered) +
  geom_histogram(aes(x = avg_rating), bins = 50) +
  theme_tufte()

ggplot(df_movies_filtered) +
  geom_boxplot(aes(x = profit_group, y = avg_rating)) +
  coord_flip() +
  theme_tufte()

##############################################################################
# Correlation analysis
#Create a plot of correlation using previous example
glimpse(df_movies_filtered)
corr_cols <- c(15,2,20,22,28,33,18,4,10,16)
df_movies_corr <- df_movies_filtered[,corr_cols]
glimpse(df_movies_corr)

# Function to recode the profit group as binary
recode_groups <- function (x){
  if (is.na(x)){
    NA}
  else if (x == 'Top_20'){
    1}
  else{
    0}
}

# Apply the function to clean the zeros
df_movies_corr$profit_group <- unlist(lapply(as.character(df_movies_corr$profit_group), recode_groups))

# Convert selected columns to integers
df_corr_nums <- c(3:7)
df_movies_corr[,df_corr_nums] <- lapply(df_movies_corr[,df_corr_nums] , as.numeric)

df_movies_std <- scale(df_movies_corr)

glimpse(df_movies_std)
summary(df_movies_std)

corr <- round(cor(df_movies_std, use="pairwise.complete.obs"), 4)
head(corr)

p.mat <- round(cor_pmat(df_movies_corr), 6)
head(p.mat)

ggcorrplot(corr, method = "circle", p.mat = p.mat, outline.col = "white", 
           ggtheme = ggplot2::theme_minimal, type = "lower", insig = "blank", colors = c("#11015E", "white", "#890501"))



# Pre-processing for multiple imputation with mi.  The actual imputation failed due to excessive runtime.
remove_sums <- c(5,8,9,13:15,17,19,21,24,27,30,32)
df_movies_bins <- df_movies_raw[,-remove_sums]
glimpse(df_movies_bins)
summary(df_movies_bins)

remove_bins <- c(5,8,9,13:15,18,20,22,25,28,31,32)
df_movies_sums <- df_movies_raw[,-remove_bins]
glimpse(df_movies_sums)

missing_bins <- missing_data.frame(as.data.frame(df_movies_bins))
show(missing_bins)
missing_bins <- change(missing_bins, y = c('country_count', 'language_count'), what = 'type',to = c('continuous', 'continuous'))
show(missing_bins)

missing_sums <- missing_data.frame(as.data.frame(df_movies_sums))
show(missing_sums)
missing_sums <- change(missing_sums, y = c('key_sum', 'actor_sum', 'crew_sum', 'country_count', 'country_sum', 'genre_sum', 'language_count', 'language_sum'), 
                       what = 'type',to = c('continuous', 'continuous', 'continuous', 'continuous', 'continuous', 'continuous', 'continuous', 'continuous'))
show(missing_sums)

summary(missing_bins)
image(missing_bins)
hist(missing_bins)

summary(missing_sums)
image(missing_sums)
hist(missing_sums)



