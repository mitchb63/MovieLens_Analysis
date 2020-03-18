#############################
# Library Imports and Setup #
#############################

#  Load packages 
library(tidyverse)
library(mi)
library(lubridate)

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
df_movie_fctrs <- c(1,3,8,9,15,18,20,22,25,26,28,31:33)
df_movies_raw[,df_movie_fctrs] <- lapply(df_movies_raw[,df_movie_fctrs] , factor)

glimpse(df_movies_raw)
summary(df_movies_raw)

ggplot(df_movies_raw) +
  geom_boxplot(aes(x = profit_group, y = budget)) +
  coord_flip()

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

rm(df_movies_bins)
rm(df_movies_sums)
rm(df_movies_raw)
options(mc.cores = 2)

bin_imputations <- mi(missing_bins, n.iter = 30, n.chains = 4, max.minutes = 2000)
show(bin_imputations)


