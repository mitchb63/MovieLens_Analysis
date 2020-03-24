#############################
# Library Imports and Setup #
#############################

#  Load packages 
library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(ggthemes)

options(scipen = 999, digits = 6)

# Set Working Dir
setwd("C:/Users/mitch/OneDrive/R_Projects/Git/MovieLens_Project")
###################################################################

# read in the dataset and view
df_imp_1 <- read_csv("data/processed/df_imp1.csv")
df_imp_2 <- read_csv("data/processed/df_imp2.csv")
df_imp_3 <- read_csv("data/processed/df_imp3.csv")
df_imp_4 <- read_csv("data/processed/df_imp4.csv")
df_imp_5 <- read_csv("data/processed/df_imp5.csv")

glimpse(df_imp_1)
glimpse(df_imp_2)
glimpse(df_imp_3)
glimpse(df_imp_4)
glimpse(df_imp_5)

summary(df_imp_1)
summary(df_imp_2)
summary(df_imp_3)
summary(df_imp_4)
summary(df_imp_5)

# Convert selected columns to factors
df_imp_fctrs <- c(1,3,9,11:13,15:17,19,20)
df_imp_1[,df_imp_fctrs] <- lapply(df_imp_1[,df_imp_fctrs] , factor)
df_imp_2[,df_imp_fctrs] <- lapply(df_imp_2[,df_imp_fctrs] , factor)
df_imp_3[,df_imp_fctrs] <- lapply(df_imp_3[,df_imp_fctrs] , factor)
df_imp_4[,df_imp_fctrs] <- lapply(df_imp_4[,df_imp_fctrs] , factor)
df_imp_5[,df_imp_fctrs] <- lapply(df_imp_5[,df_imp_fctrs] , factor)

# Create new profit and profit margin variables
df_imp_1 <- df_imp_1 %>%
  mutate(profit = revenue - budget, profit_margin = profit/revenue)
df_imp_2 <- df_imp_2 %>%
  mutate(profit = revenue - budget, profit_margin = profit/revenue)
df_imp_3 <- df_imp_3 %>%
  mutate(profit = revenue - budget, profit_margin = profit/revenue)
df_imp_4 <- df_imp_4 %>%
  mutate(profit = revenue - budget, profit_margin = profit/revenue)
df_imp_5 <- df_imp_5 %>%
  mutate(profit = revenue - budget, profit_margin = profit/revenue)

movie_div_1 <-quantile(df_imp_1$profit_margin, probs = 0.80)
movie_div_2 <-quantile(df_imp_2$profit_margin, probs = 0.80)
movie_div_3 <-quantile(df_imp_3$profit_margin, probs = 0.80)
movie_div_4 <-quantile(df_imp_4$profit_margin, probs = 0.80)
movie_div_5 <-quantile(df_imp_5$profit_margin, probs = 0.80)

# Function to assign each movie to one of two categories
assign_movie <- function(x,y) {
  if (is.na(x)){
    'NA'}
  else if (x >= y){
    'Top_20'}
  else{
    'Bottom_80'}
}

# Apply the function to categorize each movie based on profit margin
df_imp_1$profit_group <- as.factor(unlist(lapply(df_imp_1$profit_margin, assign_movie, y = movie_div_1)))
df_imp_2$profit_group <- as.factor(unlist(lapply(df_imp_2$profit_margin, assign_movie, y = movie_div_2)))
df_imp_3$profit_group <- as.factor(unlist(lapply(df_imp_3$profit_margin, assign_movie, y = movie_div_3)))
df_imp_4$profit_group <- as.factor(unlist(lapply(df_imp_4$profit_margin, assign_movie, y = movie_div_4)))
df_imp_5$profit_group <- as.factor(unlist(lapply(df_imp_5$profit_margin, assign_movie, y = movie_div_5)))

##############################################################################
# Correlation analysis
#Create a plot of correlation using previous example
glimpse(df_imp_1)
corr_cols <- c(23,22,2,4:8,10:15,17:21)
df_imp1_corr <- df_imp_1[,corr_cols]
glimpse(df_imp1_corr)

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
df_imp1_corr$profit_group <- unlist(lapply(as.character(df_imp1_corr$profit_group), recode_groups))

# Convert selected columns to integers
df_corr_nums <- c(10:12,14,15,17,18)
df_imp1_corr[,df_corr_nums] <- lapply(df_imp1_corr[,df_corr_nums] , as.numeric)

df_imp1_std <- scale(df_imp1_corr)

glimpse(df_imp1_std)
summary(df_imp1_std)

corr <- round(cor(df_imp1_std, use="pairwise.complete.obs"), 6)
corr

p.mat <- round(cor_pmat(df_imp1_std), 6)
p.mat

ggcorrplot(corr, method = "circle", p.mat = p.mat, outline.col = "white", 
           ggtheme = ggplot2::theme_minimal, type = "lower", insig = "blank", colors = c("#11015E", "white", "#890501"))

######################################################################################
# Examine 'Production factors' for determining success
# Budget, Revenue and Profit are interesting 
plot_hist <-  function(df, col, bins){
ggplot(df) +
  geom_histogram(aes(x = col), bins = bins) +
  theme_tufte()
}

plot_boxplot <- function(df, profit_group, col){
ggplot(df) +
  geom_boxplot(aes(x = profit_group, y = col)) +
  coord_flip() +
  theme_tufte()
}


plot_hist(df_imp_1, df_imp_1$budget, 50)
plot_boxplot(df_imp_1, df_imp_1$profit_group, df_imp_1$budget)

plot_hist(df_imp_1, df_imp_1$revenue, 50)
plot_boxplot(df_imp_1, df_imp_1$profit_group, df_imp_1$revenue)

plot_hist(df_imp_1, df_imp_1$profit, 50)
plot_boxplot(df_imp_1, df_imp_1$profit_group, df_imp_1$profit)

# Runtime does not appear to be a significant factor
plot_hist(df_imp_1, df_imp_1$runtime, 50)
plot_boxplot(df_imp_1, df_imp_1$profit_group, df_imp_1$runtime)

# the number of languages a movie was filmed in does not appear to be a significant factor
plot_hist(df_imp_1, df_imp_1$language_count, 50)
plot_boxplot(df_imp_1, df_imp_1$profit_group, df_imp_1$language_count)


# the original language a movie was filmed in does not appear to be a significant factor
ggplot(df_imp_1) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(original_language)))) +
  coord_flip() +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(original_language)), fill = profit_group), position = 'dodge') +
  coord_flip() +
  theme_tufte()

# the month a movie was released does not appear to be a significant factor
ggplot(df_imp_1) +
  geom_bar(aes(x = release_month)) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = release_month, fill = profit_group), position = 'dodge') +
  theme_tufte()

# the 'quality' of actors does not appear to be a significant factor - see the actor_list
ggplot(df_imp_1) +
  geom_bar(aes(x = actor_bin)) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = actor_bin, fill = profit_group), position = 'dodge') +
  theme_tufte()

# the 'quality' of the producer and/or director may have an influence - see the producer_list
ggplot(df_imp_1) +
  geom_bar(aes(x = crew_bin)) +
  theme_tufte() 

ggplot(df_imp_1) +
  geom_bar(aes(x = crew_bin, fill = profit_group), position = 'fill') +
  theme_tufte()

# the number of countries a movie was filmed in does not appear to be a significant factor
ggplot(df_imp_1) +
  geom_bar(aes(x = country_count)) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = country_count, fill = profit_group), position = 'dodge') +
  theme_tufte()

# The specific countries a movie was filmed in does not appear to be significant - see the country_diff_list
ggplot(df_imp_1) +
  geom_bar(aes(x = country_bin)) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = country_bin, fill = profit_group), position = 'dodge') +
  theme_tufte()

# The specific language a movie was filmed in does not appear to be significant - see the language_diff_list
ggplot(df_imp_1) +
  geom_bar(aes(x = language_bin)) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = language_bin, fill = profit_group), position = 'dodge') +
  theme_tufte()

######################################################################################
# Examine 'Story factors' for determining success
# the genre of a movie does not appear to be a significant factor
ggplot(df_imp_1) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(genre)))) +
  coord_flip() +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = forcats::fct_rev(fct_infreq(genre)), fill = profit_group), position = 'dodge') +
  coord_flip() +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = genre_bin)) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = genre_bin, fill = profit_group), position = 'dodge') +
  theme_tufte()

# Whether a movie is part of a collection does appear to be a significant factor
ggplot(df_imp_1) +
  geom_bar(aes(x = collection_bin)) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = collection_bin, fill = profit_group), position = 'fill') +
  theme_tufte()

# Whether a movie has distinguishing subject keywords does appear to be a significant factor - see keyword_diff_list
ggplot(df_imp_1) +
  geom_bar(aes(x = keyword_bin)) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_bar(aes(x = keyword_bin, fill = profit_group), position = 'fill') +
  theme_tufte()

######################################################################################
# Examine 'Opinion factors' for determining success
# Whether a movie has a high popularity rating may have a slight correlation
ggplot(df_imp_1) +
  geom_histogram(aes(x = popularity), bins = 50) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_boxplot(aes(x = profit_group, y = popularity)) +
  coord_flip() +
  theme_tufte()

# Whether a movie has a high 'vote average' may be an indicator
ggplot(df_imp_1) +
  geom_histogram(aes(x = vote_average), bins = 50) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_boxplot(aes(x = profit_group, y = vote_average)) +
  coord_flip() +
  theme_tufte()

# Whether a movie has a high 'average rating' does not appear to be an indicator
ggplot(df_imp_1) +
  geom_histogram(aes(x = avg_rating), bins = 50) +
  theme_tufte()

ggplot(df_imp_1) +
  geom_boxplot(aes(x = profit_group, y = avg_rating)) +
  coord_flip() +
  theme_tufte()

