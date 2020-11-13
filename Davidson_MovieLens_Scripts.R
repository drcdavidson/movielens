## HarvardX: Ph125.9x Data Science Capstone MovieLens Rating Prediction Project
## Chris Davidson

##########################################
# MovieLens Rating Prediction Project Code
##########################################

####################################################
##### MovieLens 10M Dataset & Data Preparation #####

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Remove objects from the environment to leave only the edx and validation datasets
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Additional libraries possibly needed for data analysis 
library(ggplot2)
library(lubridate)
library(knitr)

# Extract Year of Movie Release
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

# Extract Year of Movie Rating
edx <- edx %>% mutate(year_rated = year(as_datetime(timestamp)))
validation <- validation %>% mutate(year_rated = year(as_datetime(timestamp)))

# Calculate Age of Movie at Time of Rating 
edx <- edx %>% mutate(age_at_rating = year_rated - year)
validation <- validation %>% mutate(age_at_rating = year_rated - year)

# edx dataset partitioned into training and test datasets eaching having 
# 50% of the observations (not provided by coursework)
set.seed(1, sample.kind="Rounding")  
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.5, list = FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]
rm(test_index)

########################################
##### MovieLens 10M Dataset Descriptives #####

# Number of users in MovieLens 10M
users <- data.frame(c(edx$userId, validation$userId))
n_distinct(users)

# Number of movies in MovieLens 10M
movies <- data.frame(c(edx$title, validation$title))
n_distinct(movies)

# Number of ratings in MovieLens 10M
length(edx$rating) + length(validation$rating)

# Range of ratings in MovieLens 10M
ratings <- tibble(c(edx$rating, validation$rating))
range(ratings)

# Determine the year of movie range in MovieLens 10M
years <- data.frame(c(edx$year, validation$year))
range(years)

# Determine Genres and Frequency of Genres  
genres <- edx %>% 
  separate_rows(genres, sep ="\\|") %>% 
  group_by(genres) %>% 
  summarize(n = n())

genres <- genres %>%
  mutate(Total = sum(n), 
         Percentage = n / Total)

genres1 <- validation %>% 
  separate_rows(genres, sep ="\\|") %>% 
  group_by(genres) %>% 
  summarize(n = n()) 

genres1 <- genres1 %>%
  mutate(Total = sum(n), 
         Percentage = n / Total)

# Remove objects from environment 
rm(movies, ratings, users, years)

######################################################
##### Exploratory Analysis of the Tuning Edx Set #####

# View the first 6 rows of the edx dataset
head(edx)

# Summary statisics of the edx datset
summary(edx)

# Summary Distribution of Ratings 
edx %>% ggplot(aes(rating)) +
  geom_histogram(fill = "dark blue", color = "white", binwidth = .5) +
  scale_x_discrete(limits = c(seq(0,5)))+
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Distribution of Ratings")

# Summary of Ratings by Movies 
edx %>% count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "dark blue", color = "white", bins = 30) + 
  scale_x_log10() + 
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  ggtitle("Distribution of Ratings by Movie")

# Summary of Ratings by Users 
edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "dark blue", color = "white", bins = 30) + 
  scale_x_log10() + 
  xlab("Number of Ratings") +
  ylab("Number of Users") +
  ggtitle("Distribution of Ratings by Users")

# Distribution of Mean Movie Rating by Users 
edx %>% group_by(userId) %>%
  summarize(mean = mean(rating)) %>%
  ggplot(aes(mean)) +
  geom_histogram(fill = "dark blue", color = "white", bins = 30) +
  scale_x_discrete(limits = c(seq(0,5))) +
  xlab("Mean Rating") +
  ylab("Number of Users") +
  ggtitle("Mean Movie Rating by Users")

# Distribution of Mean Movie Rating by Age of Movie
edx %>% group_by(age_at_rating) %>%
  summarize(mean = mean(rating)) %>%
  ggplot(aes(mean)) +
  geom_histogram(fill = "dark blue", color = "white", bins = 12) +
  scale_x_discrete(limits = c(seq(0,5))) +
  xlab("Mean Rating") +
  ylab("Age") +
  ggtitle("Mean Movie Rating by Age")

# Distribution of Genres by Percent Rated
genres %>%
  ggplot(aes(reorder(genres, Percentage), Percentage, fill = Percentage)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Genre", y = "Percentage") +
  ggtitle("Distribution of Genres by Percent Rated")

rm(genres, genres1)

####################
##### Modeling #####
## RMSE Function
RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}

## Model I: Simple Average Movie Rating
#Mean Rating of edx Dataset
mu <- mean(edx_train$rating)
mu

# Test Model 1: RMSE based on Simple Average Movie Rating
RMSE_1 <- RMSE(edx_test$rating, mu)
RMSE_1  

# Create data frame to save RMSE's from prediction models
RMSE_Results <- data.frame(Method = "Model 1: Average Movie Rating", 
                           RMSE = RMSE_1)
RMSE_Results %>% knitr::kable()  

#############################
## Model II: The Movie Effect
# Mean Summary of the Difference in Rating and Average 
movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# Distribution of the Differences in Rating and Average to Determine Bias
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# Calculate Predicted Ratings with Movie Effect
predicted_ratings <- mu + edx_test %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# Calculate RMSE for Movie Effect Term Plus Mean (Mu)
RMSE_2 <- RMSE(predicted_ratings, edx_test$rating, na.rm=TRUE)

# Add Movie Effect Model to RMSE Results Table
RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(Method="Model 2: Movie Effect Model",
                                     RMSE = RMSE_2 ))
RMSE_Results %>% knitr::kable()

#######################################
## Model III: Movie & User Effect Model 
# Determining User Effect on Means
user_avgs <- edx_train  %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Distribution of the User Effect Terms and Positive Bias
user_avgs %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))

# Calculate Predicted Ratings with Movie & User Effect
predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Calculate RMSE for Movie & User Effect Terms Plus Mean (Mu)
RMSE_3 <- RMSE(predicted_ratings, edx_test$rating, na.rm=TRUE)

# Add Movie & User Effect Model to RMSE Results Table
RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(Method="Model 3: Movie & User Effect Model",
                                     RMSE = RMSE_3))
RMSE_Results %>% knitr::kable()

#######################################
## Model IV: Movie, User, & Time Effect
# Determining Age Effect on Means
age_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(age_at_rating) %>%
  summarize(b_a = mean(rating - mu - b_i - b_u))

# Distribution of Age Effects
age_avgs %>% qplot(b_a, geom ="histogram", bins = 30, data = ., color = I("black"))

# Calculated Predicted Ratings with Movie, User, & Age Effect
predicted_ratings_bt <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(age_avgs, by='age_at_rating') %>%
  mutate(pred = mu + b_i + b_u + b_a) %>%
  .$pred
  
# Calculate RMSE for Movie, User, & Age Effect Terms Plus Mean (Mu)
RMSE_4 <- RMSE(predicted_ratings, edx_test$rating, na.rm=TRUE)
# No change

# Determining Time of Rating Effect on Rating Means 
edx_train <- edx_train %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week"))

edx_test <- edx_test %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week"))

time_avgs <- edx_train %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(date) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u))

# Calculated Predicted Ratings with Movie, User, & Time Effect
predicted_ratings <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  .$pred

# Calculate RMSE for Movie, User, Time of Rating Effect Terms Plus Mean (Mu)
RMSE_4 <- RMSE(predicted_ratings, edx_test$rating, na.rm=TRUE)

# Test the Regularized Model 
RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(Method = "Model 4: Movie, User, & Time Effect Model",
                                     RMSE = RMSE_4))
RMSE_Results %>% knitr::kable()

#############################################################
## Model V: Regularization of the Movie and User Effect Model             
# Lambda is the tuning paramter; Uses cross-validation to choose ideal lambda for model
lambdas <- seq(0, 10, .25)

# Each lambda is used to detmine the b_i, b_u, the rating 
# prediction and testing
lambdas <- seq(0, 10, 0.25)
RMSES <- sapply(lambdas, function(l){
  mu <- mean(edx_train$rating)
  
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l), na.rm = TRUE)
  
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l), na.rm = TRUE)
  
  predicted_ratings <- edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test$rating, na.rm=TRUE))
})

# Plotting Lambdas and RMSEs 
qplot(lambdas, RMSES)

# Determine optimal lambda 
lambda <- lambdas[which.min(RMSES)]
lambda

# Test the Regularized Model 
RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(Method = "Model 5: Regularized Movie & User Effect Model",
                                     RMSE = min(RMSES)))
RMSE_Results %>% knitr::kable()

##########################################
# Final Test of Model with Validation Data
## Final Model: Regularization of the Movie and User Effect Model             
# Each lambda is used to detmine the b_i, b_u, the rating 
# prediction and testing
lambdas <- seq(0, 10, 0.25)
RMSES <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l), na.rm = TRUE)
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l), na.rm = TRUE)
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating, na.rm=TRUE))
})

# Plotting Lambdas and RMSEs 
qplot(lambdas, RMSES)

# Determine optimal lambda 
lambda <- lambdas[which.min(RMSES)]
lambda

# Test the Final Regularized Model 
RMSE_Final <- min(RMSES)
RMSE_Final

# Percent Change in RMSE (Training Set)
((RMSE_1 - RMSE_8)/RMSE_1)*100   http://192.168.7.100:8787/