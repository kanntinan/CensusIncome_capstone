
# Introduction ####


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
#set.seed(1)
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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
rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Exploring Dataset ####
# Create train and validation set from edx capstone project code 
# training set contain 9,000,055 row and 999,999 for validation set (Each row represents a rating given by one user to one movie)
# and 6 column with userId, movieId, rating, timestamp, title and genres.
glimpse(edx)
glimpse(validation)
summary(edx)

# We found 69,878  unique users that provided ratings and 10,677 unique movies were rated in training set.
edx %>% summarize(n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId))

# Explore most popular genres
#separate genres
top_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#combination genres
top_comgenres <- edx %>% group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
# Explore effect of genres to average rating 
# The plot shows evidence of a genre effect
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Transform timestamp to date and time, get movie year, difference movie year and rating year. 
library(lubridate)
# movieyear = as.numeric(gsub("(?<=\\()[0-9]*(?=\\))(*SKIP)(*F)|.", "", title, perl=T))
edx <- edx %>% mutate(date = as_datetime(timestamp), years = year(date),
                      movieyear = as.integer(substr(title,nchar(title)-4,nchar(title)-1)), 
                      yeardiff = years - movieyear)
validation <- validation %>% mutate(date = as_datetime(timestamp), years = year(date),
                             movieyear = as.integer(substr(title,nchar(title)-4,nchar(title)-1)), 
                             yeardiff = years - movieyear)

# Explore rating value 
sort(table(edx$rating))
edx %>% ggplot(aes(x = factor(rating))) + geom_bar()
# some movies are rated higher than others
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# some users are more active than others at rating movies
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# Explore average rating in each movie year and year differance. 
edx %>% group_by(yeardiff) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(yeardiff, rating)) +
  geom_point() +
  geom_smooth()

edx %>% group_by(movieyear) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(movieyear, rating)) +
  geom_point() +
  geom_smooth()
 
# We found some movies are just generally rated higher than others as well as some user are rated lot of movie.
# year and genres might be effect to rating 
# Then we use these 4 features to build recommendation model. 

# Analysis ####

#  Define function for calculate RMSE 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2,na.rm=T))
}

# Start with simplest model that predict same rating for all movies regardless of user.
mu_hat <- mean(edx$rating)
naive_rmse <- RMSE(edx$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# Add movie effect to model
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>% .$b_i
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

# Add user effect model                         
user_avgs <- edx %>% 
left_join(movie_avgs, by='movieId') %>%
group_by(userId) %>%
summarize(b_u = mean(rating - mu - b_i))
                         
predicted_ratings <- validation %>% 
left_join(movie_avgs, by='movieId') %>%
left_join(user_avgs, by='userId') %>%
mutate(pred = mu + b_i + b_u) %>%
.$pred
                          
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
data_frame(method="Movie + User Effects Model",  
RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# Explore result 
# We found the top 10 worst and best movies based on estimate b were rated by few users
# These are noisy estimates that we should not trust 
# Then, we use regularization to penalize large estimates that are formed using small sample sizes.
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 

edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 

# Add year difference and genres effects 
# regularization all features.
# find lambda value 
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_g <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
  b_yd <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    group_by(yeardiff) %>%
    summarize(b_yd = sum(rating - b_i - b_u - b_g - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_yd, by = "yeardiff") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_yd) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]
lambda # choose lambda = 5.5

# Build final model 
movie_avgs_reg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
user_avgs_reg <- edx %>% 
  left_join(movie_avgs_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
genres_avgs_reg <- edx %>%
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+lambda))
year_avgs_reg <- edx %>%
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  left_join(genres_avgs_reg, by='genres') %>%
  group_by(yeardiff) %>%
  summarize(b_yd = sum(rating - b_i - b_u - b_g - mu)/(n()+lambda))

predicted_ratings <- validation %>% 
  left_join(movie_avgs_reg, by = "movieId") %>%
  left_join(user_avgs_reg, by = "userId") %>%
  left_join(genres_avgs_reg, by = "genres") %>%
  left_join(year_avgs_reg, by = "yeardiff") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_yd) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="All feature + Regularization Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()



