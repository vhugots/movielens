###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes
## @knitr test
if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

proj_path <- "~/projects/movielens"

if (!file.exists(paste(proj_path,"data/data.rda", sep = "/"))) {
  tmpvar <-
    c("ratings",
      "movies",
      "test_index",
      "temp",
      "movielens",
      "removed",
      "tmpvar")
  if (!file.exists(paste(proj_path,"ml-10M100K/ratings.dat", sep = "/"))) {
    dl <- tempfile()
    download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip",
                  dl)
    
    ratings <-
      read.table(
        text = gsub("::", "\t", readLines(unzip(
          dl, "ml-10M100K/ratings.dat"
        ))),
        col.names = c("userId", "movieId", "rating", "timestamp")
      )
    movies <-
      str_split_fixed(readLines(unzip(dl, paste(proj_path, "ml-10M100K/movies.dat", sep = "/"))), "\\::", 3)
    tmpvar <- c("dl", tmpvar)
  } else {
    ratings <-
      read.table(
        text = gsub("::", "\t", readLines(paste(proj_path, "ml-10M100K/ratings.dat", sep = "/"))),
        col.names = c("userId", "movieId", "rating", "timestamp")
      )
    movies <-
      str_split_fixed(readLines(paste(proj_path, "ml-10M100K/movies.dat", sep = "/")), "\\::", 3)
  }
  
  colnames(movies) <- c("movieId", "title", "genres")
  movies <-
    as.data.frame(movies) %>% mutate(
      movieId = as.numeric(levels(movieId))[movieId],
      title = as.character(title),
      genres = as.character(genres)
    )
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  
  set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
  test_index <-
    createDataPartition(
      y = movielens$rating,
      times = 1,
      p = 0.1,
      list = FALSE
    )
  edx <- movielens[-test_index, ]
  temp <- movielens[test_index, ]
  
  # Make sure userId and movieId in validation set are also in edx set
  
  validation <- temp %>%
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  
  removed <- anti_join(temp, validation)
  
  edx <- rbind(edx, removed)
  
  rm(list = tmpvar)
  
  save(edx, validation, file = paste(proj_path, "data/data.rda", sep = "/"))
} else{
  load(paste(proj_path, "data/data.rda", sep = "/"))
}

##Create the function for RMSE calculation
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##naive_rmse calculation using the average
mu_hat <- mean(edx$rating)

naive_rmse <- RMSE(validation$rating, mu_hat)

predictions <- rep(mu_hat, nrow(validation))
RMSE(validation$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

##First model removing Movie Effect
mu <- mean(edx$rating)
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

##The first model but with regularization optimizing lambda
lambdas <- seq(0, 10, 0.25)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

movie_reg_avgs <- validation %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambdas[which.min(rmses)]), n_i = n())

lambdaM <- lambdas[which.min(rmses)]

predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_1r_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_1r_rmse ))

##Constructing the second model adjusting Movie and User rating
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

##The second model using regularization and optimizing lambda
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

lambdaMU <- lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
