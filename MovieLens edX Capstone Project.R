# MovieLens edX Capstone Project
# Submission by Marina Yamasaki


################################
# Submission Requirements
# Files (3): rmd file, PDF knit from rmd file, R script
# Sections (4)
##### 1. Intro/Overview/Exec Summary: describes dataset, summarizes goals of project and key steps performed
##### 2. Methods/Analysis: Explains process/techniques used (data cleaning, data exploration, visulation, insights gained, model approach)
##### 3. Results: presents modeling results and discusses model performance
##### 4. Conclusion: brief summary of report, limitations, future work
# RMSE < 0.86490 to receive full marks  
################################


# edX provided code (START)
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes
# Install packages and set-up libraries
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages('lubridate', repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages('recosystem', repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages('rmarkdown', repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages('kableExtra', repos = "http://cran.us.r-project.org")
library('tidyverse')
library('caret')
library('data.table')
library('dplyr')
library('stringr')
library('lubridate')
library('rmarkdown')
library('recosystem')
library('kableExtra')


# Download MovieLens 10M dataset:
# "This data set contains 10000054 ratings and 95580 tags applied to 10681 movies by 71567 users of the online movie recommender service MovieLens 
# All users selected had rated at least 20 movies. Unlike previous MovieLens data sets, no demographic information is included."
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")
glimpse(movielens)
colnames(movielens)
length(unique(movielens$movieId))
length(unique(movielens$userId))
unique(edx$rating)

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

##### edX provided code (END)

train_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
temp_train <- edx[-train_index,]
temp_test <- edx[train_index,]

edx_train <- temp_train %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
edx_test <- temp_test %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")


##### 1. Intro/Overview/Exec Summary: describes dataset, summarizes goals of project and key steps performed
# Training dataset dimensions: 
dim(edx)
str(edx) # Check data type of the data fields, table overview
# 10,669 unique movieIds in the training set
length(unique(edx$movieId))
# 69,878 unique userIds in the training set
length(unique(edx$userId))
# distribution of variables
summary(edx) 

##### 2. Methods/Analysis: Explains process/techniques used (data cleaning, data exploration, visulation, insights gained, model approach)
### Clean table:
# Convert timestamp column to date-time
# Add year_rated column representing the year rating was given
# Add year_movie column representing year of movie release

edx_train <- edx_train %>% mutate(timestamp = as_datetime(timestamp),
                              year_rated = year(as_datetime(timestamp)),
                              year_movie = as.numeric(str_extract(title, "(?<=\\()([0-9]{4})(?=\\)$)"))
                               )
edx_test <- edx_test %>% mutate(timestamp = as_datetime(timestamp),
                                year_rated = year(as_datetime(timestamp)),
                                year_movie = as.numeric(str_extract(title, "(?<=\\()([0-9]{4})(?=\\)$)"))
                               )
validation <- validation %>% mutate(timestamp = as_datetime(timestamp),
                          year_rated = year(as_datetime(timestamp)),
                          year_movie = as.numeric(str_extract(title, "(?<=\\()([0-9]{4})(?=\\)$)"))
                               )
# (?<=\\()[^()]*(?=\\))
# edx_use$year_movie[edx_use$title == '1-900 (06) (1994)'] <- 1994

# Rating Date - Release Date
edx_temp <- edx_train %>% mutate(year_diff = year_rated - year_movie)
edx_test_temp <- edx_test %>% mutate(year_diff = year_rated - year_movie)
validation_temp <- validation %>% mutate(year_diff = year_rated - year_movie)

# Parse genre field 
edx_genre <- edx_temp %>% separate_rows(genres, sep ="\\|")
edx_test_genre <- edx_test_temp %>% separate_rows(genres, sep ="\\|")
validation_genre <- validation_temp %>% separate_rows(genres, sep ="\\|")

# 19 genre categories, some films have "(no genres listed)"
unique(edx_genre$genres)
edx_genre$genres <- gsub("-","",edx_genre$genres)
edx_test_genre$genres <- gsub("-","",edx_test_genre$genres)
validation_genre$genres <- gsub("-","",validation_genre$genres)

dim(edx_genre)
genre_ct <- edx_genre %>% group_by(genres) %>% dplyr::summarize(count=n()) %>%
                  arrange(desc(count))
genre_ct %>% ggplot(aes(x=reorder(genres,-count),y=count)) + geom_bar(stat = "identity") +
             theme(axis.text.x = element_text(angle = 60)) + xlab("Genre") + ylab("Count of Associated Movies")
edx_genre %>% group_by(genres) %>% 
                  summarize_at(vars(rating),list(avg_rating=mean)) %>%
                  ggplot(aes(x=reorder(genres,-avg_rating),y=avg_rating)) + geom_bar(stat="identity") +
                  theme(axis.text.x = element_text(angle = 60)) + xlab("Genre") + ylab("Average Rating")
# Build table with binary genre columns
edx_use <- edx_genre %>% mutate(yesno = 1) %>% spread(genres,yesno,fill=0)
edx_test_use <- edx_test_genre %>% mutate(yesno = 1) %>% spread(genres,yesno,fill=0)
validation_use <- validation_genre %>% mutate(yesno = 1) %>% spread(genres,yesno,fill=0)


# Plot average rating by difference between year rated and year of movie release
edx_use$year_diffbins <- cut(edx_use$year_diff,breaks = 20)
edx_use %>% ggplot(aes(x = year_diffbins, y=rating)) +
  stat_summary(fun = "mean", geom = "bar") + theme(axis.text.x = element_text(angle = 60)) +
  xlab("Rating Year - Release Year") + ylab("Average Rating")

# Number of movies rated by year rated
edx_use %>% group_by(year_rated) %>% mutate(count = n()) %>% 
  ggplot(aes(x=year_rated,y=count)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60)) + xlab("Year of Movie Rating") + ylab("Number of Ratings")
# Average rating by year rated
edx_use %>% group_by(year_rated) %>% ggplot(aes(x=year_rated, y=rating)) +
  stat_summary(fun = "mean", geom = "bar") + theme(axis.text.x = element_text(angle = 90)) +
  xlab("Year of Movie Rating") + ylab("Average Rating")
# Number of movies rated by movie release year
edx_use %>% distinct(movieId,year_movie) %>% group_by(year_movie) %>% summarize(count=n()) %>%
  ggplot(aes(x=year_movie,y=count)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) +
  xlab("Movie Release Year") + ylab("Number of Movies")
edx_use %>% group_by(year_movie) %>% mutate(count = n()) %>% 
  ggplot(aes(x=year_movie,y=count)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60)) + xlab("Movie Release Year") + ylab("Number of Ratings")
# Averaged rating by movie release year
edx_use %>% group_by(year_movie) %>% ggplot(aes(x=year_movie, y=rating)) +
  stat_summary(fun = "mean", geom = "bar") +
  theme(axis.text.x = element_text(angle = 60)) + xlab("Movie Release Year") + ylab("Average Rating")

# Number of movies by rating
edx_use %>% group_by(rating) %>% summarize(count = n()) %>% ggplot(aes(x=rating,y=count)) +
  geom_bar(stat = "identity") + xlab("Movie Rating") + ylab("Rating Frequency")

# Sample average rating by user to examine dependency of rating on user
user_temp <- edx_use %>% select(userId, rating) %>% group_by(userId) %>% summarize(avg_rating = mean(rating))
user_temp <- user_temp[sample(nrow(user_temp),200),] %>% arrange(desc(avg_rating)) %>% mutate(row=row_number())
user_temp %>% ggplot(aes(x=row,y=avg_rating)) + geom_bar(stat = "identity") +
  xlab("UserId Sample") + ylab("Average Rating")

user_temp <- edx_use %>% group_by(userId) %>% summarize(movies_rated=n())
user_temp %>% group_by(movies_rated) %>% summarize(freq=n()) %>% arrange(desc(freq))
summary(user_temp)
qplot(user_temp$movies_rated,geom='histogram',binwidth=1, col=I("blue"),
      xlab="Number of Movies Rated",ylab="Number of Users",xlim=c(7,150))

# train_methd < - train(y ~ ., method = "", data = data$train)
# y_hat_methd <- predict(train_methd, data$test, type = "raw")
# confusionMatrix(y_hat_methd, data$test$y)$overall[["Accuracy"]]
# getModelInfo("method") e.g. knn
# modelLookup("method")
# ggplot(train_method,highlight=TRUE)
##### 3. Results: presents modeling results and discusses model performance
RMSE <- function(true_ratings, predicted_ratings){
                 sqrt(mean((true_ratings - predicted_ratings)^2,na.rm=T))
        }

# model 1: Prediction using average rating
mu <- mean(edx_use$rating)
naive_rmse <- RMSE(edx_test_use$rating,mu)
rmse_results <- tibble(method = "Average Rating", RMSE = naive_rmse)
rmse_results

# model 2: Movie Effects model
movie_avgs <- edx_use %>% group_by(movieId) %>% 
                          summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + edx_test_use %>% left_join(movie_avgs,by='movieId') %>%
                     pull(b_i)
movie_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "Movie Effects", RMSE = movie_rmse)

# model 3: User Effects model
user_avgs <- edx_use %>% group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))
predicted_ratings <- mu + edx_test_use %>% left_join(user_avgs,by='userId') %>%
  pull(b_u)
user_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "User Effects", RMSE = user_rmse)

# model 4: Rating Date - Premier Date model
yr_avgs <- edx_use %>% group_by(year_diff) %>% 
  summarize(b_y = mean(rating - mu))
predicted_ratings <- mu + edx_test_use %>% left_join(yr_avgs,by='year_diff') %>%
  pull(b_y)
yr_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "Year Diff Effects", RMSE = yr_rmse)

# model 5: Year of Movie Model
movieyr_avgs <- edx_use %>% group_by(year_movie) %>% 
  summarize(b_my = mean(rating - mu))
predicted_ratings <- mu + edx_test_use %>% left_join(movieyr_avgs,by='year_movie') %>%
  pull(b_my)
movieyr_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "Movie Year Effects", RMSE = movieyr_rmse)

# model 5: Genre model
genre_avgs <- edx_use %>% group_by(Comedy, Romance, Action, Crime, Thriller, Drama,
                                      SciFi, Adventure, Children, Fantasy, War, Animation,
                                      Musical, Western, Mystery, FilmNoir, Horror,
                                      Documentary, IMAX, NoGenre) %>% 
  summarize(b_g = mean(rating - mu))
predicted_ratings <- mu + edx_test_use %>% left_join(genre_avgs,by=c('Comedy','Romance','Action','Crime','Thriller','Drama',
                                      'SciFi','Adventure','Children','Fantasy','War','Animation',
                                      'Musical','Western','Mystery','FilmNoir','Horror',
                                      'Documentary','IMAX','NoGenre')) %>% pull(b_g)
genre_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "Genre Effects", RMSE = genre_rmse)

# model 5: Regularized Genre model
lambdas <- seq(0, 10, 0.25)
just_the_sum <- edx_use %>%
  group_by(Comedy, Romance, Action, Crime, Thriller, Drama,
           SciFi, Adventure, Children, Fantasy, War, Animation,
           Musical, Western, Mystery, FilmNoir, Horror,
           Documentary, IMAX, NoGenre) %>%
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){ 
  predicted_ratings <- edx_test_use %>%
    left_join(just_the_sum, by=c('Comedy','Romance','Action','Crime','Thriller','Drama',
                                 'SciFi','Adventure','Children','Fantasy','War','Animation',
                                 'Musical','Western','Mystery','FilmNoir','Horror',
                                 'Documentary','IMAX','NoGenre')) %>% mutate(b_g = s/(n_i+l)) %>%
    mutate(pred = mu + b_g) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_test_use$rating)) })
qplot(lambdas, rmses) 

lambda <- lambdas[which.min(rmses)]
# 3.75
genre_reg_rmse <- min(rmses)
rmse_results <- rmse_results %>% add_row(method = "Genre Regularized Effects", RMSE = genre_reg_rmse)

# model 6: Movie + User Effects model
predicted_ratings <- edx_test_use %>% left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  mutate(pred = mu + b_i + b_u) %>% pull(pred)
movie_user_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "Movie + User Effects", RMSE = movie_user_rmse)

# model 7: Movie + User + Genre Effects model
predicted_ratings <- edx_test_use %>% left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  left_join(genre_avgs,by=c('Comedy','Romance','Action','Crime','Thriller','Drama',
                            'SciFi','Adventure','Children','Fantasy','War','Animation',
                            'Musical','Western','Mystery','FilmNoir','Horror',
                            'Documentary','IMAX')) %>%
  mutate(pred = mu + b_i + b_u + b_g) %>% pull(pred)
movie_user_genre_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "Movie + User + Genre Effects", RMSE = movie_user_genre_rmse)

# model 8: Regularized Movie Effect model
lambdas <- seq(0, 10, 0.25)
just_the_sum <- edx_use %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){ 
  predicted_ratings <- edx_test_use %>%
  left_join(just_the_sum, by='movieId') %>% mutate(b_i = s/(n_i+l)) %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
return(RMSE(predicted_ratings, edx_test_use$rating)) })
qplot(lambdas, rmses) 

lambda <- lambdas[which.min(rmses)]
# 1.75
movie_reg_avgs <- edx_use %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

predicted_ratings <- edx_test_use %>% left_join(movie_reg_avgs, by='movieId') %>% 
  mutate(pred = mu + b_i ) %>% pull(pred)
movie_reg_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "Movie Regularized Effects", RMSE = movie_reg_rmse)

# model 9: Regularized User Effects Model
lambdas <- seq(0, 10, 0.25)
just_the_sum <- edx_use %>%
  group_by(userId) %>%
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){ 
  predicted_ratings <- edx_test_use %>%
    left_join(just_the_sum, by='userId') %>% mutate(b_u = s/(n_i+l)) %>%
    mutate(pred = mu + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_test_use$rating)) })
qplot(lambdas, rmses) 
lambda <- lambdas[which.min(rmses)]
# 5.5
user_reg_avgs <- edx_use %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu)/(n()+lambda), n_i = n())

predicted_ratings <- edx_test_use %>% left_join(user_reg_avgs, by='userId') %>% 
  mutate(pred = mu + b_u ) %>% pull(pred)
user_reg_rmse <- RMSE(predicted_ratings, edx_test_use$rating)
rmse_results <- rmse_results %>% add_row(method = "User Regularized Effects", RMSE = user_reg_rmse)

# model 10: Regularized Movie + User Effects Model
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_use$rating)
  b_i <- edx_use %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx_use %>%
    left_join(b_i, by="movieId") %>% group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    edx_test_use %>%
    left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)
  return(RMSE(predicted_ratings, edx_test_use$rating)) })
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
# 4.75
reg_movie_user_rmse <- min(rmses)

rmse_results <- rmse_results %>% add_row(method = "Regularized Movie + User Effects", RMSE = reg_movie_user_rmse)

# model 11: Regularized Movie + User + Genre Effects Model
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_use$rating)
  b_i <- edx_use %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx_use %>%
    left_join(b_i, by="movieId") %>% group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_g <- edx_use %>%
    left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% 
                                     group_by(Comedy, Romance, Action, Crime, Thriller, Drama,
                                              SciFi, Adventure, Children, Fantasy, War, Animation,
                                              Musical, Western, Mystery, FilmNoir, Horror,
                                              Documentary, IMAX, NoGenre) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
  predicted_ratings <-
    edx_test_use %>%
    left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% left_join(b_g, by = c('Comedy','Romance','Action','Crime','Thriller','Drama',
                                                                                               'SciFi','Adventure','Children','Fantasy','War','Animation',
                                                                                               'Musical','Western','Mystery','FilmNoir','Horror',
                                                                                               'Documentary','IMAX')) %>% mutate(pred = mu + b_i + b_u + b_g) %>% pull(pred)
  return(RMSE(predicted_ratings, edx_test_use$rating)) })
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
reg_movie_user_genre_rmse <- min(rmses)

rmse_results <- rmse_results %>% add_row(method = "Regularized Movie + User + Genre Effects", RMSE = reg_movie_user_genre_rmse)


lambda
b_i <- edx_use %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+4.5))
b_u <- edx_use %>%
  left_join(b_i, by="movieId") %>% group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+4.5))
b_g <- edx_use %>%
  left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% 
  group_by(Comedy, Romance, Action, Crime, Thriller, Drama,
           SciFi, Adventure, Children, Fantasy, War, Animation,
           Musical, Western, Mystery, FilmNoir, Horror,
           Documentary, IMAX, NoGenre) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+4.5))
predicted_ratings <-
  validation_use %>%
  left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% left_join(b_g, by = c('Comedy','Romance','Action','Crime','Thriller','Drama',
                                                                                             'SciFi','Adventure','Children','Fantasy','War','Animation',
                                                                                             'Musical','Western','Mystery','FilmNoir','Horror',
                                                                                             'Documentary','IMAX')) %>% mutate(pred = mu + b_i + b_u + b_g) %>% pull(pred)
# model 12: Regularized Movie + User Matrix Factorization Effects Model

# Matrix Factorization

mf_edx_use <- edx_use %>% select(movieId, userId, rating)
mf_edx_test_use <- edx_test_use %>% select(movieId, userId, rating)
mf_validation_use <- validation_use %>% select(movieId, userId, rating)

write.table(mf_edx_use,file="train.txt",sep=" ",row.names=FALSE,col.names=FALSE)
write.table(mf_edx_test_use,file="test.txt",sep=" ",row.names=FALSE,col.names=FALSE)
write.table(mf_validation_use,file="validation.txt",sep=" ",row.names=FALSE,col.names=FALSE)
mf_train <- data_file("train.txt")
mf_test <- data_file("test.txt")
mf_validation <- data_file("validation.txt")
r = Reco()

set.seed(1, sample.kind="Rounding")
opts <- r$tune(mf_train, opts = list(dim = c(10,20,30),lrate = c(0.1,0.2),
                                   costp_l1 = 0, costq_l1 = 0,
                                   nthread = 1, niter = 10))
opts
r$train(mf_train,opts=c(opts$min,nthread=1,niter=35))
mf_prediction <- r$predict(mf_test,out_memory())
mf_test_ratings <- read.table("test.txt",header=FALSE,sep="")$V3
mf_rmse <- RMSE(mf_prediction, mf_test_ratings)
rmse_results <- rmse_results %>% add_row(method = "User + Movie Matrix Factorization", RMSE = mf_rmse)

mf_validation_prediction <- r$predict(mf_validation,out_memory())
mf_validaton_ratings <- read.table("validation.txt",header=FALSE,sep="")$V3
final_rmse <- RMSE(mf_validation_prediction,mf_validaton_ratings)
rmse_results <- rmse_results %>% add_row(method = "Final User + Movie Matrix Factorization", RMSE = final_rmse)
rmse_results

##### 4. Conclusion: brief summary of report, limitations, future work
# Building a train, test, and holdout set -- for tuning lambda

edx_use %>% group_by(year_movie) %>% summarize(count=n()) %>% 
  ggplot(aes(x=year_movie,y=count)) + geom_bar(stat="identity")
