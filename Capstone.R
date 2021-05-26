###
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(magrittr) 
library(png) 
# 5 significant digits
options(digits = 5)

######################## load and partition #######################
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip",
              dl)

ratings <-
  fread(
    text = gsub("::", "\t", readLines(unzip(
      dl, "ml-10M100K/ratings.dat"
    ))),
    col.names = c("userId", "movieId", "rating", "timestamp")
  )

movies <-
  str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <-
  as.data.frame(movies) %>% mutate(
    movieId = as.numeric(movieId),
    title = as.character(title),
    genres = as.character(genres)
  )

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#########################################################
# end of HarvardX code
#########################################################

######################## cleaning and exploring #######################

## QC imported data
##############################

## check NA's
sapply(edx, function(x)
  sum(is.na(x)))
sapply(validation, function(x)
  sum(is.na(x)))

# backup original
edx_org <- edx

### delete records with "no genres listed"
edx <- filter(edx, genres != "(no genres listed)")

## ranges of rating, movieId, userId
sort((distinct(edx, rating))$rating)
count(distinct(edx, movieId))
count(distinct(edx, userId))
count(distinct(validation, movieId))
count(distinct(validation, userId))

##########QC plot

## image ratings vs movieId, userId (it will take very long time, therefore commented)
#edx%>%ggplot(aes(movieId,userId,color=rating))+
#  geom_point()+geom_text()

## rating coverage histogram
edx %>% group_by(rating) %>% ggplot(aes(rating)) + geom_histogram() +
  labs(title = "Histogram of ratings", y = "count", x = "rating")

## rating coverage vs movieId histogram
edx %>% group_by(movieId) %>% ggplot(aes(movieId)) + geom_histogram() +
  scale_x_log10() +
  labs(title = "Histogram of movieId", y = "number of ratings", x = "movieId, logistic")

## rating coverage vs userId histogram
edx %>% group_by(userId) %>% ggplot(aes(userId)) + geom_histogram() +
  scale_x_log10() +
  labs(title = "Histogram of userId", y = "number of ratings", x = "userId, logistic")



## genre QC

# count genre numbers
count(distinct(edx,genres))

# genre with minimum rating number
min(edx %>% group_by(genres) %>% summarise(sum = n()) %>% .$sum)

# genre with maximum rating number
max(edx %>% group_by(genres) %>% summarise(sum = n()) %>% .$sum)


# genre plot
# rating coverage vs genre plot
edx%>%group_by(genres)%>%summarise(sum=n())%>%arrange(sum)%>%cbind(genre_no=c(seq(1:796)))%>%
  ggplot(aes(genre_no,sum))+geom_point()+    
  scale_y_log10() +labs(title = "Plot by genres", x="genre No.", y="rating count, logistic ")


## boxplot of top 20 genres with high rating numbers 
genre<-edx%>%group_by(genres)%>%summarise(sum=n())%>%arrange(desc(sum))%>%top_n(20)%>%left_join(edx,by="genres")%>%arrange(desc(sum))
range(genre$sum)
genre%>%ggplot(aes(x=reorder(genres,-sum),rating))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Box plot of rating numbers of top 20 genres", x="genre")+ ylim(0,6)

## boxplot of bottom 20 genres with high rating numbers  
genre<-edx%>%group_by(genres)%>%summarise(sum=n())%>%arrange(desc(sum))%>%top_n(-20)%>%left_join(edx,by="genres")%>%arrange(sum)
range(genre$sum)
genre%>%ggplot(aes(x=reorder(genres,sum),rating))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Box plot of rating numbers of bottom 20 genres", x="genre")+ ylim(0,6)


######################## Data analysis #######################

# model 1
#mu throughout the all users and movies
mu_hat <- mean(edx$rating)

# Evaluate mu model result
rmse_mu <- RMSE(mu_hat, validation$rating)
rmse_mu

rmse_results <- data_frame(method = "mu Only", RMSE = rmse_mu)

# model 2
#b_i
bi <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))


# Evaluate bi model result
predicted_ratings <- mu_hat + validation %>%
  left_join(bi, by = 'movieId') %>%
  .$b_i

rmse_bi <- RMSE(predicted_ratings, validation$rating)
rmse_bi

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie Effect Model",
                                     RMSE = rmse_bi))

rmse_results %>% knitr::kable()

# model 3
#b_u

b_u <- edx %>%
  left_join(bi, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- validation %>%
  left_join(bi, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

rmse_bu <- RMSE(predicted_ratings, validation$rating)
rmse_bu
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User Effect Model",
                                     RMSE = rmse_bu))

rmse_results %>% knitr::kable()

# model 4
#b_g Genres factor

b_g <- edx %>%
  left_join(bi, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))

predicted_ratings <- validation %>%
  left_join(bi, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = 'genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  .$pred

rmse_bg <- RMSE(predicted_ratings, validation$rating)
rmse_bg
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User + Genre Effect Model",
                                     RMSE = rmse_bg))

rmse_results %>% knitr::kable()

# model 5 - lambda scan
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l) {
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + l))
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + l))
  
  b_g <- edx %>%
    left_join(bi, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    group_by(genres) %>%
    summarize(b_g = mean(rating - mu_hat - b_i - b_u))
  
  predicted_ratings <- validation %>%
    left_join(bi, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    left_join(b_g, by = 'genres') %>%
    mutate(pred = mu_hat + b_i + b_u + b_g) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})
plot(lambdas, rmses)
lambdas[which.min(rmses)]

# model 5
############ regularized b_i, b_u + bg
lambda <- 5
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))

b_g <- edx %>%
  left_join(bi, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))

predicted_ratings <- validation %>%
  left_join(bi, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = 'genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  .$pred

rmse_bg <- RMSE(predicted_ratings, validation$rating)
rmse_bg

rmse_results <- bind_rows(
  rmse_results,
  data_frame(method = "Reglarized Movie & User + Genre Effect Model",
             RMSE = rmse_bg)
)
## print the table
rmse_results %>% knitr::kable()

## add model numbers
rmse_results <-
  rmse_results %>% cbind(model = c("model 1", "model 2", "model 3", "model 4", "model 5"))



## prepare for final plot
rmse_results$method <- reorder(rmse_results$method, rmse_results$RMSE)

############### final results plot
rmse_results %>%
  ggplot(aes(model, RMSE, fill = method)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_cartesian(ylim = c(0.5, 1.1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Movie recommendation moldels' result" , x = "model", caption = "")

################################ end ################################################

