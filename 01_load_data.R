library(tidyverse)
library(readr)

#' userId | movieId | rating | timestamp 
ratings <- read_csv("data/ratings.csv")

#' movieId | title | genres (pipe separated)
movies <- read_csv("data/movies.csv")


ratings %>%
  rename(user_id = userId,
         movie_id = movieId) %>%
  mutate(timestamp = as.POSIXct(timestamp, 
                                origin="1970-01-01")) -> ratings 


movies %>%
  rename(movie_id = movieId) -> movies

#' Unique users and movies
ratings %>% 
  summarise(user_id = n_distinct(user_id),
            movie_id = n_distinct(movie_id))


#' movie_id from movies and ratings match

#' Histogram of rating
ggplot(ratings, aes(x = rating)) +
  geom_histogram()

table(ratings$rating)

#' Top films 
ratings %>% 
  group_by(movie_id) %>%
  summarise(avg_rating = mean(rating),
            cnt_votes = n()) %>%
  arrange(-cnt_votes) %>%
  left_join(movies) -> top_movies


ggplot(top_movies, aes(cnt_votes)) +
  geom_histogram()


#' Users

ratings %>%
  group_by(user_id) %>%
  summarise(cnt_votes = n()) %>%
  arrange(-cnt_votes) %>%
  ggplot(aes(cnt_votes)) +
  geom_histogram()






# save(ratings, movies, file = "data/01_datasets.Rdata")


