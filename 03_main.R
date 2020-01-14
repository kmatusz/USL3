library(recommenderlab)
library(tidyverse)
library(readr)
library(arules)
library(arulesViz)


#' userId | movieId | rating | timestamp
ratings <- read_csv("data/ratings.csv")

#' movieId | title | genres (pipe separated)
movies <- read_csv("data/movies.csv")


ratings %>%
  rename(user_id = userId,
         movie_id = movieId) %>%
  mutate(timestamp = as.POSIXct(timestamp,
                                origin = "1970-01-01")) -> ratings

movies %>%
  rename(movie_id = movieId) -> movies

# Filtrowanie użytkowników z mniej niż 30 ocen ----

ratings %>%
  group_by(user_id) %>%
  summarise(cnt_votes = n()) %>%
  filter(cnt_votes > 30) %>%
  .$user_id -> big_users


ratings_filtered <- ratings %>%
  filter(user_id %in% big_users)

# wYbranie x najnowszych ratingów ----

ratings_filtered %>%
  group_by(user_id) %>%
  top_n(1, user_id)

ratings_filtered <- ratings_filtered %>% mutate(id = row_number())

ratings_filtered %>%
  arrange(user_id, timestamp) %>%
  group_by(user_id) %>%
  mutate(time = row_number(),
         inv_time = max(time) - time + 1) %>%
  ungroup() -> ratings_ordered

ratings_ordered %>% left_join(movies) %>%
  select(-genres, -movie_id) %>%
  rename(movie_id = title) -> ratings_ordered

# inv_time = 1 - most recent
# time = 1 - oldest

no_obs_to_test <- 30

ratings_ordered %>%
  filter(inv_time > no_obs_to_test) -> training_set

ratings_ordered %>%
  filter(inv_time <= no_obs_to_test) -> test_set

# Apriori ----

training_set %>%
  select(user_id, movie_id) %>%
  write_csv("data/temp_trans.csv")

trans <- read.transactions(
  "data/temp_trans.csv",
  format = "single",
  sep = ",",
  header = T,
  cols = c("user_id", "movie_id")
)


# PObawić się parametrami
rules.trans <- apriori(trans, parameter = list(supp = 0.01, conf = 0.5))

rules.by.conf <- sort(rules.trans, by = "confidence", decreasing = TRUE)

inspect(rules.by.conf[1:100])
a <- rules.by.conf[1:10]

# recommenderlab - prepare----

training_set %>%
  select(user_id, movie_id) %>%
  mutate(rating = 1) %>%
  as.data.frame() -> training_ratings_matrix

training_affinity_matrix <- as(training_ratings_matrix, "binaryRatingMatrix")

# Train models
ubcf_model <-
  recommenderlab::Recommender(training_affinity_matrix,
                              method = "UBCF")

ibcf_model <-
  recommenderlab::Recommender(training_affinity_matrix,
                              method = "IBCF")


apriori_model <-
  recommenderlab::Recommender(training_affinity_matrix,
                              method = "AR",
                              parameter = list(supp = 0.1, conf = 0.5))

# Predict on the same data
ubcf_predictions_raw <- as(predict(ubcf_model, 
           training_affinity_matrix, n = 100, type = "topN"), "list")

apriori_predictions_raw <- as(predict(apriori_model, 
                               training_affinity_matrix, n = 100, type = "topN"), "list")


# Evaluate
# filter ubcf to the same size?

test_set %>%
  select(user_id, movie_id) %>%
  group_by(user_id) %>%
  nest() %>%
  .$data %>%
  as.list() %>%
  map(function(x) as.character(x$movie_id))-> set



map2(ubcf_predictions_raw, set, function(x,y) 
  list(predicted = x, actual = y)) -> comparison

accuracy_measures <- function(predicted, actual){
  tibble(
    in_both = sum(predicted %in% actual),
    only_in_predicted = length(predicted) - in_both,
    only_in_actual = length(actual) - in_both,
    predicted_cnt = length(predicted), 
    actual_cnt = length(actual) 
  )
}

comparison %>%
  map_df(function(x) accuracy_measures(x$predicted, x$actual)
         ) %>%
  mutate(user_id = row_number()) -> measures_raw

measures_raw



