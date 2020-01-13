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
                                origin="1970-01-01")) -> ratings 


movies %>%
  rename(movie_id = movieId) -> movies

# Different avaliable methods to deal with rating
# 1. Leave rating - if user has watched 2 films than he likes both 
# 2. Drop ratings below 3 
#   -this way this info is lost - losing ~19 000 (20%) observations


ratings %>% 
  select(user_id, movie_id) %>%
  write_csv("data/temp_trans.csv")

trans <- read.transactions("data/temp_trans.csv", 
                          format="single", 
                          sep=",", 
                          header = T,
                          cols=c("user_id","movie_id"))



rules.trans<-apriori(trans, parameter=list(supp=0.1, conf=0.5)) 
rules.by.conf<-sort(rules.trans, by="confidence", decreasing=TRUE) 
inspect(rules.by.conf[1:10])
a <- rules.by.conf[1:10]



# Recommenderlab for ubcf ----
library(recommenderlab)
ratings %>% select(-timestamp) %>% as.data.frame() -> ratings_matrix
affinity.matrix<- as(ratings_matrix,"realRatingMatrix")

a <- recommenderlab::Recommender(affinity.matrix[1:100], method = "UBCF")

as(predict(a, affinity.matrix[101:103,], n=5), "list")


# Recommenderlab for apriori ----
library(recommenderlab)
ratings %>% select(-timestamp) %>% as.data.frame() -> ratings_matrix
affinity.matrix<- as(ratings_matrix,"realRatingMatrix")

a <- recommenderlab::Recommender(affinity.matrix[1:100], method = "UBCF")

as(predict(a, affinity.matrix[101:103,], n=5), "list")


recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")