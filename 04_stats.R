
# Calc measures
apriori_measures_raw %>%
  rename(true_positive = in_both,
         false_positive = only_in_predicted) %>%
  mutate(precision = true_positive/predicted_cnt) %>%
  select(user_id, precision, everything()) -> apriori_measures



apriori_measures %>%
  select(actual_cnt) %>%
  table()

apriori_measures %>%
  select(predicted_cnt) %>%
  table()

# 498-464 = 34 less than 20

apriori_measures %>%
  select(true_positive) %>%
  table()


apriori_measures %>%
  select(-user_id, -precision) %>%
  summarise_all(sum) %>%
  mutate(precision = true_positive/predicted_cnt) %>%
  select(precision, everything())


# UBCF

# Calc measures
ubcf_measures_raw %>%
  rename(true_positive = in_both,
         false_positive = only_in_predicted) %>%
  mutate(precision = true_positive/predicted_cnt) %>%
  select(user_id, precision, everything()) -> ubcf_measures



ubcf_measures %>%
  select(actual_cnt) %>%
  table()

ubcf_measures %>%
  select(predicted_cnt) %>%
  table()

# All have complete

ubcf_measures %>%
  select(true_positive) %>%
  table()


ubcf_measures %>%
  summarise_all(sum) %>%
  mutate(precision = true_positive/predicted_cnt) %>%
  select(precision, everything())


ubcf_measures %>%
  summarise_all(sum) %>%
  mutate(precision = true_positive/predicted_cnt) %>%
  .$precision %>% sprintf("%.3f", .)

bind_rows(apriori_measures, 
          ubcf_measures, 
          .id = c("method")) %>% 
  mutate(method = ifelse(method == 1, "apriori", "UBCF")) %>%
  ggplot(aes(x = precision, fill = method)) + 
  geom_density(alpha = 0.5)

bind_rows(apriori_measures, 
          ubcf_measures, 
          .id = c("method")) %>% 
  mutate(method = ifelse(method == 1, "apriori", "UBCF")) %>%
  ggplot(aes(x = precision, fill = method)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(8, "Set1")[c(2,5)]) +
  # facet_grid(rows = vars(method)) +
  theme_minimal()

  




bind_rows(apriori_measures,
          ubcf_measures,
          .id = c("Method")) %>%
  mutate(Method = ifelse(Method == 1, "apriori", "UBCF")) %>%
  filter(!is.na(precision)) %>%
  select(Method, precision) %>%
  group_by(Method) %>%
  summarise(
    `Min` = min(precision),
    `25%` = quantile(precision, 0.25),
    `Median` = quantile(precision, 0.5),
    `Mean` = mean(precision),
    `75%` = quantile(precision, 0.75),
    `Max` = max(precision)
  ) %>%
  mutate_if(is.numeric, round, 2)

