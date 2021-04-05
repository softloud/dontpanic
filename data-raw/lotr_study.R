# https://www.kaggle.com/paultimothymooney/lord-of-the-rings-data/data

library(tidyverse)


lotr_names <-
  read_csv("data-raw/lotr_characters.csv") %>%
  pluck("name")

lotr_study <-
  tibble(
    author = lotr_names,
    year = seq(1950, 2020) %>% sample(size = length(lotr_names), replace = TRUE)
  ) %>%
  dplyr::filter(str_length(author) < 8)

usethis::use_data(lotr_study, overwrite = TRUE)
