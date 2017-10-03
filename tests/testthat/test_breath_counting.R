library(tidyverse)
context('Breath counting')

# fixture setup
bc_df       <- read_csv('../fixtures/bc.csv')
bc_accuracy <- bc_df %>%
  select(subject) %>%
  unique() %>%
  dplyr::rename(participant = subject) %>%
  mutate(total=0,correct=0,incorrect=0)

test_that("fixture is a data frame", {
  expect_is(bc_df, "data.frame")
})

test_that("breath_counting_accuracy() returns a data frame", {
  expect_is(breath_counting_accuracy(p = bc_df[, 'subject'], bc_df = bc_df), "data.frame")
})

test_that("breath_counting_accuracy() returns a data frame via do()", {
  bc_accuracy <- expand.grid(p=bc_accuracy$participant) %>%
    do(., breath_counting_accuracy(.$p, bc_df)) %>%
    rowwise %>%
    arrange(p)
  expect_is(bc_accuracy, "data.frame")
})

test_that("summarise_breath_counting_accuracy() returns a data frame", {
  expect_is(summarise_breath_counting_accuracy(bc_df, data.frame(p = 1:20)), 'data.frame')
})
