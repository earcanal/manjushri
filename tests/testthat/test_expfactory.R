library(tidyverse)
context('The Experiment Factory')

token <- '07ba0ce7-cc4c-4b55-becd-8ae3d9e7eaf6'
df <- process_ant('../fixtures/ant.json', p=token, time=1)
test_that("process_ant() can process JSON", {
  expect_is(df, 'data.frame')
})
test_that("process_ant() returns correct p column value", {
  expect_equal(df[1,'p'], token)
})
df <- process_new_expfactory_survey(token='1', survey='../fixtures/sms.json')
test_that("process_new_expfactory_survey() returns a data frame", {
   expect_is(df, 'data.frame')
})
test_that('SMS structured JSON correctly parsed', {
  expect_equal(df[1,4], 'I was aware of different emotions that arose in me')
})

df <- process_new_expfactory_survey(token='1', survey='../fixtures/sms-flat.json', flat=TRUE)
test_that("process_new_expfactory_survey() can process flat JSON", {
  expect_is(df, 'data.frame')
})
test_that('SMS flat JSON correctly parsed', {
  expect_equal(df[1,4], 'I was aware of different emotions that arose in me')
})

test_that("process_expfactory_experiment() returns a data frame", {
  expect_is(process_expfactory_experiment(path = '../fixtures/attention-network-task-no-feedback-results.json'), 'data.frame')
})

