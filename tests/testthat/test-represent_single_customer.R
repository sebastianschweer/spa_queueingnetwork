context("test-represent_single_customer.R")

test_that("Output of single customer as expected", {
  expect_equal(represent_single_customer(1,c(1,2),100), 
               c(NA, NA, 2, rep(NA,97)))
  expect_equal(represent_single_customer(2,c(1,2),100), 
               c(NA, NA, 1, rep(NA,97)))
})

test_that("Max Length defines length of vector", {
  expect_equal(length(represent_single_customer(1,c(1,2, 300),95)), 
               95)
  expect_equal(length(represent_single_customer(1,c(1,2),95)), 
               95)
  expect_equal(length(represent_single_customer(1,c(1,2, 300),2)), 
               2)
  expect_equal(length(represent_single_customer(1,c(1),2)), 
               2)
})

test_that("Exactly one entry is not NA", {
  expect_equal(sum(is.na(represent_single_customer(1,c(1,2, 300),95))), 
               95)
  expect_equal(sum(!is.na(represent_single_customer(1,c(1,2, 300),95))), 
               0)
  expect_equal(sum(!is.na(represent_single_customer(1,c(1,1,30),95))), 
               1)
  expect_equal(sum(!is.na(represent_single_customer(2,c(1),95))), 
               1)
  expect_equal(sum(is.na(represent_single_customer(1,c(1,1,2),99))), 
               98)
})
