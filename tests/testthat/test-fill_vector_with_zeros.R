context("test-fill_vector_with_zeros.R")

test_that("Filled Vector has expected length", {
  expect_equal(length(fill_vector_with_zeros(rep(NA, 3), 10, 20)), 20)
  expect_equal(length(fill_vector_with_zeros(rep(NA, 35), 10, 20)), 20)
  expect_equal(length(fill_vector_with_zeros(rep(NA, 0), 0, 24)), 24)
  expect_equal(length(fill_vector_with_zeros(rep(NA, 3), 0, 0)), 0)
})

test_that("Filled Vector is numeric", {
  expect_equal(class(fill_vector_with_zeros(rep("k", 3), 10, 20)), "numeric")
  expect_equal(class(fill_vector_with_zeros(rep(NA, 35), 10, 20)), "numeric")
  expect_equal(class(fill_vector_with_zeros(c(2, "alfa", 3), 0, 24)), "numeric")
  expect_equal(class(fill_vector_with_zeros(rep(NA, 3), 0, 0)), "numeric")
})

test_that("Expected Result", {
  expect_equal(fill_vector_with_zeros(rep("k", 3), 2, 7), c(0,0,NA,NA,NA,0,0))
  expect_equal(fill_vector_with_zeros(rep("k", 3), 0, 7), c(NA,NA,NA,0,0,0,0))
  expect_equal(fill_vector_with_zeros(rep("k", 3), 2, 3), c(0,0,NA))
  expect_equal(fill_vector_with_zeros(c(12,2,0,0,1), 2, 9), c(0,0,12,2,0,0,1,0,0))
  expect_equal(fill_vector_with_zeros(c(12,2,0,0,1), 2, 7), c(0,0,12,2,0,0,1))
  expect_equal(fill_vector_with_zeros(c(12,2,0,0,1), 2, 3), c(0,0,12))
})