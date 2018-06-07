context("test-customers_arrival.R")

test_that("Result is Matrix", {
  expect_equal(class(customers_arrival(1, list())), "matrix")
  expect_equal(class(customers_arrival(1, list(c(1,21,1), c(23,1)))), "matrix")
  expect_equal(class(customers_arrival(2, list(c(1)))), "matrix")
  expect_equal(class(customers_arrival(2, list(c(1,21,1), c(23,1)))), "matrix")
})

test_that("Column number corresponds to arrivals", {
  expect_equal(ncol(customers_arrival(1, list(c(1,21,1), c(23,1)))), 2)
  expect_equal(ncol(customers_arrival(2, list(c(1)))), 1)
  expect_equal(ncol(customers_arrival(2, list(c(1,21,1), c(23,1), c(1), c(1)))), 4)
})

test_that("Row number corresponds to arrivals", {
  expect_equal(nrow(customers_arrival(1, list(c(1,21,1), c(23,1)))), 24)
  expect_equal(nrow(customers_arrival(2, list(c(1)))), 1)
  expect_equal(nrow(customers_arrival(2, list(c(1,21,1), c(289,1), c(1), c(1)))), 290)
})

test_that("Row number corresponds to arrivals", {
  target_1 = matrix(c(NA,NA, NA, NA, NA, 1), nrow = 6, ncol = 1, byrow = FALSE)
  target_2 = matrix(c(NA,NA,2, NA, 2, NA), nrow = 3, ncol = 2, byrow = FALSE)
  expect_equal(customers_arrival(1, list(c(1,2), c(1,1))), target_2)
  expect_equal(customers_arrival(1, list(c(2,1), c(1,1))), target_2)
  expect_equal(customers_arrival(2, list(c(3), c(2))), target_2)
  expect_equal(customers_arrival(1, list(c(3,1,2))), target_1)
  expect_equal(customers_arrival(1, list(c(6))), target_1)
  expect_equal(customers_arrival(2, list(c(1,1,1,1,1,1))), target_1)
})
