context("test-list_arrival.R")

test_that("Result is a list", {
  expect_equal(class(list_arrival(1, 2, 0.2, 0.2, 0.9, 0.7)), "list")
  expect_equal(class(list_arrival(2, 2, 0.2, 0.2, 0.9, 0.7)), "list")
  expect_equal(class(list_arrival(1, 0, 0.2, 0.2, 0.9, 0.7)), "list")
  expect_equal(class(list_arrival(2, 0, 0.2, 0.2, 0.9, 0.7)), "list")
  expect_equal(length(list_arrival(1, 2, 0.2, 0.2, 0.9, 0.7)), 2)
  expect_equal(length(list_arrival(2, 2, 0.2, 0.2, 0.9, 0.7)), 2)
})

test_that("List contains numerical vectors", {
  arr <- list_arrival(1, 2, 0.2, 0.2, 0.9, 0.7)
  
  expect_equal(is.vector(arr[[1]]), TRUE)
  expect_equal(is.vector(arr[[2]]), TRUE)
  expect_equal(is.numeric(arr[[1]]), TRUE)
  expect_equal(is.numeric(arr[[2]]), TRUE)
})
