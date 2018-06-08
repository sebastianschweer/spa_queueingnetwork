context("test-cross_covariance.R")

data <- 
  data.frame(arrival_1 = c(0,0,1,0),
             arrival_2 = c(0,1,1,1), 
             departure_1 = c(1,1,2,0), 
             departure_2 = c(-4,-5,-0.2,17))

test_that("Result is a numeric value", {
  expect_equal(class(cross_covariance(data, 1, 2, 3, 4)), "numeric")
  expect_equal(class(cross_covariance(data, 1, 1, 0, 4)), "numeric")
  expect_equal(class(cross_covariance(data, 2, 2, 0, 2)), "numeric")
  expect_equal(class(cross_covariance(data, 2, 1, 1, 3)), "numeric")
  expect_equal(class(cross_covariance(data, 2, 1, 1, NULL)), "numeric")
  expect_equal(class(cross_covariance(data, 2, 1, 3, NULL)), "numeric")
  expect_equal(class(cross_covariance(data, 2, 1, 3, 500)), "numeric")
  expect_equal(class(cross_covariance(data, 1, 2, 4, 3)), "numeric")
  expect_equal(class(cross_covariance(data, 1, 2, 2, 1)), "numeric")
  expect_equal(class(cross_covariance(data, 1, 2, 4, NULL)), "numeric")
})

test_that("Estimator is 0 if Observations insufficient", {
  expect_equal(cross_covariance(data, 1, 2, 4, 3), 0)
  expect_equal(cross_covariance(data, 1, 2, 2, 1), 0)
  expect_equal(cross_covariance(data, 1, 2, 5, NULL), 0)
})
