context("test-simulate_single_customer.R")

test_that("Result is numeric vector", {
  expect_equal(class(simulate_single_customer(1,0.9,0.9,0.3,0.7)), "numeric")
  expect_equal(class(simulate_single_customer(2,0.1,0.001,0.3,0.7)), "numeric")
  expect_equal(class(simulate_single_customer(2,0.001,0.001,0.3,0.7)), "numeric")
  expect_message(simulate_single_customer(1,1,1,0.3,0.7), "Probabilities of geometric distribution out of allowed range")
  expect_equal(is.vector(simulate_single_customer(1,0.9,0.9,0.3,0.7)), TRUE)
  expect_equal(is.vector(simulate_single_customer(2,0.1,0.001,0.3,0.7)), TRUE)
  expect_equal(is.vector(simulate_single_customer(2,0.001,0.001,0.3,0.7)), TRUE)
})
