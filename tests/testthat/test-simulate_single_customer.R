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

test_that("Proportion of customers leaving from a certain node corresponds to theory",{
  p_12 = 0.3
  p_21 = 0.8
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(1, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 1})) > (1 - p_12)/(1 - p_12*p_21) - 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(1, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 1})) < (1 - p_12)/(1 - p_12*p_21) + 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(2, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 0})) > (1 - p_12)*p_21/(1 - p_12*p_21) - 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(2, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 0})) < (1 - p_12)*p_21/(1 - p_12*p_21) + 0.1, TRUE)
  p_12 = 0.1
  p_21 = 0.1
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(1, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 1})) > (1 - p_12)/(1 - p_12*p_21) - 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(1, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 1})) < (1 - p_12)/(1 - p_12*p_21) + 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(2, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 0})) > (1 - p_12)*p_21/(1 - p_12*p_21) - 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(2, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 0})) < (1 - p_12)*p_21/(1 - p_12*p_21) + 0.1, TRUE)
  p_12 = 0.9
  p_21 = 0.8
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(1, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 1})) > (1 - p_12)/(1 - p_12*p_21) - 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(1, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 1})) < (1 - p_12)/(1 - p_12*p_21) + 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(2, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 0})) > (1 - p_12)*p_21/(1 - p_12*p_21) - 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(2, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 0})) < (1 - p_12)*p_21/(1 - p_12*p_21) + 0.1, TRUE)
  p_12 = 0.78
  p_21 = 0.1
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(1, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 1})) > (1 - p_12)/(1 - p_12*p_21) - 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(1, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 1})) < (1 - p_12)/(1 - p_12*p_21) + 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(2, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 0})) > (1 - p_12)*p_21/(1 - p_12*p_21) - 0.1, TRUE)
  expect_equal(mean(replicate(1000, {length(simulate_single_customer(2, p_12 = p_12, p_21 = p_21, 0.6, 0.8)) %% 2 == 0})) < (1 - p_12)*p_21/(1 - p_12*p_21) + 0.1, TRUE)
  
})  