context("test-deconvoluting_alpha.R")

alpha_1 = c(0.2,0.3,0,0.1)
alpha_2 = c(1,0,0.1,0.1)

test_that("Result is a numeric vector", {
  expect_equal(class(deconvoluting_alpha(4, alpha_1, alpha_2, p_12 = 0.3, p_21 = 0.4)), "numeric")
  expect_equal(class(deconvoluting_alpha(10, alpha_1, alpha_2, p_12 = 0.3, p_21 = 0.4)), "numeric")
  expect_equal(class(deconvoluting_alpha(4, alpha_1[1:2], alpha_2, p_12 = 0.3, p_21 = 0.4)), "numeric")
  expect_equal(class(deconvoluting_alpha(4, alpha_1, alpha_2[1:3], p_12 = 0.3, p_21 = 0.4)), "numeric")
  expect_equal(class(deconvoluting_alpha(2, alpha_1[1:2], alpha_2[1:3], p_12 = 0.5, p_21 = 0.5)), "numeric")
})

test_that("Result has Appropriate Length", {
  expect_equal(length(deconvoluting_alpha(4, alpha_1, alpha_2, p_12 = 0.3, p_21 = 0.4)), 3)
  expect_message(deconvoluting_alpha(1, alpha_1, alpha_2, p_12 = 0.3, p_21 = 0.4), "Not enough data points for deconvolution")
  expect_equal(length(deconvoluting_alpha(10, alpha_1, alpha_2, p_12 = 0.3, p_21 = 0.4)), 3)
  expect_equal(length(deconvoluting_alpha(4, alpha_1[1:2], alpha_2, p_12 = 0.3, p_21 = 0.4)), 2)
  expect_message(deconvoluting_alpha(4, alpha_1[1], alpha_2, p_12 = 0.3, p_21 = 0.4), "Not enough data points for deconvolution")
  expect_equal(length(deconvoluting_alpha(4, alpha_1, alpha_2[1:3], p_12 = 0.3, p_21 = 0.4)), 2)
  expect_equal(length(deconvoluting_alpha(2, alpha_1[1:2], alpha_2[1:3], p_12 = 0.5, p_21 = 0.5)), 2)
})

test_that("Result is Correct", {
  expect_equal(deconvoluting_alpha(2, alpha_1[1:2], alpha_2[1:3], p_12 = 0.5, p_21 = 0.5), c(0,1))
  expect_equal(deconvoluting_alpha(2, alpha_1[1:2], alpha_2[1:3], p_12 = 0.7, p_21 = 0.5), c(0,0.1*0.3/(0.5*0.7*0.2)))
  expect_equal(deconvoluting_alpha(2, alpha_1[1:2], c(1,1,1), p_12 = 0.5, p_21 = 0.5), c(10,-5))
  expect_equal(deconvoluting_alpha(2, c(2,0.3), c(1,1,1), p_12 = 0.5, p_21 = 0.5), c(1,0.85))
})
