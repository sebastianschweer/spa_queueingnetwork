context("test-queueing_network.R")

test_that("Result is Data Frame", {
  expect_equal(class(queueing_network_poi_geom(10,10,0.3, 0.4, 2, 1, 0.6, 0.7)),"data.frame")
})
