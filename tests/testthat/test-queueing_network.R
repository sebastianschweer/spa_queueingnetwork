context("test-queueing_network.R")

test_that("Result is Data Frame", {
  expect_equal(class(queueing_network_poi_geom(10,10,0.3, 0.4, 2, 1, 0.6, 0.7)),"data.frame")
  expect_equal(class(queueing_network_poi_geom(1,1000,0.1, 0.9, 14, 2, 0.6, 0.7)),"data.frame")
  expect_equal(class(queueing_network_poi_geom(13,4,0.99, 0.99, 1, 2, 0.6, 0.7)),"data.frame")
})

test_that("Result is Data Frame with 4 Columns", {
  expect_equal(ncol(queueing_network_poi_geom(10,10,0.3, 0.4, 2, 1, 0.6, 0.7)),4)
  expect_equal(ncol(queueing_network_poi_geom(1,1000,0.1, 0.9, 14, 2, 0.6, 0.7)),4)
  expect_equal(ncol(queueing_network_poi_geom(13,4,0.99, 0.99, 1, 2, 0.6, 0.7)),4)
})


test_that("Result has predefined number of rows", {
  expect_equal(nrow(queueing_network_poi_geom(10,10,0.3, 0.4, 2, 1, 0.6, 0.7)),20)
  expect_equal(nrow(queueing_network_poi_geom(1,1000,0.1, 0.9, 14, 2, 0.6, 0.7)),1001)
  expect_equal(nrow(queueing_network_poi_geom(13,4,0.99, 0.99, 1, 2, 0.6, 0.7)),17)
})

test_that("Number of exiting customers is almost that of entering customers", {
  qn_1 <- queueing_network_poi_geom(10,10,0.3, 0.4, 2, 1, 0.6, 0.7)
  qn_2 <- queueing_network_poi_geom(100,10,0.6, 0.1, 12, 1, 0.9, 0.7)
  qn_3 <- queueing_network_poi_geom(10,100,0.1, 0.1, 0.06, 2, 0.1, 0.3)
  
  expect_equal(sum(qn_1$arrival_1 + qn_1$arrival_2) >  sum(qn_1$departure_1 + qn_1$departure_2), TRUE)
  expect_equal(sum(qn_2$arrival_1 + qn_2$arrival_2) >  sum(qn_2$departure_1 + qn_2$departure_2), TRUE)
  expect_equal(sum(qn_3$arrival_1 + qn_3$arrival_2) >  sum(qn_3$departure_1 + qn_3$departure_2), TRUE)
  })

test_that("Number of arriving customers is Poisson distributed", {
  qn_1 <- queueing_network_poi_geom(10,10,0.3, 0.4, 2, 1, 0.6, 0.7)
  qn_2 <- queueing_network_poi_geom(100,10,0.6, 0.1, 12, 1, 0.9, 0.7)
  qn_3 <- queueing_network_poi_geom(10,100,0.1, 0.1, 0.06, 2, 0.1, 0.3)
  
  expect_equal(sum(qn_1$arrival_1) >  0.5*(10+10)*2, TRUE)
  expect_equal(sum(qn_1$arrival_1) <  2*(10+10)*2, TRUE)
  expect_equal(sum(qn_1$arrival_2) >  0.5*(10+10)*1, TRUE)
  expect_equal(sum(qn_1$arrival_2) <  2*(10+10)*1, TRUE)
  expect_equal(sum(qn_2$arrival_1) <  2*(100+10)*12, TRUE)
  expect_equal(sum(qn_2$arrival_1) >  0.5*(100+10)*12, TRUE)
  expect_equal(sum(qn_2$arrival_2) <  2*(100+10)*1, TRUE)
  expect_equal(sum(qn_2$arrival_2) >  0.5*(100+10)*1, TRUE)
  expect_equal(sum(qn_3$arrival_1) >  0.5*(100+10)*0.06, TRUE)
  expect_equal(sum(qn_3$arrival_1) <  2*(100+10)*0.06, TRUE)
  expect_equal(sum(qn_3$arrival_2) >  0.5*(100+10)*2, TRUE)
  expect_equal(sum(qn_3$arrival_2) <  2*(100+10)*2, TRUE)
})

