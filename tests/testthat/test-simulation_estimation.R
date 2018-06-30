context("test-simulation_estimation.R")

p_12 = 0.1
p_21 = 0.2
lambda_1 = 3
lambda_2 = 1
G_1 = 0.3
G_2 = 0.2

qn_1 <- queueing_network_poi_geom(10000, 10000, p_12 = p_12, p_21 = p_21,
                                  lambda_1 = lambda_1, lambda_2 = lambda_2, 
                                  G_1 = G_1, G_2 = G_2)



test_that("Cross-Covariance satsifies Lemma 6, Edelmann/Wichelhaus for k = 0", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 0, nobs = NULL) >  0.05, FALSE)

})



test_that("Cross-Covariance satisfies Lemma 6, Edelmann/Wichelhaus for k = 1", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) < -0.05 + lambda_1*G_1*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) >  0.05 + lambda_1*G_1*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 1, nobs = NULL) < -0.05 + lambda_1*0*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 1, nobs = NULL) >  0.05 + lambda_1*0*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 1, nobs = NULL) < -0.05 + lambda_2*0*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 1, nobs = NULL) >  0.05 + lambda_2*0*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 1, nobs = NULL) < -0.05 + lambda_2*G_2*(1 - p_21)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 1, nobs = NULL) >  0.05 + lambda_2*G_2*(1 - p_21)/(1 - p_12*p_21), FALSE)
  
})

test_that("Cross-Covariance satisfies Lemma 6, Edelmann/Wichelhaus for k = 2", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 2, nobs = NULL) < -0.05 + lambda_1*G_1*(1-G_1)*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 2, nobs = NULL) >  0.05 + lambda_1*G_1*(1-G_1)*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) < -0.05 + lambda_1*G_1*G_2*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) >  0.05 + lambda_1*G_1*G_2*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 2, nobs = NULL) < -0.05 + lambda_2*G_1*G_2*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 2, nobs = NULL) >  0.05 + lambda_2*G_1*G_2*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 2, nobs = NULL) < -0.05 + lambda_2*G_2*(1-G_2)*(1 - p_21)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 2, nobs = NULL) >  0.05 + lambda_2*G_2*(1-G_2)*(1 - p_21)/(1 - p_12*p_21), FALSE)
  
})


alpha_1_1 = queueingnetworkR:::cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) 
alpha_2_2 = queueingnetworkR:::cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) 

test_that("Cross-Covariance estimator yields expected result for k = 1", {
  expect_equal((1-p_12)*alpha_2_2/(alpha_1_1*(1-p_21)*p_12) - G_2 < 0.05, TRUE)
  expect_equal((1-p_12)*alpha_2_2/(alpha_1_1*(1-p_21)*p_12) - G_2 > -0.05, TRUE)
})


p_12 = 0.6
p_21 = 0.8
lambda_1 = 0.3
lambda_2 = 1
G_1 = 0.3
G_2 = 0.2

qn_1 <- queueing_network_poi_geom(10000, 10000, p_12 = p_12, p_21 = p_21,
                                  lambda_1 = lambda_1, lambda_2 = lambda_2, 
                                  G_1 = G_1, G_2 = G_2)



test_that("Cross-Covariance satisfies Lemma 6, Edelmann/Wichelhaus for k = 0", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 0, nobs = NULL) >  0.05, FALSE)
  
})

test_that("Cross-Covariance satisfies Lemma 6, Edelmann/Wichelhaus for k = 1", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) < -0.05 + lambda_1*G_1*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) >  0.05 + lambda_1*G_1*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 1, nobs = NULL) < -0.05 + lambda_1*0*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 1, nobs = NULL) >  0.05 + lambda_1*0*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 1, nobs = NULL) < -0.05 + lambda_2*0*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 1, nobs = NULL) >  0.05 + lambda_2*0*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 1, nobs = NULL) < -0.05 + lambda_2*G_2*(1 - p_21)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 1, nobs = NULL) >  0.05 + lambda_2*G_2*(1 - p_21)/(1 - p_12*p_21), FALSE)
  
})

test_that("Cross-Covariance satisfies Lemma 6, Edelmann/Wichelhaus for k = 2", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 2, nobs = NULL) < -0.05 + lambda_1*G_1*(1-G_1)*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 2, nobs = NULL) >  0.05 + lambda_1*G_1*(1-G_1)*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) < -0.05 + lambda_1*G_1*G_2*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) >  0.05 + lambda_1*G_1*G_2*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 2, nobs = NULL) < -0.05 + lambda_2*G_1*G_2*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 2, nobs = NULL) >  0.05 + lambda_2*G_1*G_2*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 2, nobs = NULL) < -0.05 + lambda_2*G_2*(1-G_2)*(1 - p_21)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 2, nobs = NULL) >  0.05 + lambda_2*G_2*(1-G_2)*(1 - p_21)/(1 - p_12*p_21), FALSE)
  
})


alpha_1_1 = queueingnetworkR:::cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) 
alpha_2_2 = queueingnetworkR:::cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) 

test_that("Cross-Covariance estimator yields expected result for k = 1", {
  expect_equal((1-p_12)*alpha_2_2/(alpha_1_1*(1-p_21)*p_12) - G_2 < 0.05, TRUE)
  expect_equal((1-p_12)*alpha_2_2/(alpha_1_1*(1-p_21)*p_12) - G_2 > -0.05, TRUE)
})



p_12 = 0.4
p_21 = 0.3
lambda_1 = 1
lambda_2 = 3
G_1 = 0.8
G_2 = 0.7

qn_1 <- queueingnetworkR::queueing_network_poi_geom(10000, 10000, p_12 = p_12, p_21 = p_21,
                                  lambda_1 = lambda_1, lambda_2 = lambda_2, 
                                  G_1 = G_1, G_2 = G_2)



test_that("Cross-Covariance satsifies Lemma 6, Edelmann/Wichelhaus for k = 0", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 0, nobs = NULL) >  0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 0, nobs = NULL) < -0.05, FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 0, nobs = NULL) >  0.05, FALSE)
  
})

test_that("Cross-Covariance satsifies Lemma 6, Edelmann/Wichelhaus for k = 1", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) < -0.05 + lambda_1*G_1*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) >  0.05 + lambda_1*G_1*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 1, nobs = NULL) < -0.05 + lambda_1*0*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 1, nobs = NULL) >  0.05 + lambda_1*0*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 1, nobs = NULL) < -0.05 + lambda_2*0*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 1, nobs = NULL) >  0.05 + lambda_2*0*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 1, nobs = NULL) < -0.05 + lambda_2*G_2*(1 - p_21)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 1, nobs = NULL) >  0.05 + lambda_2*G_2*(1 - p_21)/(1 - p_12*p_21), FALSE)
  
})

test_that("Cross-Covariance satisfies Lemma 6, Edelmann/Wichelhaus for k = 2", {
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 2, nobs = NULL) < -0.05 + lambda_1*G_1*(1-G_1)*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 2, nobs = NULL) >  0.05 + lambda_1*G_1*(1-G_1)*(1 - p_12)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) < -0.05 + lambda_1*G_1*G_2*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) >  0.05 + lambda_1*G_1*G_2*(1 - p_21)*p_12/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 2, nobs = NULL) < -0.05 + lambda_2*G_1*G_2*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 1, lag = 2, nobs = NULL) >  0.05 + lambda_2*G_1*G_2*(1 - p_12)*p_21/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 2, nobs = NULL) < -0.05 + lambda_2*G_2*(1-G_2)*(1 - p_21)/(1 - p_12*p_21), FALSE)
  expect_equal(cross_covariance(qn_1, node_1 = 2, node_2 = 2, lag = 2, nobs = NULL) >  0.05 + lambda_2*G_2*(1-G_2)*(1 - p_21)/(1 - p_12*p_21), FALSE)
  
})


alpha_1_1 = queueingnetworkR:::cross_covariance(qn_1, node_1 = 1, node_2 = 1, lag = 1, nobs = NULL) 
alpha_2_2 = queueingnetworkR:::cross_covariance(qn_1, node_1 = 1, node_2 = 2, lag = 2, nobs = NULL) 

test_that("Cross-Covariance estimator yields expected result for k = 1", {
  expect_equal((1-p_12)*alpha_2_2/(alpha_1_1*(1-p_21)*p_12) - G_2 < 0.05, TRUE)
  expect_equal((1-p_12)*alpha_2_2/(alpha_1_1*(1-p_21)*p_12) - G_2 > -0.05, TRUE)
})
