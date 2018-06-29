simulation_estimation <- function(max_lag, p_12, p_21, ...){
  
  data = data <- queueing_network_poi_geom(p_12 = p_12, p_21 = p_21, ...)
  
  alpha_12_hat = sapply(c(1:max_lag), 
                         queueingnetworkR:::cross_covariance, 
                         data = data, node_1 = 1, node_2 = 2, nobs = NULL)
  alpha_11_hat = sapply(c(1:max_lag),
                         queueingnetworkR:::cross_covariance, 
                         data = data, node_1 = 1, node_2 = 1, nobs = NULL)
  g_2_est = queueingnetworkR:::deconvoluting_alpha(max_lag,
                                alpha_1 = alpha_11_hat,
                                alpha_2 = alpha_12_hat,
                                p_12 = p_12,
                                p_21 = p_21)
  return(g_2_est)
}

#simulation_estimation(10, 0.1, 0.9, n_obs = 100000, burn_in = 1000, lambda_1 = 1, lambda_2 = 0.3, G_1 = 0.3, G_2 = 0.9, progress = TRUE)
# 
# p_12 = 0.7
# p_21 = 0.3
# max_lag = 12
# 
# data <- queueing_network_poi_geom(p_12 = p_12, p_21 = p_21, n_obs = 10000, burn_in = 1000, lambda_1 = 0.3, lambda_2 = 0.5, G_1 = 0.3, G_2 = 0.1, progress = TRUE)
# 
# alpha_12_hat = sapply(c(1:max_lag),
#                       cross_covariance,
#                       data = data, node_1 = 1, node_2 = 2, nobs = NULL)
# alpha_11_hat = sapply(c(1:max_lag),
#                       queueingnetworkR:::cross_covariance,
#                       data = data, node_1 = 1, node_2 = 1, nobs = NULL)
# g_2_est = deconvoluting_alpha(max_lag,
#                               alpha_1 = alpha_11_hat,
#                               alpha_2 = alpha_12_hat,
#                               p_12 = p_12,
#                               p_21 = p_21)