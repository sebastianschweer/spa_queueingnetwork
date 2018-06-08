cross_covariance <- function(data, node_1, node_2, lag, nobs){
  if(is.null(nobs)){
    nobs = nrow(data)
  }
  if(!is.null(nobs) & nobs > nrow(data)){
    nobs = nrow(data)
  }
  if(lag < nobs){
    estimator = cov(data$arrival_1, data$departure_1)
  } else {
    estimator = 0
  }
  return(estimator)
}