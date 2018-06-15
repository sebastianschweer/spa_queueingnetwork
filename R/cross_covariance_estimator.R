cross_covariance <- function(data, node_1, node_2, lag, nobs){
  if(is.null(nobs)){
    nobs = nrow(data)
  }
  if(!is.null(nobs) & nobs > nrow(data)){
    nobs = nrow(data)
  }
  if(lag < nobs){
    if(node_1 == 1){
      arrival = data$arrival_1
    } else if (node_1 ==2){
      arrival = data$arrival_1
    } else {
      message("Arrival node not contained in the model")
    }
    
    if(node_2 == 1){
      departure = data$departure_1
    } else if (node_2 ==2){
      departure = data$departure_2
    } else {
      message("Departure node not contained in the model")
    }
    arrival_k   = head(arrival, (nobs - lag))
    departure_k = tail(departure, (nobs - lag))
    ### Following formula (3) in Schweer/Wichelhaus
    estimator = 1/nobs*(sum(arrival_k*departure_k)) - 1/nobs*sum(arrival_k)*mean(departure) - 1/nobs*sum(departure_k)*mean(arrival) + (nobs - lag)/nobs*mean(arrival)*mean(departure)
  } else {
    estimator = 0
  }
  return(estimator)
}


# Following Lemma 9 in Edelmann/Wichelhaus
deconvoluting_alpha <- function(number_of_steps, alpha_1, alpha_2, p_12, p_21){
  M_n = min(which(alpha_1 > 0))
  if(is.finite(M_n)){
    length_1 = length(alpha_1) - M_n + 1
    length_2 = length(alpha_2) - M_n 
    max_num_steps = min(length_1, length_2, number_of_steps)
    if(max_num_steps > 1){
      v_n = numeric(length = max_num_steps)
      v_n[1] = (1 - p_12)* alpha_2[M_n + 1] / (alpha_1[M_n] * (1 - p_21) * p_12)
      
      for(idx in c(2:max_num_steps)){
        alpha_1_k = alpha_1[(M_n + idx -1):(M_n + 1)]
        v_n[idx] = (1 - p_12)* alpha_2[M_n + idx] / (alpha_1[M_n] * (1 - p_21) * p_12) - (sum(alpha_1_k*v_n[1:(idx-1)]))/alpha_1[M_n]
      }
      return(v_n)
    } else {
      message("Not enough data points for deconvolution")    
      }
  } else {
    message("Alpha Estimator consists of 0 values")
  }
}