#' Simulate a Two-Node Queueing Network
#' 
#' Creates a simulation of a Poi/Geom/infty-Network with 2 nodes and routing 
#' probabilities p_12 and p_21.
#'
#' @param n_obs Integer, number of observations to be analyzed
#' @param burn_in Integer, length of burn in phase in order to get a stationary 
#' distribution. Netowrk starts empty
#' @param p_12 Numeric, in [0,1], routing probability from node 1 to node 2
#' @param p_21 Numeric, in [0,1], routing probability from node 2 to node 1
#' @param lambda_1 Numeric (> 0), (Poisson) arrival rate at node 1
#' @param lambda_2 Numeric (> 0), (Poisson) arrival rate at node 2
#' @param G_1 Numeric (in [0,1]), parameter steering the (Geometric) service 
#' time distribution at node 1
#' @param G_2 Numeric (in [0,1]), parameter steering the (Geometric) service 
#' time distribution at node 2
#' @return Data Frame with 4 columns (number of arrivals and departures) with 
#' n_obs + burn_in rows, each representing one time slot.
#' @export

queueing_network_poi_geom <- 
  function(n_obs,
           burn_in,
           p_12,
           p_21,
           lambda_1,
           lambda_2,
           G_1,
           G_2){
    
    ### Initiate the observations with 0
    observable_data <- 
      data.frame(arrival_1 = rep(0, n_obs + burn_in),
                 arrival_2 = rep(0, n_obs + burn_in),
                 departure_1 = rep(0, n_obs + burn_in),
                 departure_2 = rep(0, n_obs + burn_in))
    
    ### Loop over each time point:
    ### 
    progressbar <- txtProgressBar(style = 3)
    for(time in c(1:(n_obs + burn_in))){
      
      arrival_1 = rpois(1, lambda = lambda_1)
      arrival_2 = rpois(1, lambda = lambda_2)
      
      if(arrival_1 > 0){
        list_arrivals_1 = list_arrival(1, arrival_1, p_12 = p_12, p_21 = p_21, G_1 = G_1, G_2 = G_2)
        
        customers_arrival_1 = customers_arrival(1, list_arrivals_1)
        
        ### Departure Streams are appended with Departures resulting from new Arrivals
        departures_enter_1_leave_1 = 
          fill_vector_with_zeros(vector = rowSums(customers_arrival_1 == 1, na.rm = TRUE),
                                 lead = time,
                                 length = n_obs + burn_in)
        
        departures_enter_1_leave_2 = 
          fill_vector_with_zeros(vector = rowSums(customers_arrival_1 == 2, na.rm = TRUE),
                                 lead = time,
                                 length = n_obs + burn_in)
        
        observable_data$departure_1 <- 
          observable_data$departure_1 + departures_enter_1_leave_1
        
        observable_data$departure_2 <- 
          observable_data$departure_2 + departures_enter_1_leave_2
      }
      if(arrival_2 > 0){
        list_arrivals_2 = list_arrival(2, arrival_2, p_12 = p_12, p_21 = p_21, G_1 = G_1, G_2 = G_2)
        
        customers_arrival_2 = customers_arrival(2, list_arrivals_2)
        
        departures_enter_2_leave_1 = 
          fill_vector_with_zeros(vector = rowSums(customers_arrival_2 == 1, na.rm = TRUE),
                                 lead = time,
                                 length = n_obs + burn_in)
        
        departures_enter_2_leave_2 = 
          fill_vector_with_zeros(vector = rowSums(customers_arrival_2 == 2, na.rm = TRUE),
                                 lead = time,
                                 length = n_obs + burn_in)
        
        observable_data$departure_1 <- 
          observable_data$departure_1 + departures_enter_2_leave_1
        
        observable_data$departure_2 <- 
          observable_data$departure_2 + departures_enter_2_leave_2
      }
      
      observable_data$arrival_1[time] <-
        observable_data$arrival_1[time] + arrival_1
      
      observable_data$arrival_2[time] <-
        observable_data$arrival_2[time] + arrival_2
      
      setTxtProgressBar(progressbar, time/(n_obs + burn_in))
    }
    
    
    return(observable_data)
  }

fill_vector_with_zeros <- function(vector, lead, length){
  if((length - length(vector) - lead) > 0){
    result =
      c(rep(0, lead), 
        vector,
        rep(0, (length - length(vector) - lead)))
  }
  else {
    result = c(rep(0, lead), vector)
    result = head(result, length)
  }
  return(
    suppressWarnings(
      as.numeric(result)
    ))
}

simulate_single_customer <- function(enter_node, p_12, p_21, G_1, G_2){
  if(p_12 < 1 & p_12 >= 0 & p_21 < 1 & p_12 >= 0 & G_1 < 1 & G_1 >= 0 &  G_2 < 1 & G_2 >= 0){
    number_of_steps = rgeom(1, (1-p_12*p_21))
    if(enter_node == 1){
      service_time_par_a = G_1
      service_time_par_b = G_2
    } else {
      service_time_par_a = G_2
      service_time_par_b = G_1
    }
    service_times <- rep(NA, number_of_steps + 1)
    idx <- c(1:(number_of_steps +1))
    service_times_a = rgeom(number_of_steps + 1, service_time_par_a)
    service_times_b = rgeom(number_of_steps + 1, service_time_par_b)
    service_times[idx %% 2 == 0] <- service_times_b[ idx %% 2 == 0] + 1
    service_times[idx %% 2 != 0] <- service_times_a[ idx %% 2 != 0] + 1
    return(service_times) 
  } else {
    message("Probabilities of geometric distribution out of allowed range")
  }
}

customers_arrival <- function(enter_node, list_arrivals){
  if(length(list_arrivals) > 0){
    max_length = max(unlist(lapply(list_arrivals, sum)))
  } else {max_length = 0}
  matrix_arrivals = 
    sapply(list_arrivals, 
           represent_single_customer, 
           simplify = TRUE, 
           enter_node = enter_node,
           max_length = max_length)
  if(max_length == 0){
    matrix_arrivals <- matrix("" , nrow = 0, ncol = 0)
  }
  if(max_length == 1){
    matrix_arrivals <- as.matrix(t(matrix_arrivals))
  }
  return(matrix_arrivals)
}  

list_arrival <- function(enter_node, arrival, p_12, p_21, G_1, G_2){
  return(sapply(rep(enter_node, arrival), simplify = FALSE, simulate_single_customer,
                p_12 = p_12, p_21 = p_21, G_1 = G_1, G_2 = G_2))
}

represent_single_customer <- function(enter_node, simulation, max_length){
  if(enter_node == 1){
    if(length(simulation) %% 2 == 0){
      exit_node = 2
    } else {
      exit_node = 1
    }
  } else {
    if(length(simulation) %% 2 == 0){
      exit_node = 1
    } else {
      exit_node = 2
    }
  }
  represent = rep(NA, max_length)
  if(sum(simulation) <= max_length){
    represent[sum(simulation)] = exit_node
  }
  return(represent)
}