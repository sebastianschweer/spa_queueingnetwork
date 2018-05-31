### Main function creating 

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
      
      arrival_1 = 2
      arrival_2 = 0
      
      if(arrival_1 > 0){
      customers_arrival_1 = 
        customers_arrival(arrivals = arrival_1, enter_node = 1, 
                          p_12 = p_12, p_21 = p_21, 
                          G_1 = G_1, G_2 = G_2)
        
      
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
        customers_arrival_2 = 
          customers_arrival(arrivals = arrival_2, enter_node = 2, 
                            p_12 = p_12, p_21 = p_21, 
                            G_1 = G_1, G_2 = G_2)
      
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

### TODO: Else Case nochmal genau betrachten, scheint unplausibel
fill_vector_with_zeros <- function(vector, lead, length){
  if((length - length(vector) - lead) > 0){
  result =
    c(rep(0, lead), 
      vector,
      rep(0, (length - length(vector) - lead)))
  }
  else {
    result = rep(0,length)
  }
  return(result)
  }

#test <- queueing_network_poi_geom(1000, 5000, 0.1, 0.2, 3, 2, 0.7, 0.8)

simulate_single_customer <- function(enter_node, p_12, p_21, G_1, G_2){
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
}


simulate_single_customer(1, 0.9, 0.9, 0.02, 0.8)

customers_arrival <- function(arrivals, enter_node, p_12, p_21, G_1, G_2){
  list_arrivals = sapply(rep(enter_node,arrivals), simplify = FALSE, simulate_single_customer,
                         p_12 = p_12, p_21 = p_21, G_1 = G_1, G_2 = G_2)
  max_length = max(unlist(lapply(list_arrivals, sum)))
  matrix_arrivals = 
    sapply(list_arrivals, 
           represent_single_customer, 
           simplify = TRUE, 
           enter_node = enter_node,
           max_length = max_length)
  if(max_length == 1){
    matrix_arrivals <- as.matrix(t(matrix_arrivals))
  }
  return(matrix_arrivals)
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
  represent[sum(simulation)] = exit_node
  return(represent)
}

test <- queueing_network_poi_geom(100,500,p_12 = 0.9,
                                  p_21 = 0.1,
                                  G_1 = 0.1,
                                  G_2 = 0.1,
                                  lambda_1 = 12,
                                  lambda_2 = 1)
customers_arrival(arrivals = 2,
                  enter_node = 1,
                  p_12 = 0.9,
                  p_21 = 0.1,
                  G_1 = 0.1,
                  G_2 = 0.1)