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
      
      ### TODO: Simulier Ankünfte. Anforderung: Ergebnis als Matrix mit Anzahl Zeilen 
      ### geg. durch Maximal Wert Service Time. Auffüllen mit NAs. Nur Abgangszeitpunkt nicht NA je Spalte
      example_customer_enter_1 = 
        as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2))
      example_customer_enter_2 = 
        as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1))
      example_customer_enter_1 = append(example_customer_enter_1, rep(NA,22))
      example_customer_enter_2 = append(example_customer_enter_2, rep(NA,28))
      max_obs = 40
      customers_arrival_1 =
        matrix(0, nrow = max_obs, ncol = arrival_1)
      customers_arrival_2 =
        matrix(0, nrow = max_obs, ncol = arrival_1)
      customers_arrival_1[ ,1] = example_customer_enter_1
      customers_arrival_1[ ,2] = example_customer_enter_2
      customers_arrival_2[ ,1] = example_customer_enter_2
      customers_arrival_2[ ,2] = example_customer_enter_1
      
      ### Departure Streams are appended with Departures resulting from new Arrivals
      departures_enter_1_leave_1 = 
        fill_vector_with_zeros(vector = rowSums(customers_arrival_1 == 1, na.rm = TRUE),
                               lead = time,
                               length = n_obs + burn_in)
      
      departures_enter_1_leave_2 = 
        fill_vector_with_zeros(vector = rowSums(customers_arrival_1 == 2, na.rm = TRUE),
                               lead = time,
                               length = n_obs + burn_in)
      
      departures_enter_2_leave_1 = 
        fill_vector_with_zeros(vector = rpois(1,0.2)*rowSums(customers_arrival_1 == 2, na.rm = TRUE),
                               lead = time,
                               length = n_obs + burn_in)
      
      departures_enter_2_leave_2 = 
        fill_vector_with_zeros(vector = rpois(1,0.2)*rowSums(customers_arrival_1 == 2, na.rm = TRUE),
                               lead = time,
                               length = n_obs + burn_in)
      
      
      observable_data$arrival_1[time] <-
        observable_data$arrival_1[time] + arrival_1
      
      observable_data$arrival_2[time] <-
        observable_data$arrival_2[time] + arrival_2
      
      observable_data$departure_1 <- 
        observable_data$departure_1 + departures_enter_1_leave_1 + departures_enter_2_leave_1
      
      observable_data$departure_2 <- 
        observable_data$departure_2 + departures_enter_1_leave_2 + departures_enter_2_leave_2
      
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
  service_times[idx %% 2 == 0] <- service_times_b[ idx %% 2 == 0]
  service_times[idx %% 2 != 0] <- service_times_a[ idx %% 2 != 0]
  return(service_times)
}


simulate_single_customer(1, 0.9, 0.9, 0.02, 0.8)

