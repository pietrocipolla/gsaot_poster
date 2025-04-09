lotka_volterra_fun <- function(N, T = 20, dt = 0.1, return_type = "all") {
  x <- lotka_volterra_input(N)
  y <- lotka_volterra_model(x, T, dt, return_type)
  
  return(list(x = x, y = y))
}

lotka_volterra_input <- function(N) {
  # Ranges: prey0, pred0, alpha, beta, delta, gamma
  ranges <- matrix(c(
    10, 30,   # Prey initial population
    5, 15,    # Predator initial population
    0.5, 1.5, # Alpha (prey growth)
    0.2, 0.6, # Beta (prey death rate due to predation)
    0.05, 0.15, # Delta (predator growth rate per prey)
    0.3, 0.5   # Gamma (predator death rate)
  ), ncol = 2, byrow = TRUE)
  
  x <- FME::Latinhyper(ranges, N)
  colnames(x) <- c("prey0", "pred0", "alpha", "beta", "delta", "gamma")
  
  return(x)
}

lotka_volterra_model <- function(x, T, dt, return_type = "all") {
  steps <- floor(T / dt)
  N <- nrow(x)
  
  # Initialize arrays to store full time series
  prey_matrix <- matrix(0, nrow = N, ncol = steps + 1)
  pred_matrix <- matrix(0, nrow = N, ncol = steps + 1)
  
  for (i in 1:N) {
    prey <- numeric(steps + 1)
    pred <- numeric(steps + 1)
    
    prey[1] <- x[i, "prey0"]
    pred[1] <- x[i, "pred0"]
    
    alpha <- x[i, "alpha"]
    beta  <- x[i, "beta"]
    delta <- x[i, "delta"]
    gamma <- x[i, "gamma"]
    
    for (t in 1:steps) {
      dx <- alpha * prey[t] - beta * prey[t] * pred[t]
      dy <- delta * prey[t] * pred[t] - gamma * pred[t]
      
      prey[t + 1] <- prey[t] + dt * dx
      pred[t + 1] <- pred[t] + dt * dy
    }
    
    prey_matrix[i, ] <- prey
    pred_matrix[i, ] <- pred
  }
  
  # Return based on user selection
  if (return_type == "prey") {
    return(prey_matrix)
  } else if (return_type == "predator") {
    return(pred_matrix)
  } else if (return_type == "all") {
    return(cbind(prey_matrix, pred_matrix))
  } else {
    stop("Invalid return_type. Use 'prey', 'predator', or 'all'.")
  }
}
