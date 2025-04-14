# Main function to generate input samples and compute model outputs
gaussian_fun <- function(N) {
  # Generate correlated input samples
  x <- gaussian_inputs(N)
  
  # Compute the model output
  y <- gaussian_model(x)
  
  # Return the inputs and corresponding outputs as a list
  return(list(x = x, y = y))
}

# Function to generate correlated input samples
gaussian_inputs <- function(N) {
  # Define the mean vector for the multivariate normal distribution
  mx <- c(1, 1, 1)
  
  # Define the covariance matrix
  Sigmax <- matrix(data = c(1, 0.5, 0.5, 
                            0.5, 1, 0.5, 
                            0.5, 0.5, 1), nrow = 3)
  
  # Sample from standard normal distributions
  x1 <- rnorm(N)
  x2 <- rnorm(N)
  x3 <- rnorm(N)
  
  # Combine the sampled values into a matrix
  x <- cbind(x1, x2, x3)
  
  # Apply Cholesky transformation to induce correlation
  x <- mx + x %*% chol(Sigmax)
  
  # Assign column names
  colnames(x) <- paste0("X", 1:3)
  
  return(x)
}

# Function to compute the model output
gaussian_model <- function(x) {
  # Define the coefficient matrix for the linear transformation
  A <- matrix(data = c(4, -2, 1, 
                       2, 5, -1), nrow = 2, byrow = TRUE)
  
  # Compute the output y = Ax'
  y <- t(A %*% t(x))
  
  # Assign column names to the output
  colnames(y) <- c("Y1", "Y2")
  
  return(y)
}