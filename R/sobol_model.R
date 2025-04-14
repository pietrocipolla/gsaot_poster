# Main function to generate Sobol function outputs
sobol_fun <- function(N, a = c(0, 1, 4.5, 9, 99, 99, 99, 99)) {
  # Generate input samples using Latin Hypercube Sampling
  x <- sobol_fun_inputs(N)
  
  # Compute the Sobol function output
  y <- sobol_fun_model(x, a)
  
  # Return the inputs and corresponding outputs as a list
  return(list(x = x, y = y))
}

# Function to generate input samples using Latin Hypercube Sampling
sobol_fun_inputs <- function(N) {
  # Define the range of inputs for 8 dimensions (each between 0 and 1)
  ranges <- matrix(rep(c(0, 1), each = 8), ncol = 2)
  
  # Generate N input samples using Latin Hypercube Sampling from the FME package
  x <- FME::Latinhyper(ranges, N)
  
  # Assign column names
  colnames(x) <- paste0("X", 1:8)
  
  return(x)
}

# Function to compute the Sobol function model output
sobol_fun_model <- function(x, a) {
  # Initialize the output variable y
  y <- 1
  
  # Loop over the 8 input dimensions
  for (j in 1:8) {
    # Compute the product of each term in the Sobol function
    y <- y * (abs(4 * x[, j] - 2) + a[j]) / (1 + a[j])
  }
  
  # Return the computed response
  return(y)
}