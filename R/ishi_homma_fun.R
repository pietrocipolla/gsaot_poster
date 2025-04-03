# Main function to compute the Ishigami-Homma function outputs
ishi_homma_fun <- function(N, A = 2, B = 1) {
  # Generate input samples using Latin Hypercube Sampling
  x <- ishi_homma_input(N)
  
  # Compute the Ishigami-Homma function output
  y <- ishi_homma_model(x, A, B)
  
  # Return the inputs and corresponding outputs as a list
  return(list(x = x, y = y))
}

# Function to generate input samples using Latin Hypercube Sampling
ishi_homma_input <- function(N) {
  # Define the range of inputs for 8 dimensions (each between -π and π)
  ranges <- matrix(rep(c(-pi, pi), each = 8), ncol = 2)
  
  # Generate N input samples using Latin Hypercube Sampling from the FME package
  x <- FME::Latinhyper(ranges, N)
  
  return(x)
}

# Function to compute the Ishigami-Homma function model output
ishi_homma_model <- function(x, A, B) {
  # Compute the Ishigami-Homma function for each sample
  y <- sin(x[, 1]) + A * sin(x[, 2]) ^ 2 + B * x[, 3] ^ 4 * sin(x[, 1])
  
  return(y)
}