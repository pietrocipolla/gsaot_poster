# Main function to compute the Bratley function outputs
bratley_fun <- function(N, k = 8) {
  # Generate input samples using Latin Hypercube Sampling
  x <- bratley_input(N, k)
  
  # Compute the Bratley function output
  y <- bratley_model(x, k)
  
  # Return the inputs and corresponding outputs as a list
  return(list(x = x, y = y))
}

# Function to generate input samples using Latin Hypercube Sampling
bratley_input <- function(N, k) {
  # Define the range of inputs for k dimensions (each between 0 and 1)
  ranges <- matrix(rep(c(0, 1), each = k), ncol = 2)
  
  # Generate N input samples using Latin Hypercube Sampling from the FME package
  x <- FME::Latinhyper(ranges, N)
  
  # Assign column names
  colnames(x) <- paste0("X", 1:k)
  
  return(x)
}

# Function to compute the Bratley function model output
bratley_model <- function(x, k) {
  # Initialize the output y as a zero vector
  y <- rep(0, nrow(x))
  
  # Loop over k dimensions to compute the Bratley function
  for (i in 1:k) {
    # Compute the product of x_j terms from j = 1 to i
    product_term <- apply(x[, 1:i, drop = FALSE], 1, prod)
    
    # Add the term (-1)^i * product_term to the output y
    y <- y + (-1)^i * product_term
  }
  
  return(y)
}