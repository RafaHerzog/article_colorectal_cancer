library(dplyr)
library(caret)

source("generate_samples_function.R")

get_params_function <- function(t1, t2, vetor_p1, vetor_p2, seed, p_cens) {
  # Creating a function that finds the parameters of the Weibull distribution given two survival times and their probabilities
  find_weibull_params <- function(t1, t2, p1, p2) {
    # Calculating the value of the shape parameter
    shape <- (log(-log(p1)) - log(-log(p2))) / (log(t1) - log(t2))
    
    # Calculating the value of the scale parameter
    scale <- t1 / (-log(p1))^(1/shape)
    
    return(list(shape = shape, scale = scale))
  }
  
  # Creating a function to find the value of theta that satisfies P(T > C) = p_cens
  find_theta <- function(scale, shape, p_cens) {
    # Function to calculate P(T > C) given a value of theta
    P_T_greater_than_C <- function(theta, scale, shape, p_cens) {
      # Checking whether theta is too small or too large, which may cause numerical issues
      if (theta <= 0) return(Inf)  # Preventing theta from being zero or negative
      if (theta > 10 * scale) return(0)  # Avoiding excessively large intervals
      
      # Integrating P(T > c) for c from 0 to theta
      integral_result <- tryCatch({
        integrate(function(c) exp(-(c/scale)^shape), lower = 0, upper = theta)
      }, error = function(e) NULL)
      
      if (is.null(integral_result)) return(NA)  # If the integration fails, return NA
      
      integral_value <- integral_result$value
      # Divided by theta
      P <- (1/theta) * integral_value
      return(P - p_cens)  # We want this to be 0
    }
    
    # Using a numerical search to find theta within an adjusted interval
    result <- tryCatch({
      uniroot(P_T_greater_than_C, c(0, 10*scale), scale = scale, shape = shape, p_cens = p_cens)
    }, error = function(e) NULL)
    
    if (is.null(result)) {
      cat("Erro ao encontrar a raiz para theta.\n")
      return(NULL)
    }
    
    return(result$root)
  }
  
  set.seed(seed)
  # Defining the parameters of the Weibull random variables (T0 and T1) from which values will be generated
  # Note: these parameters are calculated given two survival times and the corresponding
  # survival probabilities associated with those times
  
  ## Calculating the parameters of T0's distribution, assuming that T0 ~ Weibull(shape0, scale0)
  params0 <- find_weibull_params(t1 = t1, t2 = t2, p1 = vetor_p1[1], p2 = vetor_p2[1])
  shape0 <- params0$shape
  scale0 <- params0$scale
  
  ## Calculating the parameters of T1's distribution, assuming that T1 ~ Weibull(shape1, scale1)
  params1 <- find_weibull_params(t1 = t1, t2 = t2, p1 = vetor_p1[2], p2 = vetor_p2[2])
  shape1 <- params1$shape
  scale1 <- params1$scale
  
  # Calculating the values of theta0 and theta1, parameters of the uniformly 
  # distributed variables that will be used to simulate censoring
  theta0 <- find_theta(scale = scale0, shape = shape0, p_cens = p_cens)
  theta1 <- find_theta(scale = scale1, shape = shape1, p_cens = p_cens)
  
  # Returning a list with the parameters
  return(
    list(
      params0 = list("scale" = scale0, "shape" = shape0, "theta" = theta0),
      params1 = list("scale" = scale1, "shape" = shape1, "theta" = theta1)
    )
  )
  
}
