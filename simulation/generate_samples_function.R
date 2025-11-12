library(dplyr)
library(caret)

# Creating a function to generate values from the Weibull distribution with censoring
generate_samples_function <- function(params0, params1, p_cens, n, prop_n0, prop_n1, seed) {
  # Obtaining the parameter values of the distributions of T1 and T2 
  shape0 <- params0$shape
  scale0 <- params0$scale
  theta0 <- params0$theta
  
  shape1 <- params1$shape
  scale1 <- params1$scale
  theta1 <- params1$theta

  # Generating n0 observations of T0 and C0
  set.seed(seed)
  amostra_t0 <- rweibull(prop_n0*n, shape = shape0, scale = scale0)
  amostra_c0 <- runif(prop_n0*n, 0, theta0)
  
  # Generating n1 observations of T1 and C1
  set.seed(seed)
  amostra_t1 <- rweibull(prop_n1*n, shape = shape1, scale = scale1)
  amostra_c1 <- runif(prop_n1*n, 0, theta1)
  
  # Organizing the samples in a dataframe and creating variables for the observed time and failure indicator
  df_amostras_aux1 <- data.frame(
    t = c(amostra_t0, amostra_t1),
    c = c(amostra_c0, amostra_c1),
    preditor = c(rep(0, times = prop_n0*n), rep(1, times = prop_n1*n))
  ) |>
    mutate(
      t_latente = t,
      t_obs = ifelse(t < c, t, c),
      falha = ifelse(t < c, 1, 0),
      .after = "c"
    ) 
  
  # Shuffling the rows of the generated dataframe
  df_amostras_aux2 <- df_amostras_aux1[sample(1:nrow(df_amostras_aux1)), ]
  row.names(df_amostras_aux2) <- 1:nrow(df_amostras_aux2)
  
  # Removing the columns with theoretical time and censoring time from the dataframe
  df_amostras <- df_amostras_aux2 |> select(!c(t, c))
  
  # Returning the dataframe of samples
  return(
    df_amostras
  )
}

