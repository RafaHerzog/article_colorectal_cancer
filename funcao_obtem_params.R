library(dplyr)
library(caret)

source("funcao_gera_amostras.R")

funcao_obtem_params <- function(t1, t2, vetor_p1, vetor_p2, seed, p_cens) {
  # Criando uma função que encontra os parâmetros da distribuição Weibull dados dois tempos de sobrevivência e probs
  find_weibull_params <- function(t1, t2, p1, p2) {
    # Calculando o valor do parâmetro de forma 
    shape <- (log(-log(p1)) - log(-log(p2))) / (log(t1) - log(t2))
    
    # Calculando o valor do parâmetro de escala
    scale <- t1 / (-log(p1))^(1/shape)
    
    return(list(shape = shape, scale = scale))
  }
  
  # Criando uma função para encontrar o valor de theta que satisfaça P(T > C) = p_cens
  find_theta <- function(scale, shape, p_cens) {
    # Função para calcular P(T > C) dado um valor de theta
    P_T_greater_than_C <- function(theta, scale, shape, p_cens) {
      # Verificar se theta é muito pequeno ou muito grande, o que pode causar problemas numéricos
      if (theta <= 0) return(Inf)  # Evitar que theta seja zero ou negativo
      if (theta > 10 * scale) return(0)  # Evitar intervalos muito grandes
      
      # Integrando P(T > c) para c de 0 até theta
      integral_result <- tryCatch({
        integrate(function(c) exp(-(c/scale)^shape), lower = 0, upper = theta)
      }, error = function(e) NULL)
      
      if (is.null(integral_result)) return(NA)  # Se a integração falhar, retornar NA
      
      integral_value <- integral_result$value
      # Dividido por theta, conforme a fórmula
      P <- (1/theta) * integral_value
      return(P - p_cens)  # Queremos que isso seja 0
    }
    
    # Usar uma busca numérica para encontrar theta com um intervalo ajustado
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
  # Definindo os parâmetros das v.a.'s com distribuição Weibull (T0 e T1), das quais geraremos valores
  # Obs.: esses parâmetros são calculados dados dois tempos de sobrevivência e duas probabilidades 
  # de sobrevivência associadas a esses tempos
  
  ## Calculando os parâmetros da distribuição de T0, assumindo que T0 ~ Weibull(shape0, scale0)
  params0 <- find_weibull_params(t1 = t1, t2 = t2, p1 = vetor_p1[1], p2 = vetor_p2[1])
  shape0 <- params0$shape
  scale0 <- params0$scale
  
  ## Calculando os parâmetros da distribuição de T1, assumindo que T1 ~ Weibull(shape1, scale1)
  params1 <- find_weibull_params(t1 = t1, t2 = t2, p1 = vetor_p1[2], p2 = vetor_p2[2])
  shape1 <- params1$shape
  scale1 <- params1$scale
  
  # Calculando os valores de theta0 e theta1, parâmetros das variáveis com distribuição
  # uniforme que utilizaremos para simular a censura
  theta0 <- find_theta(scale = scale0, shape = shape0, p_cens = p_cens)
  theta1 <- find_theta(scale = scale1, shape = shape1, p_cens = p_cens)
  
  # Retornando uma lista com os parâmetros
  return(
    list(
      params0 = list("scale" = scale0, "shape" = shape0, "theta" = theta0),
      params1 = list("scale" = scale1, "shape" = shape1, "theta" = theta1)
    )
  )
  
}
