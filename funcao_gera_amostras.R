library(dplyr)
library(caret)

# Criando uma função para gerar valores da distribuição Weibull com censura
funcao_gera_amostras <- function(params0, params1, p_cens, n, prop_n0, prop_n1, seed) {
  # Obtendo os valores dos parâmetros das distribuições de T1 e T2
  shape0 <- params0$shape
  scale0 <- params0$scale
  theta0 <- params0$theta
  
  shape1 <- params1$shape
  scale1 <- params1$scale
  theta1 <- params1$theta

  # Gerando n0 observações de T0 e C0
  set.seed(seed)
  amostra_t0 <- rweibull(prop_n0*n, shape = shape0, scale = scale0)
  amostra_c0 <- runif(prop_n0*n, 0, theta0)
  
  # Gerando n1 observações de T1 e C1
  set.seed(seed)
  amostra_t1 <- rweibull(prop_n1*n, shape = shape1, scale = scale1)
  amostra_c1 <- runif(prop_n1*n, 0, theta1)
  
  # Organizando as amostras em um dataframe e criando variáveis com o tempo observado e a indicação de falha
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
  
  # Embaralhando as linhas do dataframe gerado
  df_amostras_aux2 <- df_amostras_aux1[sample(1:nrow(df_amostras_aux1)), ]
  row.names(df_amostras_aux2) <- 1:nrow(df_amostras_aux2)
  
  # Retirando as colunas com o tempo teórico e o tempo de censura do dataframe
  df_amostras <- df_amostras_aux2 |> select(!c(t, c))
  
  # Retornando o dataframe de amostras
  return(
    df_amostras
  )
}

