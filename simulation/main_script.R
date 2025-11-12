options(scipen = 999)

# Creating a list of required packages
packages <- c(
  "dplyr", "caret", "survival", "randomForestSRC", "glue"
)

# Installing missing packages and importing them
for (pkg in packages) {
  if (!suppressPackageStartupMessages(require(pkg, character.only = TRUE))) {
    install.packages(pkg, dependencies = TRUE)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

rm(pkg, packages)

# Importing the files with auxiliary functions
source("generate_samples_function.R")
source("tune_xgboost_function.R")
source("get_params_function.R")

simulacao_predicoes_xgboost_rsf <- function(t1, t2, vetor_p1, vetor_p2, seed, vetor_p_cens, vetor_n, vetor_tempos_interesse, M, prop_n0, prop_n1) {

  # Setting the initial seeds
  seed_p_cens <- seed
  seed_tempos <- seed
  seed_amostras <- seed
  
  for (p_cens in vetor_p_cens) {
    # Obtaining the parameters of the Weibull and Uniform distributions for each predictor
    params <- funcao_obtem_params(t1 = t1, t2 = t2, vetor_p1 = vetor_p1, vetor_p2 = vetor_p2, seed = seed_p_cens, p_cens = p_cens)
    params0 <- params$params0
    params1 <- params$params1
    
    # Updating the seed
    seed_p_cens <- seed_p_cens + 1
    
    for (tempo in vetor_tempos_interesse) {
      # Generating an initial sample to train the models' hyperparameters
      df_amostras <- funcao_gera_amostras(
        params0 = params0, params1 = params1,
        p_cens = p_cens,
        n = 10000, prop_n0 = prop_n0, prop_n1 = prop_n1,
        seed = seed_tempos
      )

      # Creating a training set
      set.seed(seed_tempos)
      linhas_treino <- createDataPartition(
        df_amostras$falha,
        p = 0.7, 
        list = FALSE,
        times = 1
      )
      
      df_treino <- df_amostras[linhas_treino, ]
      
      # For training/testing XGBoost, removing observations whose failure time is 
      # censored and shorter than the given time, and creating the response variable
      df_treino_classificacao <- df_treino |>
        filter(!(t_obs < tempo & falha == 0)) |>
        mutate(
          !!paste0("sobrev_", tempo, "ano") := ifelse(t_obs > tempo, 1, 0)
        )
      
      # Ensuring that the data are in the correct format for Python libraries
      X_train_classificacao <- df_treino_classificacao |> dplyr::select(preditor) |> as.matrix()
      y_train_classificacao <- df_treino_classificacao |> pull(starts_with("sobrev")) |> as.integer()
      
      # Tuning the hyperparameters of XGBoost
      print(glue("Tunando os hiperparâmetros do XGBoost para o cenário de p_cens = {p_cens} e tempo de interesse = {tempo}"))
      params_xgboost <- funcao_tune_xgboost(X_train = X_train_classificacao, y_train = y_train_classificacao, seed = as.integer(seed_tempos))
      ## For the RSF, it does not make sense to tune hyperparameters, since there is only one binary predictor
      
      # Updating the seed
      seed_tempos <- seed_tempos + 1
      
      # Starting the simulations
      for (n in vetor_n) {
        
        # Creating a dataframe to store predictions
        df_predicoes <- data.frame()
        
        for (i in 1:M) {
          # Generating a sample of size n with (failure time, failure indicator, predictor)
          df_amostras <- funcao_gera_amostras(
            params0 = params0, params1 = params1,
            p_cens = p_cens,
            n = n, prop_n0 = prop_n0, prop_n1 = prop_n1,
            seed = seed_amostras
          )
          
          # Creating training and test sets
          set.seed(seed_amostras)
          linhas_treino <- createDataPartition(
            df_amostras$falha,
            p = 0.7, 
            list = FALSE,
            times = 1
          )
          
          df_treino <- df_amostras[linhas_treino, ]
          df_teste <- df_amostras[-linhas_treino, ]
          
          # For training/testing XGBoost, removing observations whose failure time is 
          # censored and shorter than the given time, and creating the response variable
          df_treino_classificacao <- df_treino |>
            filter(!(t_obs < tempo & falha == 0)) |>
            mutate(
              !!paste0("sobrev_", tempo, "ano") := ifelse(t_obs > tempo, 1, 0)
            )
          
          df_teste_classificacao <- df_teste |>
            filter(!(t_obs < tempo & falha == 0)) |>
            mutate(
              !!paste0("sobrev_", tempo, "ano") := ifelse(t_obs > tempo, 1, 0)
            )
          
          observacoes_excluidas_teste <- df_teste |>
            mutate(
              index = 1:nrow(df_teste),
              flag_exclusao = ifelse(t_obs < tempo & falha == 0, 1, 0)
            ) |>
            filter(flag_exclusao == 1) |>
            pull(index)
          
          # Ensuring that the data are in the correct format for Python libraries
          X_train_classificacao <- df_treino_classificacao |> dplyr::select(preditor) |> as.matrix()
          y_train_classificacao <- df_treino_classificacao |> pull(starts_with("sobrev")) |> as.integer()
          
          X_test_classificacao <- df_teste_classificacao |> dplyr::select(preditor) |> as.matrix()
          y_test_classificacao <- df_teste_classificacao |> pull(starts_with("sobrev")) |> as.integer()
          
          # Fitting the models
          ## XGBoost
          params_xgboost['random_state'] <- as.integer(seed_amostras)
          params_xgboost['scale_pos_weight'] <- 0.3
          
          fit_xgboost <- xgb$XGBClassifier()
          do.call(fit_xgboost$set_params, params_xgboost)
          
          fit_xgboost$fit(X_train_classificacao, y_train_classificacao) 
          
          ## RSF
          fit_rsf <- rfsrc(
            Surv(t_obs, falha) ~ preditor, data = df_treino, ntime = NULL, save.memory = TRUE
          )
          
          # Making the predictions
          ## For XGBoost (saving only the predictions for response class 1)
          predicoes_xgboost <- fit_xgboost$predict_proba(X_test_classificacao)[, 2]
          
          ## For RSF
          predicoes_rsf <- predict.rfsrc(fit_rsf, df_teste)
          probs_rsf_tempo_interesse <- predicoes_rsf$survival[, which.min(abs(predicoes_rsf$time.interest[which(predicoes_rsf$time.interest <= tempo)] - tempo))]
          
          ## Calculating the theoretical survival probabilities for each group
          prob_teorica0 <- pweibull(tempo, shape = params0$shape, scale = params0$scale, lower.tail = FALSE)
          prob_teorica1 <- pweibull(tempo, shape = params1$shape, scale = params1$scale, lower.tail = FALSE)
          
          ## Creating a dataframe with the estimated and theoretical probabilities for each observation in the test set
          df_predicoes_aux <- data.frame(
            index = 1:nrow(df_teste),
            preditor = df_teste$preditor
          ) |>
            mutate(
              predicao_xgboost = ifelse(index %in% observacoes_excluidas_teste, NA, predicoes_xgboost),
              predicao_rsf = probs_rsf_tempo_interesse,
              prob_teorica = ifelse(preditor == 0, prob_teorica0, prob_teorica1)
            ) |>
            select(!index)
          
          df_predicoes <- bind_rows(df_predicoes, df_predicoes_aux)
          
          print(paste(p_cens, tempo, n, i))
          
          write.csv(df_predicoes, glue("results/df_predicoes_{p_cens}_{tempo}_{n}.csv"), row.names = FALSE)
          
          # Updating the seed
          seed_amostras <- seed_amostras + 1
        }
      }
    }
  }
}


system.time(results <- simulacao_predicoes_xgboost_rsf(
  t1 = 5,
  t2 =  20,
  vetor_p1 = c(0.8319006, 0.7201794),
  vetor_p2 = c(0.7382254, 0.5685534),
  seed = 803,
  vetor_p_cens = c(0.25, 0.4, 0.5, 0.6, 0.7),
  vetor_n = c(500, 1000, 2500, 5000),
  vetor_tempos_interesse = c(1, 3, 5),
  M = 1000,
  prop_n0 = 0.4,
  prop_n1 = 0.6
))


