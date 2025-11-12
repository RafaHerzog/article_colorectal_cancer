# Creating a list of required packages
packages <- c(
  "dplyr", "janitor", "survival", "survminer", "gridExtra",
  "randomForestSRC", "caret", "survex", "ranger", "tuneRanger", 
  "fastshap", "doParallel", "shapviz", "treeshap"
)

# Installing missing packages and importing them
for (pkg in packages) {
  if (!suppressPackageStartupMessages(require(pkg, character.only = TRUE))) {
    install.packages(pkg, dependencies = TRUE)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

rm(pkg, packages)

# Preparing the data ---------------------------------------------------------
## Reading the data, converting variables to factors, and selecting the desired variables
data_aux <- read.csv("dataset_clean.csv") |>
  clean_names() |>
  mutate(
    sexo = factor(case_when(
      sexo == 1 ~ "masc",
      sexo == 2 ~ "fem"
    ), levels = c("masc", "fem")),
    cateatend = factor(case_when(
      cateatend == 1 | cateatend == 3 ~ "convenio_ou_particular",
      cateatend == 2 ~ "sus",
      cateatend == 9 ~ "sem_informacao"
    ), levels = c("convenio_ou_particular", "sus", "sem_informacao")),
    diagprev = factor(case_when(
      diagprev == 1 ~ "sem_diag_e_sem_trat",
      diagprev == 2 ~ "com_diag_e_sem_trat"
    ), levels = c("sem_diag_e_sem_trat", "com_diag_e_sem_trat")),
    ecgrup = factor(ecgrup),
    cirurgia = factor(case_when(
      cirurgia == 0 ~ "nao",
      cirurgia == 1 ~ "sim"
    ), levels = c("nao", "sim")),
    hormonio = factor(case_when(
      hormonio == 0 ~ "nao",
      hormonio == 1 ~ "sim"
    ), levels = c("nao", "sim")),
    quimio = factor(case_when(
      quimio == 0 ~ "nao",
      quimio == 1 ~ "sim"
    ), levels = c("nao", "sim")),
    radio = factor(case_when(
      radio == 0 ~ "nao",
      radio == 1 ~ "sim"
    ), levels = c("nao", "sim")),
    outros = factor(case_when(
      outros == 0 ~ "nao",
      outros == 1 ~ "sim"
    ), levels = c("nao", "sim")),
    recorrencia = factor(case_when(
      recnenhum == 1 ~ "nao",
      recnenhum == 0 ~ "sim"
    ), levels = c("nao", "sim")),
    escolari_2 = factor(case_when(
      escolari_2 == 1 ~ "analfabeto",
      escolari_2 == 2 ~ "ens_fund_incompleto",
      escolari_2 == 3 ~ "ens_fund_completo",
      escolari_2 == 4 ~ "ens_medio",
      escolari_2 == 5 ~ "ens_superior"
    ), levels = c("analfabeto", "ens_fund_incompleto", "ens_fund_completo", "ens_medio", "ens_superior")),
    idade_cat = factor(case_when(
      idade <= 49 ~ "0_a_49_anos",
      idade >= 50 & idade <= 74 ~ "50_a_74_anos",
      idade >= 75 ~ "75_anos_mais"
    ), levels = c("0_a_49_anos", "50_a_74_anos", "75_anos_mais")),
    tratcons_cat = factor(case_when(
      tratcons <= 60 ~ "ate_60_dias",
      tratcons > 60 ~ "mais_de_60_dias"
    ), levels = c("ate_60_dias", "mais_de_60_dias"))
  ) |>
  select(
    time_years, falha = status_cancer_specific, anodiag, cateatend, cirurgia,
    diagprev, diagtrat, ecgrup, escolari_2, hormonio, idade_cat, outros,
    quimio, radio, recorrencia, sexo, tratcons_cat
  )

## Finding optimal cut-off points for anodiag and diagtrat using the maxstat package
cutpoints <- surv_cutpoint(
  data_aux,
  time = "time_years",
  event = "falha",
  c("anodiag", "diagtrat"),
  minprop = 0.2
)

cutpoints

## Adding the new categorizations to the original dataframe
data <- data_aux |>
  mutate(
    anodiag_cat = factor(
      ifelse(anodiag <= 2006, "ate_2006", "apos_2006"),
      levels = c("ate_2006", "apos_2006")
    ),
    .after = "anodiag"
  ) |>
  mutate( 
    diagtrat_cat = factor(
      ifelse(diagtrat <= 81, "ate_81_dias", "mais_de_81_dias"),
      levels = c("ate_81_dias", "mais_de_81_dias")
    ),
    .after = "diagtrat"
  ) |>
  select(!c(anodiag, diagtrat)) |>
  mutate(
    anodiag_cat = relevel(anodiag_cat, "apos_2006"),
    cateatend = relevel(cateatend, "convenio_ou_particular"),
    cirurgia = relevel(cirurgia, "sim"),
    diagprev = relevel(diagprev, "com_diag_e_sem_trat"),
    diagtrat_cat = relevel(diagtrat_cat, "ate_81_dias"),
    ecgrup = relevel(ecgrup, "I"),
    escolari_2 = relevel(escolari_2, "ens_superior"),
    hormonio = relevel(hormonio, "sim"),
    idade_cat = relevel(idade_cat, "0_a_49_anos"),
    outros = relevel(outros, "sim"),
    quimio = relevel(quimio, "nao"),
    radio = relevel(radio, "nao"),
    recorrencia = relevel(recorrencia, "nao"),
    sexo = relevel(sexo, "fem"),
    tratcons_cat = relevel(tratcons_cat, "mais_de_60_dias")
  ) 

rm(cutpoints, data_aux)

## Transforming all categorical variables into dummies
data_dummies <- cbind(
  data |> select(time_years, falha),
  as.data.frame(model.matrix(~., data = data |> select(!c(time_years, falha)) |> rename_all(~ paste0(., "_")))) |> select(!`(Intercept)`)
)

## Creating training and test samples
set.seed(428)
linhas_train <- createDataPartition(
  data_dummies$falha,
  p = 0.7, 
  list = FALSE,
  times = 1
)
data_train <- data_dummies[linhas_train, ]
data_test <- data_dummies[-linhas_train, ]

prop.table(table(data_train$falha))
prop.table(table(data_test$falha))

## Importing auxiliary functions
source("auxiliary_modelling_functions.R")

# Choosing among the different splitrules -----------------------------------
## Creating a vector with the names of the splitrules
splitrules <- c("logrank", "bs.gradient", "logrankscore", "C", "maxstat", "extratrees")

## Tuning the hyperparameters of the models ---------------------------------
### Creating a helper function to tune and save results
tune_save <- function(idx, splitrules, data, path = "r_objects/") {
  splitrule <- splitrules[idx]
  set.seed(428)
  
  # Selecting the tuning function according to the splitrule
  fit_tune <- if (splitrule %in% c("logrank", "bs.gradient", "logrankscore")) {
    tune(Surv(time_years, falha) ~ ., data = data, doBest = TRUE, splitrule = splitrule, trace = TRUE)
  } else {
    tune_ranger(Surv(time_years, falha) ~ ., data = data, splitrule = splitrule)
  }
  
  # Printing the results
  if (splitrule %in% c("logrank", "bs.gradient", "logrankscore")) {
    print(fit_tune$optimal)
    print(fit_tune$rf)
  } else {
    print(fit_tune)
  }
  
  # Saving the object
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  saveRDS(fit_tune, file = paste0(path, "fit_tune_split", idx, ".RDS"))
  
  invisible(fit_tune)
}

### Performing tuning with all splitrules
fits <- lapply(seq_along(splitrules), tune_save, splitrules = splitrules, data = data_train)


## Fitting the models with the selected hyperparameters ---------------------
### Creating a helper function to fit the model and save it
fit_save_model <- function(idx, splitrules, data, path = "r_objects/") {
  splitrule <- splitrules[idx]
  
  # Reading the object with the tuning results
  tune_file <- paste0(path, "fit_tune_split", idx, ".RDS")
  fit_tune <- readRDS(tune_file)
  
  set.seed(428)
  
  # Fitting the model
  fit <- if (splitrule %in% c("logrank", "bs.gradient", "logrankscore")) {
    rfsrc(
      Surv(time_years, falha) ~ ., data = data,
      mtry = fit_tune$optimal[2],
      nodesize = fit_tune$optimal[1],
      splitrule = ifelse(splitrule == "logrank", "logrank", splitrule),
      ntime = NULL,
      save.memory = TRUE
    )
  } else {
    ranger(
      Surv(time_years, falha) ~ ., data = data,
      mtry = fit_tune$optimal[2],
      min.node.size = fit_tune$optimal[1],
      replace = FALSE,
      num.threads = 8,
      splitrule = splitrule,
      importance = ifelse(splitrule == "C", "permutation", NULL),
      respect.unordered.factors = FALSE,
      save.memory = TRUE
    )
  }
  
  print(fit)
  
  # Saving the object
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  saveRDS(fit, paste0(path, "fit_split", idx, ".RDS"))
  
  # Performing cleanup
  rm(fit, fit_tune)
  gc()
  
  invisible(NULL)
}

### Fitting with all splitrules
lapply(seq_along(splitrules), fit_save_model, splitrules = splitrules, data = data_train)


### Table with the results -------------------------------------------------
fit_split1 <- readRDS("r_objects/fit_split1.RDS")  # logrank
fit_split2 <- readRDS("r_objects/fit_split2.RDS")  # bs.gradient
fit_split3 <- readRDS("r_objects/fit_split3.RDS")  # logrank-score
fit_split4 <- readRDS("r_objects/fit_split4.RDS")  # C
fit_split5 <- readRDS("r_objects/fit_split5.RDS")  # maxstat
fit_split6 <- readRDS("r_objects/fit_split6.RDS")  # extratrees

df_performance_train_splitrules <- data.frame(
  splitrule = c("logrank", "bs.gradient", "logrank-score", "C", "maxstat", "extratrees"),
  error = c(
    get.cindex(data_train$time_years, data_train$falha, rowSums(fit_split1$chf.oob)),
    get.cindex(data_train$time_years, data_train$falha, rowSums(fit_split2$chf.oob)),
    get.cindex(data_train$time_years, data_train$falha, rowSums(fit_split3$chf.oob)),
    get.cindex(data_train$time_years, data_train$falha, rowSums(fit_split4$chf)),
    get.cindex(data_train$time_years, data_train$falha, rowSums(fit_split5$chf)),
    get.cindex(data_train$time_years, data_train$falha, rowSums(fit_split6$chf))
  ),
  ibs = c(
    ibsRSF(fit_split1),
    ibsRSF(fit_split2),
    ibsRSF(fit_split3),
    ibsRSF(fit_split4, package = "ranger", data = data_train),
    ibsRSF(fit_split5, package = "ranger", data = data_train),
    ibsRSF(fit_split6, package = "ranger", data = data_train)
  )
)
df_performance_train_splitrules

saveRDS(df_performance_train_splitrules, "r_objects/df_performance_train_splitrules.RDS")

rm(list = ls()[startsWith(ls(), "fit_split")])
gc()

### Choice: splitrule C (lowest prediction error) 


# Variable selection ---------------------------------------------------------
## Reading the object containing the fitted model 
fit_split4 <- readRDS("r_objects/fit_split4.RDS") 
names(fit_split4$variable.importance)

## Correcting the variable names
names(fit_split4$variable.importance) <- c(
  "Year of diagnosis: ≤ 2006",
  "Diagnostic care category: Public Healthcare",
  "Diagnostic care category: No information",
  "Surgery: No",
  "Previous diagnosis/treatment: No/No",
  "Time between treatment and diagnosis: > 81 days",
  "Clinical staging group - II: Yes",
  "Clinical staging group - III: Yes",
  "Clinical staging group - IV: Yes",
  "Education level: Illiterate",
  "Education level: Incomplete Primary Education",
  "Education level: Complete Primary Education",
  "Education level: High School",
  "Hormone therapy: No",
  "Age group: 50 to 74 years",
  "Age group: ≥ 75 years",
  "Other treatment: No",
  "Chemotherapy: Yes",
  "Radiotherapy: Yes",
  "Recurrence: Yes",
  "Sex: Male",
  "Time between treatment and consultation: ≤ 60 days"
)

## Creating a dataframe with the VIMP of each variable
df_vimps <- data.frame(
  var = names(sort(fit_split4$variable.importance, decreasing = FALSE)),
  vimp = sort(as.numeric(fit_split4$variable.importance), decreasing = FALSE)
)
df_vimps$var <- factor(df_vimps$var, levels = df_vimps$var)

## Plotting the model's VIMPs
ggplot(df_vimps, aes(y = var)) +
  geom_segment(aes(x = 0, xend = vimp, yend = var), color = "blue", linewidth = 1) + 
  labs(x = "Variable Importance (VIMP)", y = "Predictor") +
  theme_bw() +
  theme(
    legend.position = "top",
    text = element_text(size = 14, color = "black"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),  # <- rotação aqui
    strip.text = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

ggsave(filename = "figures/vimps_complete_model.jpg", width = 10, height = 6, dpi = 500)


## Fitting the models by removing one variable at a time ----------------------
variaveis <- c("completo", names(data |> select(!c(time_years, falha))))
df_performance_variables <- data.frame()
fit_tune_split4 <- readRDS("r_objects/fit_tune_split4.RDS")

for (variavel in variaveis) {
  set.seed(428)
  fit_sem_variaveis <- ranger(
    Surv(time_years, falha) ~ .,  data = data_train |> select(!starts_with(variavel)), 
    mtry = fit_tune_split4$optimal[2], min.node.size = fit_tune_split4$optimal[1],
    replace = FALSE, num.threads = 16, splitrule = "C",
    respect.unordered.factors = FALSE, save.memory = FALSE
  )
  fit_sem_variaveis

  df_performance_variables <- bind_rows(
    df_performance_variables,
    data.frame(
      variavel = ifelse(variavel == "completo", "Complete", paste("Without", variavel)),
      error = get.cindex(data_train$time_years, data_train$falha, rowSums(fit_sem_variaveis$chf)),
      ibs = ibsRSF(fit_sem_variaveis, package = "ranger", data = data_train |> select(!starts_with(variavel)))
    )
  )
  
  saveRDS(df_performance_variables, "r_objects/df_performance_variables.RDS")
  
  rm(fit_sem_variaveis)
  gc()
}
df_performance_variables |> 
  rename(model = variavel) |>
  mutate(
    error = round(error, 3),
    ibs = round(ibs, 3)
  )


## Tuning the hyperparameters of the reduced model ---------------------------
### Creating reduced train and test sets
data_train_final <- data_train |> 
  select(
    !c(
      starts_with("anodiag"), starts_with("sexo"), 
      starts_with("hormonio"), starts_with("tratcons"), 
      starts_with("diagtrat"))
    )

data_test_final <- data_test |> 
  select(
    !c(
      starts_with("anodiag"), starts_with("sexo"), 
      starts_with("hormonio"), starts_with("tratcons"), 
      starts_with("diagtrat"))
  )

### Tuning the hyperparameters of the reduced model
set.seed(428)
fit_tune_final <- tune_ranger(
  Surv(time_years, falha) ~ ., 
  data = data_train_final, 
  splitrule = "C"
)
fit_tune_final
saveRDS(fit_tune_final, "r_objects/fit_tune_final.RDS")


## Fitting the reduced model ------------------------------------------------
fit_tune_final <- readRDS("r_objects/fit_tune_final.RDS")
set.seed(428)
fit_final <- ranger(
  Surv(time_years, falha) ~ .,
  data = data_train_final, 
  mtry = fit_tune_final$optimal[2], min.node.size = fit_tune_final$optimal[1],
  replace = FALSE, num.threads = parallel::detectCores(), splitrule = "C",
  respect.unordered.factors = FALSE, importance = "permutation"
)
fit_final

saveRDS(fit_final, "r_objects/fit_final.RDS")
rm(list = ls()[endsWith(ls(), "fit_final")])
gc()


## Evaluating the predictive performance---------------------------------------
set.seed(428)
pred_fit_final <- predict(
  readRDS("r_objects/fit_final.RDS"), 
  data_test_final,
  num.threads = parallel::detectCores(), save.memory = TRUE
)

saveRDS(pred_fit_final, "r_objects/pred_fit_final.RDS")
rm(pred_fit_final)
gc()

## Table with the results --------------------------------------------------
fit_split4 <- readRDS("r_objects/fit_split4.RDS")
pred_fit_split4 <- readRDS("r_objects/pred_fit_split4.RDS")

fit_final <- readRDS("r_objects/fit_final.RDS")
pred_fit_final <- readRDS("r_objects/pred_fit_final.RDS")

df_performance_final_model <- data.frame(
  modelo = c("Complete", "Reduced (final)"),
  error_train = c(
    get.cindex(data_train$time_years, data_train$falha, rowSums(fit_split4$chf)),
    get.cindex(data_train_final$time_years, data_train_final$falha, rowSums(fit_final$chf))
  ),
  ibs_train = c(
    ibsRSF(fit_split4, package = "ranger", data = data_train),
    ibsRSF(fit_final, package = "ranger", data = data_train_final)
  ),
  error_test = c(
    get.cindex(data_test$time_years, data_test$falha, rowSums(pred_fit_split4$chf)),
    get.cindex(data_test_final$time_years, data_test_final$falha, rowSums(pred_fit_final$chf))
  ),
  ibs_test = c(
    ibsRSF(pred_fit_split4, package = "ranger", data = data_test),
    ibsRSF(pred_fit_final, package = "ranger", data = data_test_final)
  )
)
df_performance_final_model

saveRDS(df_performance_final_model, "r_objects/df_performance_final_model.RDS")

rm(list = ls()[endsWith(ls(), "fit_final")])
gc()


# Calculating SHAP values ---------------------------------------------------
## Unifying the ranger model with the training data 
unified_model <- ranger_surv.unify(
  readRDS("r_objects/fit_final.RDS"),
  data_train_final |>
    select(!c(
      1, 2
    ))
)
gc()

## Computing SHAP values using the TreeSHAP algorithm on the test set
set.seed(428)
shap_treeshap_final <- treeshap(
  unified_model,
  data_test_final
)
saveRDS(shap_treeshap_final, "r_objects/shap_treeshap_final.RDS")

## Converting to a shapviz object for visualization
shp <- shapviz(readRDS("r_objects/shap_treeshap_final.RDS"))

## Renaming features for clearer axis labels in the plots
dimnames(shp)[[2]] <- c(
  "Diagnostic care category: Public Healthcare",
  "Diagnostic care category: No information",
  "Surgery: No",
  "Previous diagnosis/treatment: No/No",
  "Clinical staging group - II: Yes",
  "Clinical staging group - III: Yes",
  "Clinical staging group - IV: Yes",
  "Education level: Illiterate",
  "Education level: Incomplete Primary Education",
  "Education level: Complete Primary Education",
  "Education level: High School",
  "Age group: 50 to 74 years",
  "Age group: ≥ 75 years",
  "Other treatment: No",
  "Chemotherapy: Yes",
  "Radiotherapy: Yes",
  "Recurrence: Yes"
)

## Creating a beeswarm plot of SHAP feature importance
plot_shap <- sv_importance(shp, kind = "beeswarm", max_display = 16) +
  ggplot2::scale_colour_gradient(
    low = "#0089fa", high = "#ff0053",
    breaks = c(0, 1), labels = c("Low", "High")
  ) +
  ggplot2::theme_bw(base_size = 14) +
  theme(
    text = element_text(size = 14, color = "black"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(color = "black"), 
    strip.text = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )
plot_shap

ggsave(plot = plot_shap, filename = "figures/shapvalues_final_model.jpg", width = 10, height = 6, dpi = 300)
