library(dplyr)
library(simhelpers)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)
library(tidyr)

dados_preditor0 <- data.frame()
dados_preditor1 <- data.frame()

for (p_cens in c(0.25, 0.4, 0.5, 0.6, 0.7)) {
  dados_aux3_0 <- data.frame()
  dados_aux3_1 <- data.frame()
  
  for (tempo in c(1, 3, 5)) {
    dados_aux2_0 <- data.frame()
    dados_aux2_1 <- data.frame()
    
    for (n in c(500, 1000, 2500, 5000)) {
      dados_aux <- read.csv(glue::glue("results/df_predicoes_{p_cens}_{tempo}_{n}.csv")) |>
        pivot_longer(
          cols = starts_with("predicao"),
          names_to = "modelo",
          values_to = "predicao"
        ) |>
        mutate(
          p_cens = p_cens,
          tempo = tempo,
          n = n,
          modelo = ifelse(modelo == "predicao_xgboost", "XGBoost", "RSF"),
          .before = "preditor"
        ) |>
        group_by(p_cens, n, tempo, preditor, modelo) |>
        summarise(
          vicio = mean(predicao - prob_teorica, na.rm = TRUE),
          eqm = mean((predicao - prob_teorica)^2, na.rm = TRUE)
        ) |>
        ungroup()
      
      dados_aux1_0 <- dados_aux |>
        filter(preditor == 0) 
      
      dados_aux1_1 <- dados_aux |>
        filter(preditor == 1)
      
      dados_aux2_0 <- bind_rows(dados_aux2_0, dados_aux1_0)
      dados_aux2_1 <- bind_rows(dados_aux2_1, dados_aux1_1)
    }
    dados_aux3_0 <- bind_rows(dados_aux3_0, dados_aux2_0)
    dados_aux3_1 <- bind_rows(dados_aux3_1, dados_aux2_1)
  }
  dados_preditor0 <- bind_rows(dados_preditor0, dados_aux3_0)
  dados_preditor1 <- bind_rows(dados_preditor1, dados_aux3_1)
}

dados_completo <- full_join(dados_preditor0, dados_preditor1) |> 
  arrange(p_cens, n, tempo, preditor)

rm(list = ls()[grepl("dados_aux", ls())])


# Bias --------------------------------------------------------------------
cores_personalizadas <- c(
  "RSF (predictor 0)"     = "#ed1f24",  
  "RSF (predictor 1)"     = "#40b749",
  "XGBoost (predictor 0)" = "#3953a5",  
  "XGBoost (predictor 1)" = "#c77cff" 
)

df_vicio <- dados_completo |>
  mutate(
    n = factor(n),
    tempo = paste("Time of interest =", tempo),
    p_cens = paste("Cens. =", p_cens),
    grupo = paste0(modelo, " (predictor ", preditor, ")")
  )

ggplot(df_vicio, aes(
  x = n, 
  y = vicio, 
  group = grupo, 
  color = grupo, 
  linetype = grupo, 
  shape = grupo
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(tempo ~ p_cens) +
  scale_color_manual(values = cores_personalizadas) +
  labs(
    x = "Sample size (n)",
    y = "Bias",
    color = "Model",
    linetype = "Model",
    shape = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    text = element_text(size = 15, color = "black"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),  
    strip.text = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

ggsave(
  "figures/plot_vicio.png", width = 10, height = 8, units = "in", 
  dpi = 500
)


# MSE ---------------------------------------------------------------------
df_eqm <- dados_completo |>
  mutate(
    n = factor(n),
    tempo = paste("Time of interest =", tempo),
    p_cens = paste("Cens. =", p_cens),
    grupo = paste0(modelo, " (predictor ", preditor, ")")
  )

ggplot(df_eqm, aes(
  x = n, 
  y = eqm, 
  group = grupo, 
  color = grupo, 
  linetype = grupo, 
  shape = grupo
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(tempo ~ p_cens) +
  scale_color_manual(values = cores_personalizadas) + 
  labs(
    x = "Sample size (n)",
    y = "MSE",
    color = "Model",
    linetype = "Model",
    shape = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    text = element_text(size = 15, color = "black"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    strip.text = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )


ggsave(
  "figures/plot_eqm.png", width = 10, height = 8, units = "in", 
  dpi = 500
)
