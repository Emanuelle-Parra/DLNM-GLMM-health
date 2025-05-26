# 1. Libraries  --------------------------------------------------------------

## 1.1 Data manipulation and transformation ----

library(dplyr)       # Data manipulation
library(tidyr)       # Data transformation
library(tidyverse)   # Collection of packages for data analysis

## 1.2 Visualization and graphics ----

library(ggplot2)     # ggplot-style plots
library(ggseas)      # Seasonal component visualization
library(scales)      # Custom scale formatting for plots
library(viridis)     # Perceptually uniform color palettes
library(corrplot)    # Correlation plots
library(GGally)      # Enhancements for ggplot2 (plot matrices)

## 1.3 Statistical analysis and modeling ----

library(car)         # Linear model diagnostics
library(dlnm)        # Distributed lag nonlinear models
library(forecast)    # Time series forecasting
library(glmmTMB)     # Generalized linear mixed models (GLMM)
library(lme4)        # Mixed effects models
library(mgcv)        # Generalized additive models (GAM)
library(gamlss)      # GAMLSS models for zero-inflated data
library(MASS)        # GLM fitting functions
library(splines)     # Cubic splines generation
library(urca)        # Unit root tests for time series
library(tseries)     # Time series analysis tools

## 1.4 Information analysis and metrics ----

library(energy)      # Distance correlation (dCor)
library(infotheo)    # Mutual Information (MI)
library(Metrics)     # Model evaluation metrics

## 1.5 Spatial data handling ----

library(sf)          # Handling spatial data (simple features)

## 1.6 Table creation ----

library(kableExtra)  # Table creation and customization
library(DT)          # Interactive tables


# 2. Data loading ------------------------------------------------------------

## 2.1 Main dataset ----
load(file = "datos_final1_para_analizar.RData")

## 2.2 Spatial data ----
subregion_st <- st_read("Subegiones Climáticas.shp")


# 3. Data generation by region and subregion --------------------------------


#' This code block generates summary data by region and subregion,
#' calculating key weekly statistics for each combination of year,
#' epidemiological week, and subregion.
#' The data includes weekly summaries of discharges, relative risk of infection,
#' population, precipitation, temperature, aerosol concentration, and relative humidity.

## 3.1 Data by subregion and epidemiological week ----
data_ra_subreg <- datos_finales_subregion %>%
  filter(reg_climat == "RA", epi.week != 53) %>%
  group_by(year, epi.week, Subreg_climat) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    Pob = sum(pobl_subreg_),
    logpop = log(sum(pobl_subreg_)),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

## 3.2 Aggregated data by epidemiological week for each region ----

data_region1 <- datos_finales_subregion %>%
  filter(reg_climat == "PC", epi.week != 53) %>%
  group_by(year, epi.week) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    Pob = sum(pobl_subreg_),
    logpop = log(sum(pobl_subreg_)),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

data_region2 <- datos_finales_subregion %>%
  filter(reg_climat == "PN", epi.week != 53) %>%
  group_by(year, epi.week) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    Pob = sum(pobl_subreg_),
    logpop = log(sum(pobl_subreg_)),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

data_region3 <- datos_finales_subregion %>%
  filter(reg_climat == "PS", epi.week != 53) %>%
  group_by(year, epi.week) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    Pob = sum(pobl_subreg_),
    logpop = log(sum(pobl_subreg_)),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

data_region4 <- datos_finales_subregion %>%
  filter(reg_climat == "RMS", epi.week != 53) %>%
  group_by(year, epi.week) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    Pob = sum(pobl_subreg_),
    logpop = log(sum(pobl_subreg_)),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

data_region5 <- datos_finales_subregion %>%
  filter(reg_climat == "VC", epi.week != 53) %>%
  group_by(year, epi.week) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    Pob = sum(pobl_subreg_),
    logpop = log(sum(pobl_subreg_)),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

data_region6 <- datos_finales_subregion %>%
  filter(reg_climat == "RN", epi.week != 53) %>%
  group_by(year, epi.week) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    Pob = sum(pobl_subreg_),
    logpop = log(sum(pobl_subreg_)),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

data_region7 <- datos_finales_subregion %>%
  filter(reg_climat == "RA", epi.week != 53) %>%
  group_by(year, epi.week) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    Pob = sum(pobl_subreg_),
    logpop = log(sum(pobl_subreg_)),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

#' Add a column to identify each region and combine all data into a single dataset

data_region1$region <- "PC"
data_region2$region <- "PN"
data_region3$region <- "PS"
data_region4$region <- "RMS"
data_region5$region <- "VC"
data_region6$region <- "RN"
data_region7$region <- "RA"

data_pc <- data_region1
data_ps <- data_region2
data_pn <- data_region3
data_rms <- data_region4
data_vc <- data_region5
data_rn <- data_region6
data_ra <- data_region7
# 4. Correlational analysis using permutation tests -------------------------

#' This block defines a function to assess the correlation between lagged
#' predictor variables and the response variable in region RA using different methods:
#' - Pearson correlation (linear)
#' - Distance correlation (dCor) (nonlinear)
#' - Mutual information (MI) (nonlinear)
#' Significance is assessed using permutation tests.

#' Define the maximum lag to evaluate


## 4.1 Functions to set crossbasis parameterization ----

#' ------------------------------------------------------------------------------
#' Function: perm_test
#' ------------------------------------------------------------------------------
#' Description:
#' Performs a permutation test to evaluate statistical dependence between two numeric vectors.
#' Supports three methods: Pearson correlation, distance correlation (dCor), and mutual information (MI).
#'
#' Arguments:
#' - x: numeric vector (independent variable).
#' - y: numeric vector (dependent variable).
#' - method: character, specifies the dependence method to use: "pearson" (default), "dcor", or "MI".
#' - n_perm: integer, number of permutations to perform (default = 50).
#'
#' Value:
#' - A numeric value between 0 and 1 representing the p-value of the permutation test.
#'
#' Details:
#' - For "pearson", computes |cor(x, y)| with random permutations of x.
#' - For "dcor", uses the dcor() function from the `energy` package.
#' - For "MI", computes mutual information between discretized variables using mutinformation() from `infotheo`.

perm_test <- function(x, y, method = "pearson", n_perm = 50) {
  if (method == "pearson") {
    obs_stat <- abs(cor(x, y))
    perm_stats <- replicate(n_perm, abs(cor(sample(x), y)))
  } else if (method == "dcor") {
    obs_stat <- dcor(x, y)
    perm_stats <- replicate(n_perm, dcor(sample(x), y))
  } else if (method == "MI") {
    obs_stat <- mutinformation(discretize(x), discretize(y))
    perm_stats <- replicate(n_perm, mutinformation(discretize(sample(x)), discretize(y)))
  }
  p_value <- mean(perm_stats >= obs_stat)
  return(p_value)
}
#' ------------------------------------------------------------------------------
#' buscar_mejor_modelo()
#' ------------------------------------------------------------------------------
#' Function to identify the best model among combinations of distributed
#' nonlinear structures (DLNM), evaluating different types of exposure
#' and lag functions using Poisson and Negative Binomial regressions.
#' The optimal model is selected based on fit and prediction metrics.
#'
#' Parameters:
#' - data: data.frame containing the input data
#' - variable: name of the exposure variable (string)
#' - respuesta: name of the response variable (string)
#' - max_lag: maximum number of lags to evaluate
#' - max_df_var: maximum degrees of freedom for the exposure function
#' - max_df_lag: maximum degrees of freedom for the lag function
#'
#' Returns:
#' - A list with:
#'   - mejor_modelo: data.frame with the optimal model row
#'   - resultado_general: data.frame with all evaluated combinations
#' ------------------------------------------------------------------------------

buscar_mejor_modelo <- function(data, variable, respuesta, max_lag, max_df_var, max_df_lag) {
  #' --------------------------------------------------------------------------
  #' Create structure to store evaluation results
  #' --------------------------------------------------------------------------
  resultados <- data.frame(
    modelo = character(),
    lag = integer(),
    df_var = character(),
    df_lag = character(),
    AIC = numeric(),
    MAE = numeric(),
    MSE = numeric(),
    RMSE = numeric(),
    R2 = numeric(),
    score = numeric()
  )
  
  #' --------------------------------------------------------------------------
  #' Helper function to evaluate fit metrics for a given model
  #' --------------------------------------------------------------------------
  evaluar_modelo <- function(modelo, data_filtrada, respuesta, lag, df_var, df_lag, tipo) {
    predicciones <- predict(modelo, type = "response")
    obs <- data_filtrada[[respuesta]]
    
    MAE <- mean(abs(obs - predicciones))
    MSE <- mean((obs - predicciones)^2)
    RMSE <- sqrt(MSE)
    R2 <- cor(obs, predicciones)^2
    AIC_val <- AIC(modelo)
    
    score <- (1 / AIC_val) + (1 / MAE) + (1 / MSE) + (1 / RMSE) + R2
    
    data.frame(
      modelo = tipo,
      lag = lag,
      df_var = as.character(df_var),
      df_lag = as.character(df_lag),
      AIC = AIC_val,
      MAE = MAE,
      MSE = MSE,
      RMSE = RMSE,
      R2 = R2,
      score = score
    )
  }
  
  #' --------------------------------------------------------------------------
  #' Initial evaluation: linear function with strata lag structure
  #' --------------------------------------------------------------------------
  for (lag in 2:max_lag) {
    cb_temp_ini <- crossbasis(data[[variable]],
                              lag = lag,
                              argvar = list(fun = "lin"),
                              arglag = list(fun = "strata")
    )
    
    n_lags <- max(attr(cb_temp_ini, "lag"))
    data_filtrada <- data[(n_lags + 1):nrow(data), ]
    cb_temp_ini_filtrado <- cb_temp_ini[(n_lags + 1):nrow(cb_temp_ini), , drop = FALSE]
    
    modelo_poi <- glm(
      data_filtrada[[respuesta]] ~ cb_temp_ini_filtrado,
      family = poisson(link = "log"),
      data = data_filtrada
    )
    
    modelo_nb <- glm.nb(
      data_filtrada[[respuesta]] ~ cb_temp_ini_filtrado,
      link = log,
      data = data_filtrada
    )
    
    resultados <- rbind(
      resultados,
      evaluar_modelo(modelo_poi, data_filtrada, respuesta, lag, "lin", "strata", "Poisson"),
      evaluar_modelo(modelo_nb, data_filtrada, respuesta, lag, "lin", "strata", "NB")
    )
  }
  
  #' --------------------------------------------------------------------------
  #' Evaluation using natural cubic splines for both variable and lag
  #' --------------------------------------------------------------------------
  for (lag in 2:max_lag) {
    for (df_var in 2:max_df_var) {
      for (df_lag in 2:max_df_lag) {
        cb_temp <- crossbasis(data[[variable]],
                              lag = lag,
                              argvar = list(fun = "ns", df = df_var),
                              arglag = list(fun = "ns", df = df_lag)
        )
        
        n_lags <- max(attr(cb_temp, "lag"))
        data_filtrada <- data[(n_lags + 1):nrow(data), ]
        cb_temp_filtrado <- cb_temp[(n_lags + 1):nrow(cb_temp), , drop = FALSE]
        
        modelo_poi <- glm(
          data_filtrada[[respuesta]] ~ cb_temp_filtrado,
          family = poisson(link = "log"),
          data = data_filtrada
        )
        
        modelo_nb <- glm.nb(
          data_filtrada[[respuesta]] ~ cb_temp_filtrado,
          link = log,
          data = data_filtrada
        )
        
        resultados <- rbind(
          resultados,
          evaluar_modelo(modelo_poi, data_filtrada, respuesta, lag, df_var, df_lag, "Poisson"),
          evaluar_modelo(modelo_nb, data_filtrada, respuesta, lag, df_var, df_lag, "NB")
        )
      }
    }
  }
  
  #' --------------------------------------------------------------------------
  #' Identify the model with the best global score
  #' --------------------------------------------------------------------------
  mejor_modelo <- resultados[resultados$score == max(resultados$score), ]
  
  cat("\nBest model based on score:\n")
  print(mejor_modelo)
  
  list(
    mejor_modelo = mejor_modelo,
    resultado_general = resultados
  )
}
## 4.2 Correlational analysis for mean temperature ----

results_Temp <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)


#' Compute correlations and p-values for each lag

max_lag <- 14
for (lag in 1:max_lag) {
  X_lagged <- head(data_ra$Temp_media, length(data_ra$Temp_media) - lag)
  Y_lagged <- tail(data_ra$egreso_semana, length(data_ra$egreso_semana) - lag)
  
  # Pearson and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  # dCor and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  # MI and p-value
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  # Save results
  results_Temp <- rbind(
    results_Temp,
    data.frame(
      Lag = lag, Pearson = pearson_corr, Pearson_p = pearson_p,
      dCor = dcor_val, dCor_p = dcor_p,
      MI = MI_val, MI_p = MI_p
    )
  )
}


#' View results

results_Temp

#' Compute max and min values for each series
max_pearson <- max(results_Temp$Pearson)
min_pearson <- min(results_Temp$Pearson)

max_dcor <- max(results_Temp$dCor)
min_dcor <- min(results_Temp$dCor)

max_mi <- max(results_Temp$MI)
min_mi <- min(results_Temp$MI)


#' Correlation plot for each method

ggplot(results_Temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature Lag (k)", y = "Coefficient") +
  scale_color_manual(
    values = c(
      "Pearson (linear)" = "blue",
      "dCor (nonlinear)" = "red",
      "MI (nonlinear)" = "green"
    )
  ) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_Temp$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Temp$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_Temp$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Temp$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_Temp$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Temp$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )


#' Run model selection up to lag 9

res <- buscar_mejor_modelo(
  data = data_ra,
  variable = "Temp_media",
  respuesta = "egreso_semana",
  max_lag = 12,
  max_df_var = 3,
  max_df_lag = 3
)


#' View results using DT::datatable

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",           # Black text
    backgroundColor = "lightgray" # Light gray background for contrast
  )
## 4.3 Correlational analysis for precipitation and discharges ----

#' Create an empty data frame to store results
results_Prec <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)


#' Compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  # Shift series to evaluate lag effect
  X_lagged <- head(data_ra$precip_media, length(data_ra$precip_media) - lag)
  Y_lagged <- tail(data_ra$egreso_semana, length(data_ra$egreso_semana) - lag)
  
  # Pearson correlation and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  # Distance correlation (dCor) and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  # Mutual information (MI) and p-value
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  # Store results
  results_Prec <- rbind(
    results_Prec,
    data.frame(
      Lag = lag,
      Pearson = pearson_corr, Pearson_p = pearson_p,
      dCor = dcor_val, dCor_p = dcor_p,
      MI = MI_val, MI_p = MI_p
    )
  )
}

#' Display results
results_Prec


#' Compute max and min values for each series
max_pearson <- max(results_Prec$Pearson)
min_pearson <- min(results_Prec$Pearson)

max_dcor <- max(results_Prec$dCor)
min_dcor <- min(results_Prec$dCor)

max_mi <- max(results_Prec$MI)
min_mi <- min(results_Prec$MI)

#' Plot correlations and lags
ggplot(results_Prec, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Precipitation Lag (k)", y = "Coefficient") +
  
  #' Custom colors for series
  scale_color_manual(
    values = c(
      "Pearson (linear)" = "blue",
      "dCor (nonlinear)" = "red",
      "MI (nonlinear)" = "green"
    )
  ) +
  
  #' Format X-axis to show integer lags
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  
  #' Apply minimalist theme
  theme_minimal() +
  theme(legend.title = element_blank()) +
  
  #' Add annotations for max and min values
  annotate(
    "text",
    x = which.max(results_Prec$Pearson), y = max_pearson,
    label = paste("Max:", round(max_pearson, 2)),
    color = "blue", vjust = -0.5
  ) +
  annotate(
    "text",
    x = which.min(results_Prec$Pearson), y = min_pearson,
    label = paste("Min:", round(min_pearson, 2)),
    color = "blue", vjust = 1.5
  ) +
  annotate(
    "text",
    x = which.max(results_Prec$dCor), y = max_dcor,
    label = paste("Max:", round(max_dcor, 2)),
    color = "red", vjust = -0.5
  ) +
  annotate(
    "text",
    x = which.min(results_Prec$dCor), y = min_dcor,
    label = paste("Min:", round(min_dcor, 2)),
    color = "red", vjust = 1.5
  ) +
  annotate(
    "text",
    x = which.max(results_Prec$MI), y = max_mi,
    label = paste("Max:", round(max_mi, 2)),
    color = "green", vjust = -0.5
  ) +
  annotate(
    "text",
    x = which.min(results_Prec$MI), y = min_mi,
    label = paste("Min:", round(min_mi, 2)),
    color = "green", vjust = 1.5
  )


#' Search for the best model using precipitation and discharges,
#' with a maximum of 12 lags

res <- buscar_mejor_modelo(
  data_ra,
  "precip_media",
  "egreso_semana",
  max_lag = 12,
  max_df_var = 3,
  max_df_lag = 3
)


#' Display results in an interactive table

datatable(
  res$resultado_general,
  options = list(
    pageLength = 5,      # Number of visible rows per page
    scrollX = TRUE       # Enable horizontal scrolling
  )
) %>%
  formatStyle(
    columns = names(res$resultado_general), # Apply to all columns
    color = "black",                        # Black text
    backgroundColor = "lightgray"          # Light gray background
  )
## 4.4 Correlational analysis for Relative Humidity (RH) ----

#' Store results in a data frame

results_RH <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' Compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  #' Create lagged series
  X_lagged <- head(data_ra$RH, length(data_ra$RH) - lag)
  Y_lagged <- tail(data_ra$egreso_semana, length(data_ra$egreso_semana) - lag)
  
  #' Pearson correlation and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  #' Distance correlation and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  #' Mutual information and p-value
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' Store results
  results_RH <- rbind(
    results_RH,
    data.frame(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
  )
}

#' Display results
results_RH

#' Compute max and min values for each series

max_pearson <- max(results_RH$Pearson)
min_pearson <- min(results_RH$Pearson)

max_dcor <- max(results_RH$dCor)
min_dcor <- min(results_RH$dCor)

max_mi <- max(results_RH$MI)
min_mi <- min(results_RH$MI)

#' Plot correlation results

ggplot(results_RH, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    x = "Relative Humidity Lag (k)",
    y = "Coefficient"
  ) +
  scale_color_manual(
    values = c(
      "Pearson (linear)" = "blue",
      "dCor (nonlinear)" = "red",
      "MI (nonlinear)" = "green"
    )
  ) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_RH$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_RH$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_RH$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_RH$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_RH$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_RH$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' Model evaluation for Relative Humidity (RH) with up to 7 lags
res <- buscar_mejor_modelo(
  data_ra,
  "RH",
  "egreso_semana",
  max_lag = 7,
  max_df_var = 3,
  max_df_lag = 3
)

#' Display results in interactive table
datatable(
  res$resultado_general,
  options = list(
    pageLength = 5,
    scrollX = TRUE
  )
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )


## 4.5 Correlational analysis for AOD index (Aerosol) -----

results_AOD <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' Compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  X_lagged <- head(data_ra$aerosol, length(data_ra$aerosol) - lag)
  Y_lagged <- tail(data_ra$egreso_semana, length(data_ra$egreso_semana) - lag)
  
  #' Pearson and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  #' dCor and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  #' MI and p-value
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' Store results
  results_AOD <- rbind(
    results_AOD,
    data.frame(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
  )
}

#' Display results
results_AOD

#' Compute max and min values for each series
max_pearson <- max(results_AOD$Pearson)
min_pearson <- min(results_AOD$Pearson)

max_dcor <- max(results_AOD$dCor)
min_dcor <- min(results_AOD$dCor)

max_mi <- max(results_AOD$MI)
min_mi <- min(results_AOD$MI)

#' Plot correlation results

ggplot(results_AOD, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    x = "AOD Index Lag (k)",
    y = "Coefficient"
  ) +
  scale_color_manual(
    values = c(
      "Pearson (linear)" = "blue",
      "dCor (nonlinear)" = "red",
      "MI (nonlinear)" = "green"
    )
  ) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_AOD$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_AOD$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_AOD$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_AOD$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_AOD$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_AOD$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' Model evaluation for AOD index (Aerosol)
res <- buscar_mejor_modelo(
  data_ra,
  "aerosol",
  "egreso_semana",
  max_lag = 11,
  max_df_var = 3,
  max_df_lag = 3
)

#' Display results in interactive table
datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )

#' ------------------------------------------------------------------------------
#' Function: discrete_modeling_parameters()
#' ------------------------------------------------------------------------------
#' Evaluates multiple combinations of nonlinear basis functions for climatic
#' variables (temperature, precipitation, relative humidity, and AOD) using
#' negative binomial models with distributed lags.
#'
#' Inputs:
#' - data: full input dataset.
#' - respuesta: name of the response variable (count).
#' - temp_var, prec_var, rh_var, aod_var: names of the predictor variables.
#' - max_lag_*: maximum lag to evaluate for each predictor.
#' - max_df_var_*: maximum degrees of freedom for the variable function.
#' - max_df_lag_*: maximum degrees of freedom for the lag function.
#'
#' Output:
#' - A list with a data.frame named 'resultados_completos' containing the
#'   model fit metrics (AIC, BIC, MAE, MSE, RMSE, R2, MAPE, logLik, score)
#'   for each parameter combination evaluated.
#'
#' Details:
#' - The fitted model is of type glm.nb.
#' - Includes seasonal effects via as.factor(epi.week).
#' - Uses crossbasis functions from the dlnm package.
#' ------------------------------------------------------------------------------

discrete_modeling_parameters <- function(
    data, respuesta,
    temp_var, prec_var, rh_var, aod_var,
    max_lag_temp, max_df_var_temp, max_df_lag_temp,
    max_lag_prec, max_df_var_prec, max_df_lag_prec,
    max_lag_rh, max_df_var_rh, max_df_lag_rh,
    max_lag_AOD, max_df_var_AOD, max_df_lag_AOD
) {
  
  variables <- list(
    temp = list(var = temp_var, max_lag = max_lag_temp, max_df_var = max_df_var_temp, max_df_lag = max_df_lag_temp),
    prec = list(var = prec_var, max_lag = max_lag_prec, max_df_var = max_df_var_prec, max_df_lag = max_df_lag_prec),
    rh   = list(var = rh_var,   max_lag = max_lag_rh,   max_df_var = max_df_var_rh,   max_df_lag = max_df_lag_rh),
    aod  = list(var = aod_var,  max_lag = max_lag_AOD,  max_df_var = max_df_var_AOD,  max_df_lag = max_df_lag_AOD)
  )
  
  resultados <- data.frame(
    modelo = character(),
    fun_var_temp = character(), df_var_temp = integer(), fun_lag_temp = character(), df_lag_temp = integer(),
    fun_var_prec = character(), df_var_prec = integer(), fun_lag_prec = character(), df_lag_prec = integer(),
    fun_var_rh   = character(), df_var_rh   = integer(), fun_lag_rh   = character(), df_lag_rh   = integer(),
    fun_var_aod  = character(), df_var_aod  = integer(), fun_lag_aod  = character(), df_lag_aod  = integer(),
    AIC = numeric(), BIC = numeric(), MAE = numeric(), MSE = numeric(), RMSE = numeric(), R2 = numeric(),
    MAPE = numeric(), logLik = numeric(), score = numeric(),
    stringsAsFactors = FALSE
  )
  
  evaluate_model <- function(model, data_filtered, response, type, params) {
    predictions <- predict(model, type = "response")
    obs <- data_filtered[[response]]
    
    MAE <- mean(abs(obs - predictions))
    MSE <- mean((obs - predictions)^2)
    RMSE <- sqrt(MSE)
    R2 <- cor(obs, predictions)^2
    AIC_val <- AIC(model)
    BIC_val <- BIC(model)
    MAPE_val <- mean(abs((obs - predictions) / obs)) * 100
    ll <- as.numeric(logLik(model))
    
    logLik_term <- if (ll < 0) 1 / abs(ll) else ll
    
    score <- (1 / AIC_val) + (1 / BIC_val) + (1 / MAE) + (1 / MSE) + (1 / RMSE) +
      (1 / MAPE_val) + logLik_term
    
    return(data.frame(
      modelo = type,
      fun_var_temp = params$fun_var_temp, df_var_temp = params$df_var_temp, 
      fun_lag_temp = params$fun_lag_temp, df_lag_temp = params$df_lag_temp,
      fun_var_prec = params$fun_var_prec, df_var_prec = params$df_var_prec, 
      fun_lag_prec = params$fun_lag_prec, df_lag_prec = params$df_lag_prec,
      fun_var_rh   = params$fun_var_rh,   df_var_rh   = params$df_var_rh, 
      fun_lag_rh   = params$fun_lag_rh,   df_lag_rh   = params$df_lag_rh,
      fun_var_aod  = params$fun_var_aod,  df_var_aod  = params$df_var_aod, 
      fun_lag_aod  = params$fun_lag_aod,  df_lag_aod  = params$df_lag_aod,
      AIC = AIC_val, BIC = BIC_val, MAE = MAE, MSE = MSE, RMSE = RMSE, R2 = R2,
      MAPE = MAPE_val, logLik = ll, score = score,
      stringsAsFactors = FALSE
    ))
  }
  
  generate_crossbasis <- function(variable_info, data) {
    var <- variable_info$var
    max_lag <- variable_info$max_lag
    max_df_var <- variable_info$max_df_var
    max_df_lag <- variable_info$max_df_lag
    
    bases <- list()
    
    bases[["lin_lin"]] <- list(
      crossbasis = crossbasis(data[[var]], lag = max_lag, 
                              argvar = list(fun = "lin"), 
                              arglag = list(fun = "lin")),
      fun_var = "lin", df_var = NA,
      fun_lag = "lin", df_lag = NA
    )
    
    for (df_var in 2:max_df_var) {
      for (df_lag in 2:max_df_lag) {
        key <- paste("ns", df_var, "ns", df_lag, sep = "_")
        bases[[key]] <- list(
          crossbasis = crossbasis(data[[var]], lag = max_lag, 
                                  argvar = list(fun = "ns", df = df_var), 
                                  arglag = list(fun = "ns", df = df_lag)),
          fun_var = "ns", df_var = df_var,
          fun_lag = "ns", df_lag = df_lag
        )
      }
    }
    return(bases)
  }
  
  crossbases <- lapply(variables, generate_crossbasis, data = data)
  
  max_na_lags <- max(sapply(crossbases, function(cb) 
    max(sapply(cb, function(x) max(attr(x$crossbasis, "lag"))))
  ))
  data_filtered <- data[(max_na_lags + 1):nrow(data), ]
  
  filtered_crossbases <- lapply(crossbases, function(cb) {
    lapply(cb, function(x) list(
      crossbasis = x$crossbasis[(max_na_lags + 1):nrow(x$crossbasis), , drop = FALSE],
      fun_var = x$fun_var, df_var = x$df_var,
      fun_lag = x$fun_lag, df_lag = x$df_lag
    ))
  })
  
  for (cb_temp in filtered_crossbases$temp) {
    for (cb_prec in filtered_crossbases$prec) {
      for (cb_rh in filtered_crossbases$rh) {
        for (cb_aod in filtered_crossbases$aod) {
          
          params <- list(
            fun_var_temp = cb_temp$fun_var, df_var_temp = cb_temp$df_var, 
            fun_lag_temp = cb_temp$fun_lag, df_lag_temp = cb_temp$df_lag,
            fun_var_prec = cb_prec$fun_var, df_var_prec = cb_prec$df_var, 
            fun_lag_prec = cb_prec$fun_lag, df_lag_prec = cb_prec$df_lag,
            fun_var_rh   = cb_rh$fun_var,   df_var_rh   = cb_rh$df_var, 
            fun_lag_rh   = cb_rh$fun_lag,   df_lag_rh   = cb_rh$df_lag,
            fun_var_aod  = cb_aod$fun_var,  df_var_aod  = cb_aod$df_var, 
            fun_lag_aod  = cb_aod$fun_lag,  df_lag_aod  = cb_aod$df_lag
          )
          
          model_fit <- glm.nb(data_filtered[[respuesta]] ~ 
                                cb_temp$crossbasis + cb_prec$crossbasis + 
                                cb_rh$crossbasis + cb_aod$crossbasis +
                                as.factor(epi.week), 
                              link = "log", data = data_filtered)
          
          resultados <- rbind(resultados, 
                              evaluate_model(model_fit, data_filtered, respuesta, "modelo_optim", params))
        }
      }
    }
  }
  
  return(list(resultados_completos = resultados))
}

## 4.6 Scatter plots for variables with optimal lags ----

#' Scatter plot for Temp_media (lag 5)
d_temp <- data_ra %>%
  mutate(Temp_media_lag5 = lag(Temp_media, 5)) %>%
  slice(6:n()) # Remove NA due to lag

ggplot(d_temp, aes(x = Temp_media_lag5, y = log(egreso_semana))) +
  geom_point(color = "#2c7fb8") + # Blue
  labs(x = "Mean Temperature (lag 5)", y = "Log of Weekly Discharges") +
  theme_bw()

#' Scatter plot for precip_media (lag 12)
d_precip <- data_ra %>%
  mutate(precip_media_lag12 = lag(precip_media, 12)) %>%
  slice(13:n())

ggplot(d_precip, aes(x = precip_media_lag12, y = log(egreso_semana))) +
  geom_point(color = "#f03b20") + # Red
  labs(x = "Mean Precipitation (lag 12)", y = "Log of Weekly Discharges") +
  theme_bw()

#' Scatter plot for aerosol (lag 11)
d_aerosol <- data_ra %>%
  mutate(aerosol_lag11 = lag(aerosol, 11)) %>%
  slice(12:n())

ggplot(d_aerosol, aes(x = aerosol_lag11, y = log(egreso_semana))) +
  geom_point(color = "#31a354") + # Green
  labs(x = "AOD Index (lag 11)", y = "Log of Weekly Discharges") +
  theme_bw()

#' Scatter plot for RH (lag 7)
d_rh <- data_ra %>%
  mutate(RH_lag7 = lag(RH, 7)) %>%
  slice(8:n())

ggplot(d_rh, aes(x = RH_lag7, y = log(egreso_semana))) +
  geom_point(color = "#756bb1") + # Purple
  labs(x = "Relative Humidity (lag 7)", y = "Log of Weekly Discharges") +
  theme_bw()


# 5. Crossbasis for climatic variables ---------------------------------------

#' This block creates crossbasis functions using cubic splines to model the
#' nonlinear relationship between climatic variables and hospital discharges.
#' Specific lags are set per variable based on prior findings.


## 5.1 Mean Temperature (lag = 5, cubic spline) ----

cb_temp_egre_ra <- crossbasis(
  data_ra$Temp_media,
  lag = 5,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.2 Mean Precipitation (lag = 12, cubic spline) ----

cb_prec_egre_ra <- crossbasis(
  data_ra$precip_media,
  lag = 12,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.3 Relative Humidity (lag = 7, cubic spline) ----

cb_rh_egre_ra <- crossbasis(
  data_ra$RH,
  lag = 7,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.4 AOD index (lag = 11, cubic spline) ----

cb_aod_egre_ra <- crossbasis(
  data_ra$aerosol,
  lag = 11,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)


# 6. DLNM models for hospital discharges -------------------------------------

#' This block fits DLNM models using Poisson and Negative Binomial distributions
#' to evaluate the effect of climatic variables on hospital discharges.

#' Remove the first 12 observations to ensure complete lag structure
df_ra_fitted_egresos <- data_ra %>%
  slice(-(1:12))

#' ----------------------------------------------------
## 6.1 DLNM model with Poisson distribution and weekly seasonal factors ----
#' ----------------------------------------------------
dlnm_model_poi_estac_ra <- glm(
  egreso_semana ~ cb_temp_egre_ra + cb_prec_egre_ra +
    cb_rh_egre_ra + cb_aod_egre_ra +
    as.factor(epi.week),
  family = poisson(link = "log"),
  data = data_ra
)

#' ----------------------------------------------------
## 6.2 DLNM model with Poisson distribution (no seasonal factors) ----
#' ----------------------------------------------------
dlnm_model_poi_ra <- glm(
  egreso_semana ~ cb_temp_egre_ra + cb_prec_egre_ra +
    cb_rh_egre_ra + cb_aod_egre_ra,
  family = poisson(link = "log"),
  data = data_ra
)

#' ----------------------------------------------------
## 6.3 DLNM model with Negative Binomial distribution and weekly seasonal factors ----
#' ----------------------------------------------------
dlnm_model_nb_estac_ra <- glm.nb(
  egreso_semana ~ cb_temp_egre_ra + cb_prec_egre_ra +
    cb_rh_egre_ra + cb_aod_egre_ra +
    as.factor(epi.week),
  data = data_ra
)

#' ----------------------------------------------------
## 6.4 DLNM model with Negative Binomial distribution (no seasonal factors) ----
#' ----------------------------------------------------
dlnm_model_nb_ra <- glm.nb(
  egreso_semana ~ cb_temp_egre_ra + cb_prec_egre_ra +
    cb_rh_egre_ra + cb_aod_egre_ra,
  data = data_ra
)

# 7. Fitting GLM models (Poisson and Negative Binomial) -----------------------

#' This block fits two classical GLM models (without lag structure):
#' - Poisson model
#' - Negative Binomial model
#' These are used as reference models for comparison against DLNM.

## 7.1 GLM model with Poisson distribution -----
glm_model_poi_ra <- glm(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  family = poisson(link = "log"),
  data = df_ra_fitted_egresos
)

## 7.2 GLM model with Negative Binomial distribution ----
glm_model_nb_ra <- glm.nb(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  data = df_ra_fitted_egresos
)


# 8. Error metrics computation for all models --------------------------

#' Generate fitted predictions for each model
df_ra_fitted_egresos <- df_ra_fitted_egresos %>%
  mutate(
    ajustado_dlnm_model_poi_estac_ra = fitted(dlnm_model_poi_estac_ra),
    ajustado_dlnm_model_poi_ra = fitted(dlnm_model_poi_ra),
    ajustado_dlnm_model_nb_estac_ra = fitted(dlnm_model_nb_estac_ra),
    ajustado_dlnm_model_nb_ra = fitted(dlnm_model_nb_ra),
    ajustado_glm_model_poi_ra = fitted(glm_model_poi_ra),
    ajustado_glm_model_nb_ra = fitted(glm_model_nb_ra)
  )


## 8.1 Function to compute model performance metrics ----

#' ------------------------------------------------------------------------------
#' Function: calcular_metricas()
#' ------------------------------------------------------------------------------
#' Calculates performance metrics for a fitted model and its predictions.
#' Includes classical error metrics, information criteria, and a composite score.
#'
#' Parameters:
#' - modelo: fitted model object (e.g., glm, glm.nb)
#' - predicciones: numeric vector with model predictions
#' - obs: numeric vector with observed values
#' - nombre_modelo: string identifier for the model being evaluated
#'
#' Returns:
#' - Data frame with MAE, MSE, RMSE, MAPE, AIC, BIC, logLik, and composite Score
#' ------------------------------------------------------------------------------

calcular_metricas <- function(modelo, predicciones, obs, nombre_modelo) {
  MAE <- mean(abs(obs - predicciones))
  MSE <- mean((obs - predicciones)^2)
  RMSE <- sqrt(MSE)
  
  # MAPE with epsilon to avoid division by zero
  epsilon <- 1e-10
  MAPE_val <- mean(abs((obs - predicciones) / (obs + epsilon))) * 100
  
  AIC_val <- tryCatch(AIC(modelo), error = function(e) NA)
  BIC_val <- tryCatch(BIC(modelo), error = function(e) NA)
  ll <- tryCatch(as.numeric(logLik(modelo)), error = function(e) NA)
  
  # Composite score
  logLik_term <- if (!is.na(ll) && ll < 0) 1 / abs(ll) else if (!is.na(ll)) ll else 0
  score <- (1 / MAE) + (1 / MSE) + (1 / RMSE) + (1 / MAPE_val) + logLik_term
  
  data.frame(
    modelo = nombre_modelo,
    MAE = MAE,
    MSE = MSE,
    RMSE = RMSE,
    AIC = ifelse(is.na(AIC_val), "-", round(AIC_val, 2)),
    BIC = ifelse(is.na(BIC_val), "-", round(BIC_val, 2)),
    MAPE = round(MAPE_val, 2),
    logLik = ifelse(is.na(ll), "-", round(ll, 2)),
    Score = round(score, 3)
  )
}


#' Compute metrics for each model
resultados <- rbind(
  calcular_metricas(
    dlnm_model_poi_estac_ra,
    df_ra_fitted_egresos$ajustado_dlnm_model_poi_estac_ra,
    df_ra_fitted_egresos$egreso_semana,
    "DLNM_Poi_est_RA"
  ),
  calcular_metricas(
    dlnm_model_poi_ra,
    df_ra_fitted_egresos$ajustado_dlnm_model_poi_ra,
    df_ra_fitted_egresos$egreso_semana,
    "DLNM_Poi_RA"
  ),
  calcular_metricas(
    dlnm_model_nb_estac_ra,
    df_ra_fitted_egresos$ajustado_dlnm_model_nb_estac_ra,
    df_ra_fitted_egresos$egreso_semana,
    "DLNM_NB_est_RA"
  ),
  calcular_metricas(
    dlnm_model_nb_ra,
    df_ra_fitted_egresos$ajustado_dlnm_model_nb_ra,
    df_ra_fitted_egresos$egreso_semana,
    "DLNM_NB_RA"
  ),
  calcular_metricas(
    glm_model_poi_ra,
    df_ra_fitted_egresos$ajustado_glm_model_poi_ra,
    df_ra_fitted_egresos$egreso_semana,
    "GLM_Poi_RA"
  ),
  calcular_metricas(
    glm_model_nb_ra,
    df_ra_fitted_egresos$ajustado_glm_model_nb_ra,
    df_ra_fitted_egresos$egreso_semana,
    "GLM_NB_RA"
  )
)

#' Sort results by best composite score
resultados <- resultados %>%
  arrange(desc(Score))

#' Display results in an interactive table
datatable(resultados, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados),
    color = "black",
    backgroundColor = "lightgray"
  )

## 8.2 Optimal model ----
dlnm_model_ra <- dlnm_model_nb_estac_ra

# Save the models to a file
save(dlnm_model_nb_estac_ra,
     dlnm_model_nb_ra,
     dlnm_model_ra,
     dlnm_model_poi_estac_ra,
     dlnm_model_poi_ra,
     glm_model_nb_ra,
     glm_model_poi_ra,
     file = "fixed_effects_models/models_for_discharges_ra.RData"
)


# 9. Residual diagnostics for optimal DLNM model -----------------------------

#' Create time series for observed and fitted discharges
egresos_ra <- ts(
  df_ra_fitted_egresos$egreso_semana,
  start = c(2000, 13),
  frequency = 52
)

egresos_fitt_dlnm_ra <- ts(
  fitted(dlnm_model_ra),
  start = c(2000, 13),
  frequency = 52
)

#' Pearson and deviance residuals
resid_pearson_ra <- residuals(
  dlnm_model_ra,
  type = "pearson"
)

resid_deviance_ra <- residuals(
  dlnm_model_ra,
  type = "deviance"
)

#' Convert residuals to time series
resid_pearson_st <- ts(
  resid_pearson_ra,
  start = c(2000, 13),
  frequency = 52
)

resid_deviance_st <- ts(
  resid_deviance_ra,
  start = c(2000, 13),
  frequency = 52
)


## 9.1 Dispersion plots ----------------------------------------------

#' Scatter plot for Pearson residuals
plot(
  fitted(dlnm_model_ra), resid_pearson_ra,
  xlab = "Fitted values",
  ylab = "Pearson residuals"
)
abline(h = 0, col = "red")

#' Scatter plot for deviance residuals
plot(
  fitted(dlnm_model_ra), resid_deviance_ra,
  xlab = "Fitted values",
  ylab = "Deviance residuals"
)
abline(h = 0, col = "red")


## 9.2 Autocorrelation diagnostics ----

#' ACF and PACF for Pearson residuals
ggtsdisplay(resid_pearson_st)

#' ACF and PACF for deviance residuals
ggtsdisplay(resid_deviance_st)

#' Ljung-Box test for autocorrelation
ljbox_pearson_dlnm_ra <- Box.test(
  resid_pearson_ra,
  lag = 20,
  type = "Ljung-Box"
)

ljbox_deviance_dlnm_ra <- Box.test(
  resid_deviance_ra,
  lag = 20,
  type = "Ljung-Box"
)


## 9.3 Normality tests ----

#' QQ-plot for Pearson residuals
qqnorm(resid_pearson_ra, main = "QQ plot of Pearson residuals")
qqline(resid_pearson_ra, col = "red")

#' QQ-plot for deviance residuals
qqnorm(resid_deviance_ra, main = "QQ plot of Deviance residuals")
qqline(resid_deviance_ra, col = "red")

#' Shapiro-Wilk normality test
shapiro_test_pearson <- shapiro.test(resid_pearson_ra)
shapiro_test_deviance <- shapiro.test(resid_deviance_ra)

#' Display test results
print(shapiro_test_pearson)
print(shapiro_test_deviance)

# 10. Model predictions ---------------------------------------------------

## 10.1 Time series comparison plot ----
#' ----------------------------------------------------------------------------
#' This code generates a comparison plot between observed values and fitted values
#' using DLNM Negative Binomial models with and without seasonal factors, and
#' a standard GLM with Negative Binomial distribution for hospital discharges
#' in the RA region.
#' ----------------------------------------------------------------------------

#' Create time series for plotting
df_ra_fitted_egresos <- df_ra_fitted_egresos %>%
  mutate(
    egresos_ts = ts(egreso_semana, start = c(2000, 13), frequency = 52),        # Observed discharges
    ajuste_DLNM = ts(ajustado_dlnm_model_nb_estac_ra, start = c(2000, 13), frequency = 52), # DLNM with seasonal effects
    ajuste_glm = ts(ajustado_glm_model_nb_ra, start = c(2000, 13), frequency = 52)          # GLM NB model
  )

#' Plot observed vs fitted values using autoplot
autoplot(df_ra_fitted_egresos$egresos_ts, series = "Observed") +
  autolayer(df_ra_fitted_egresos$ajuste_DLNM, series = "DLNM NB Fit") +
  autolayer(df_ra_fitted_egresos$ajuste_glm, series = "GLM NB Fit") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Observed" = "green",
      "DLNM NB Fit" = "blue",
      "GLM NB Fit" = "red"
    )
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )


## 10.2 Predictions for lagged effects  ----

#' ====================================================
#' This block generates predictions from the fitted DLNM model using crossbasis
#' for temperature, precipitation, relative humidity, and AOD.

#' ----------------------------------------------------
#' Mean temperature predictions (crosspred)
#' ----------------------------------------------------
pred_dlnm_ra_temp <- crosspred(
  cb_temp_egre_ra,
  dlnm_model_ra,
  cen = mean(data_ra$Temp_media, na.rm = TRUE),
  bylag = 0.1
)

#' ----------------------------------------------------
#' Mean precipitation predictions (crosspred)
#' ----------------------------------------------------
pred_dlnm_ra_prec <- crosspred(
  cb_prec_egre_ra,
  dlnm_model_ra,
  cen = mean(data_ra$precip_media, na.rm = TRUE),
  bylag = 0.1
)

#' ----------------------------------------------------
#' Relative humidity predictions (crosspred)
#' ----------------------------------------------------
pred_dlnm_ra_rh <- crosspred(
  cb_rh_egre_ra,
  dlnm_model_ra,
  cen = mean(data_ra$RH, na.rm = TRUE),
  bylag = 0.1
)

#' ----------------------------------------------------
#' AOD index predictions (crosspred)
#' ----------------------------------------------------
pred_dlnm_ra_aod <- crosspred(
  cb_aod_egre_ra,
  dlnm_model_ra,
  cen = mean(data_ra$aerosol, na.rm = TRUE),
  bylag = 0.1
)


### 10.2.1 Mean temperature predictions ----

#' 3D plot (temperature - lag - RR)
plot(
  pred_dlnm_ra_temp,
  xlab = "Mean Temperature",
  zlab = "Relative Risk (RR)",
  theta = 300, phi = 10, lphi = 100
)

#' Contour plot (temperature - lag - RR)
plot(
  pred_dlnm_ra_temp, "contour",
  xlab = "Mean Temperature",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response curve for 26°C
plot(
  pred_dlnm_ra_temp, "slices",
  var = 26,
  col = 2,
  ylab = "Relative Risk (RR)"
)

#' Lag-response curve for 27°C
plot(
  pred_dlnm_ra_temp, "slices",
  var = 27,
  col = 2,
  ylab = "Relative Risk (RR)"
)


### 10.2.2 Mean precipitation predictions ----

#' 3D plot (precipitation - lag - RR)
plot(
  pred_dlnm_ra_prec,
  xlab = "Mean Precipitation",
  zlab = "Relative Risk (RR)",
  theta = 280, phi = 10, lphi = 100
)

#' Contour plot (precipitation - lag - RR)
plot(
  pred_dlnm_ra_prec, "contour",
  xlab = "Mean Precipitation",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response curve for 40 mm
plot(
  pred_dlnm_ra_prec, "slices",
  var = 40,
  col = 2,
  ylab = "Relative Risk (RR)"
)

#' Lag-response curve for 100 mm
plot(
  pred_dlnm_ra_prec, "slices",
  var = 100,
  col = 2,
  ylab = "Relative Risk (RR)"
)


### 10.2.3 Relative humidity predictions ----

#' 3D plot (humidity - lag - RR)
plot(
  pred_dlnm_ra_rh,
  xlab = "Mean Relative Humidity",
  zlab = "Relative Risk (RR)",
  theta = 300, phi = 10, lphi = 100
)

#' Contour plot (humidity - lag - RR)
plot(
  pred_dlnm_ra_rh, "contour",
  xlab = "Mean Relative Humidity",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response curve for humidity = 0.25
plot(
  pred_dlnm_ra_rh, "slices",
  var = 0.25,
  col = 2,
  ylab = "Relative Risk (RR)"
)

#' Lag-response curve for humidity = 0.45
plot(
  pred_dlnm_ra_rh, "slices",
  var = 0.45,
  col = 2,
  ylab = "Relative Risk (RR)"
)


### 10.2.4 AOD index predictions ----

#' 3D plot (AOD - lag - RR)
plot(
  pred_dlnm_ra_aod,
  xlab = "AOD Index",
  zlab = "Relative Risk (RR)",
  theta = 200, phi = 10, lphi = 100
)

#' Contour plot (AOD - lag - RR)
plot(
  pred_dlnm_ra_aod, "contour",
  xlab = "AOD Index",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response curve for AOD = 1e-05
plot(
  pred_dlnm_ra_aod, "slices",
  var = 1e-05,
  col = 2,
  ylab = "Relative Risk (RR)"
)

#' Lag-response curve for AOD = 5e-04
plot(
  pred_dlnm_ra_aod, "slices",
  var = 5e-04,
  col = 2,
  ylab = "Relative Risk (RR)"
)

# 11. Forecasting -----------------------------------------------------------

## 11.1 General data setup ----

# Construct data frame with crossbasis predictors
df_con_crossbasis_egresos <- cbind(data_ra,
                                   setNames(as.data.frame(cb_temp_egre_ra), make.names(paste0("t_", colnames(cb_temp_egre_ra)), unique = TRUE)),
                                   setNames(as.data.frame(cb_prec_egre_ra), make.names(paste0("p_", colnames(cb_prec_egre_ra)), unique = TRUE)),
                                   setNames(as.data.frame(cb_rh_egre_ra), make.names(paste0("rh_", colnames(cb_rh_egre_ra)), unique = TRUE)),
                                   setNames(as.data.frame(cb_aod_egre_ra), make.names(paste0("aod_", colnames(cb_aod_egre_ra)), unique = TRUE))
)

# Remove initial NA due to lag
df_con_crossbasis_egresos <- df_con_crossbasis_egresos %>%
  slice(-(1:12))

# Function to compute error metrics
calcular_errores <- function(obs, pred, modelo) {
  mae_val <- mae(obs, pred)
  mse_val <- mse(obs, pred)
  rmse_val <- rmse(obs, pred)
  mape_val <- mape(obs, pred) * 100
  sampe_val <- mean(2 * abs(pred - obs) / (abs(obs) + abs(pred))) * 100
  
  tibble(
    modelo = modelo,
    MAE = mae_val,
    MSE = mse_val,
    RMSE = rmse_val,
    MAPE = mape_val,
    SAMPE = sampe_val
  )
}

## 11.2 Forecast for 2017 ----

df_train <- df_con_crossbasis_egresos %>% filter(year != 2017)
df_2017 <- df_con_crossbasis_egresos %>% filter(year == 2017)

columns_exposure <- colnames(df_con_crossbasis_egresos)[12:ncol(df_con_crossbasis_egresos)]

formula_dlnm <- as.formula(
  paste("egreso_semana ~", paste(columns_exposure, collapse = " +"), "+ as.factor(epi.week)")
)

dlnm_model_nb_estac_pc <- glm.nb(formula = formula_dlnm, data = df_train)
glm_model_nb_pc <- glm.nb(egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week), data = df_train)

df_train <- df_train %>%
  mutate(
    pred_dlnm_train = predict(dlnm_model_nb_estac_pc, type = "response"),
    pred_glm_train = predict(glm_model_nb_pc, type = "response")
  )

df_2017 <- df_2017 %>%
  mutate(
    pred_dlnm_2017 = predict(dlnm_model_nb_estac_pc, newdata = df_2017, type = "response"),
    pred_glm_2017 = predict(glm_model_nb_pc, newdata = df_2017, type = "response")
  )

errores_2017 <- bind_rows(
  calcular_errores(df_train$egreso_semana, df_train$pred_glm_train, "GLM_train"),
  calcular_errores(df_train$egreso_semana, df_train$pred_dlnm_train, "DLNM_train"),
  calcular_errores(df_2017$egreso_semana, df_2017$pred_dlnm_2017, "DLNM_2017"),
  calcular_errores(df_2017$egreso_semana, df_2017$pred_glm_2017, "GLM_2017")
)

## 11.3 Forecast for 2018 ----

df_train <- df_con_crossbasis_egresos %>% filter(year != 2018)
df_2018 <- df_con_crossbasis_egresos %>% filter(year == 2018)

dlnm_model_nb_estac_pc <- glm.nb(formula = formula_dlnm, data = df_train)
glm_model_nb_pc <- glm.nb(egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week), data = df_train)

df_train <- df_train %>%
  mutate(
    pred_dlnm_train = predict(dlnm_model_nb_estac_pc, type = "response"),
    pred_glm_train = predict(glm_model_nb_pc, type = "response")
  )

df_2018 <- df_2018 %>%
  mutate(
    pred_dlnm_2018 = predict(dlnm_model_nb_estac_pc, newdata = df_2018, type = "response"),
    pred_glm_2018 = predict(glm_model_nb_pc, newdata = df_2018, type = "response")
  )

errores_2018 <- bind_rows(
  calcular_errores(df_train$egreso_semana, df_train$pred_glm_train, "GLM_train"),
  calcular_errores(df_train$egreso_semana, df_train$pred_dlnm_train, "DLNM_train"),
  calcular_errores(df_2018$egreso_semana, df_2018$pred_dlnm_2018, "DLNM_2018"),
  calcular_errores(df_2018$egreso_semana, df_2018$pred_glm_2018, "GLM_2018")
)

## 11.4 Forecast for 2019 ----

df_train <- df_con_crossbasis_egresos %>% filter(year != 2019)
df_2019 <- df_con_crossbasis_egresos %>% filter(year == 2019)

dlnm_model_nb_estac_pc <- glm.nb(formula = formula_dlnm, data = df_train)
glm_model_nb_pc <- glm.nb(egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week), data = df_train)

df_train <- df_train %>%
  mutate(
    pred_dlnm_train = predict(dlnm_model_nb_estac_pc, type = "response"),
    pred_glm_train = predict(glm_model_nb_pc, type = "response")
  )

df_2019 <- df_2019 %>%
  mutate(
    pred_dlnm_2019 = predict(dlnm_model_nb_estac_pc, newdata = df_2019, type = "response"),
    pred_glm_2019 = predict(glm_model_nb_pc, newdata = df_2019, type = "response")
  )

errores_2019 <- bind_rows(
  calcular_errores(df_train$egreso_semana, df_train$pred_glm_train, "GLM_train"),
  calcular_errores(df_train$egreso_semana, df_train$pred_dlnm_train, "DLNM_train"),
  calcular_errores(df_2019$egreso_semana, df_2019$pred_dlnm_2019, "DLNM_2019"),
  calcular_errores(df_2019$egreso_semana, df_2019$pred_glm_2019, "GLM_2019")
)

# Consolidated forecast error table
errores_totales <- bind_rows(errores_2017, errores_2018, errores_2019)

datatable(errores_totales, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(errores_totales),
    color = "black",
    backgroundColor = "lightgray"
  )


## 11.5 Forecast plot ----

# Full observed time series
serie_real <- ts(df_con_crossbasis_egresos$egreso_semana, start = c(2000, 13), frequency = 52)

# Forecast time series with NAs elsewhere
serie_pred_2017 <- ts(rep(NA, length(serie_real)), start = c(2000, 13), frequency = 52)
serie_pred_2017[which(df_con_crossbasis_egresos$year == 2017)] <- df_2017$pred_dlnm_2017

serie_pred_2018 <- ts(rep(NA, length(serie_real)), start = c(2000, 13), frequency = 52)
serie_pred_2018[which(df_con_crossbasis_egresos$year == 2018)] <- df_2018$pred_dlnm_2018

serie_pred_2019 <- ts(rep(NA, length(serie_real)), start = c(2000, 13), frequency = 52)
serie_pred_2019[which(df_con_crossbasis_egresos$year == 2019)] <- df_2019$pred_dlnm_2019

# Plot observed vs forecasted values
autoplot(serie_real, series = "Observed", color = "green", size = 0.4) +
  autolayer(serie_pred_2017, series = "Forecast 2017", color = "red", size = 0.9, alpha = 0.7) +
  autolayer(serie_pred_2018, series = "Forecast 2018", color = "blue", size = 0.9, alpha = 0.7) +
  autolayer(serie_pred_2019, series = "Forecast 2019", color = "#006600", size = 0.9, alpha = 0.7) +
  labs(
    x = "Epidemiological week",
    y = "Hospital discharges",
    color = "Series"
  ) + theme_bw()
