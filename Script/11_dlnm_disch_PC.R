# 1. Libraries  --------------------------------------------------------------

## 1.1 Data manipulation and transformation ----

library(dplyr)       # Data manipulation
library(tidyr)       # Data structure transformation
library(tidyverse)   # Core packages for data analysis

## 1.2 Visualization and graphics ----

library(ggplot2)     # Grammar of graphics plots
library(ggseas)      # Seasonal decomposition visualizations
library(scales)      # Axis scale customization
library(viridis)     # Perceptually uniform color palettes
library(corrplot)    # Correlation matrix plots
library(GGally)      # Extensions for ggplot2 (e.g., ggpairs)

## 1.3 Statistical analysis and modeling ----

library(car)         # Linear model diagnostics
library(dlnm)        # Distributed lag nonlinear models
library(forecast)    # Time series forecasting
library(glmmTMB)     # Generalized linear mixed models
library(lme4)        # Linear mixed effects models
library(mgcv)        # Generalized additive models (GAM)
library(gamlss)      # Generalized additive models for location, scale, shape
library(MASS)        # Generalized linear models and distributions
library(splines)     # Cubic spline generation
library(urca)        # Unit root tests
library(tseries)     # Time series analysis tools

## 1.4 Information analysis and model metrics ----

library(energy)      # Distance correlation (dCor)
library(infotheo)    # Mutual information (MI)
library(Metrics)     # Model evaluation metrics

## 1.5 Spatial data handling ----

library(sf)          # Simple features for spatial vector data

## 1.6 Table creation ----

library(kableExtra)  # Table formatting for LaTeX/HTML
library(DT)          # Interactive HTML tables


# 2. Load data ---------------------------------------------------------------

## 2.1 General dataset ----
load(file = "datos_final1_para_analizar.RData")

## 2.2 Spatial data ----
subregion_st <- st_read("Subegiones Climáticas.shp")

# 3. Data aggregation by region and subregion -------------------------------

#' This block summarizes weekly statistics for each region and subregion.
#' For every year and epidemiological week, it calculates the total number
#' of hospital discharges, the relative risk of discharges, population,
#' and the mean values of precipitation, temperature, aerosol concentration,
#' and relative humidity.


## 3.1 Subregional data by epidemiological week ----

data_pc_subreg <- datos_finales_subregion %>%
  filter(reg_climat == "PC", epi.week != 53) %>%
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


## 3.2 Aggregated data by region and epidemiological week ----

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


#' Add a region identifier column to each dataset and rename for clarity

data_region1$region <- "PC"
data_region2$region <- "PN"
data_region3$region <- "PS"
data_region4$region <- "RMS"
data_region5$region <- "VC"
data_region6$region <- "RN"
data_region7$region <- "RA"

data_pc <- data_region1
data_pn <- data_region2
data_ps <- data_region3
data_rms <- data_region4
data_vc <- data_region5
data_rn <- data_region6
data_ra <- data_region7

# 4. Correlation analysis using permutation tests -----------------------------

#' This block defines a correlation testing function between lagged predictors
#' and the response variable for region PC, using three methods:
#' - Pearson correlation (linear)
#' - Distance correlation (dCor) (nonlinear)
#' - Mutual information (MI) (nonlinear)
#' Statistical significance is evaluated via permutation tests.


## 4.1 Functions for crossbasis parametrization -------------------------------

#' ------------------------------------------------------------------------------
#' Function: perm_test
#' ------------------------------------------------------------------------------
#' Description:
#' Performs a permutation test to assess statistical dependence between two numeric vectors.
#' Supports three methods: Pearson correlation, distance correlation (dCor), and mutual information (MI).
#'
#' Arguments:
#' - x: numeric vector (independent variable).
#' - y: numeric vector (dependent variable).
#' - method: character, method to use: "pearson" (default), "dcor", or "MI".
#' - n_perm: integer, number of permutations to run (default is 50).
#'
#' Returns:
#' - A numeric p-value between 0 and 1.
#'
#' Details:
#' - For "pearson", |cor(x, y)| is computed using random permutations of x.
#' - For "dcor", distance correlation is computed via energy::dcor().
#' - For "MI", mutual information is estimated using infotheo::mutinformation() with discretized values.
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
#' Function: buscar_mejor_modelo()
#' ------------------------------------------------------------------------------
#' Description:
#' Identifies the best model among combinations of distributed lag nonlinear model (DLNM) structures,
#' by testing various exposure and lag functions using Poisson and Negative Binomial regressions.
#' The best model is selected based on predictive and fit metrics.
#'
#' Parameters:
#' - data: input data.frame
#' - variable: name of the exposure variable (character)
#' - respuesta: name of the response variable (character)
#' - max_lag: maximum number of lags to test
#' - max_df_var: maximum degrees of freedom for the exposure variable
#' - max_df_lag: maximum degrees of freedom for the lag structure
#'
#' Returns:
#' - A list with:
#'   - mejor_modelo: data.frame row corresponding to the best model
#'   - resultado_general: data.frame of all evaluated model combinations
buscar_mejor_modelo <- function(data, variable, respuesta, max_lag, max_df_var, max_df_lag) {
  
  #' Initialize results storage
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
  
  #' Function to evaluate model fit and predictive metrics
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
  
  #' Linear exposure with strata for lag structure
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
  
  #' Cubic spline functions for both exposure and lag
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
  
  #' Identify the best model based on score
  mejor_modelo <- resultados[resultados$score == max(resultados$score), ]
  
  cat("\nBest model based on score:\n")
  print(mejor_modelo)
  
  list(
    mejor_modelo = mejor_modelo,
    resultado_general = resultados
  )
}
## 4.2 Correlation analysis for mean temperature ----

results_Temp <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)

#' Compute correlation coefficients and p-values for each lag

max_lag <- 14
for (lag in 1:max_lag) {
  X_lagged <- head(data_pc$Temp_media, length(data_pc$Temp_media) - lag)
  Y_lagged <- tail(data_pc$egreso_semana, length(data_pc$egreso_semana) - lag)
  
  # Pearson correlation and p-value
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

#' Display correlation results
results_Temp

#' Compute maximum and minimum values for each correlation series
max_pearson <- max(results_Temp$Pearson)
min_pearson <- min(results_Temp$Pearson)

max_dcor <- max(results_Temp$dCor)
min_dcor <- min(results_Temp$dCor)

max_mi <- max(results_Temp$MI)
min_mi <- min(results_Temp$MI)

#' Correlation plots for each method
ggplot(results_Temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature lag (k)", y = "Correlation coefficient") +
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

#' Run best DLNM model search for temperature up to lag 9
res <- buscar_mejor_modelo(
  data = data_pc,
  variable = "Temp_media",
  respuesta = "egreso_semana",
  max_lag = 9,
  max_df_var = 3,
  max_df_lag = 3
)

#' Display model search results using DT::datatable
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",           # Text color
    backgroundColor = "lightgray"  # Light background for readability
  )
## 4.3 Correlation analysis for precipitation and discharges ----

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
  # Lag the series to evaluate delay effects
  X_lagged <- head(data_pc$precip_media, length(data_pc$precip_media) - lag)
  Y_lagged <- tail(data_pc$egreso_semana, length(data_pc$egreso_semana) - lag)
  
  # Pearson correlation and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  # Distance correlation and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  # Mutual information and p-value
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

#' Show results
results_Prec

#' Compute max and min values of each series
max_pearson <- max(results_Prec$Pearson)
min_pearson <- min(results_Prec$Pearson)

max_dcor <- max(results_Prec$dCor)
min_dcor <- min(results_Prec$dCor)

max_mi <- max(results_Prec$MI)
min_mi <- min(results_Prec$MI)

#' Plot correlations by lag
ggplot(results_Prec, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Precipitation lag (k)", y = "Correlation coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text", x = which.max(results_Prec$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5) +
  annotate("text", x = which.min(results_Prec$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5) +
  annotate("text", x = which.max(results_Prec$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5) +
  annotate("text", x = which.min(results_Prec$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5) +
  annotate("text", x = which.max(results_Prec$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5) +
  annotate("text", x = which.min(results_Prec$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5)

#' Best model search for precipitation and hospital discharges
res <- buscar_mejor_modelo(
  data_pc,
  "precip_media",
  "egreso_semana",
  max_lag = 9,
  max_df_var = 3,
  max_df_lag = 3
)

#' Show results in interactive table
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )


## 4.4 Correlation analysis for relative humidity (RH) ----

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

#' Compute correlation coefficients and p-values for each lag
for (lag in 1:max_lag) {
  X_lagged <- head(data_pc$RH, length(data_pc$RH) - lag)
  Y_lagged <- tail(data_pc$egreso_semana, length(data_pc$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
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

#' Show results
results_RH

#' Compute max and min values
max_pearson <- max(results_RH$Pearson)
min_pearson <- min(results_RH$Pearson)

max_dcor <- max(results_RH$dCor)
min_dcor <- min(results_RH$dCor)

max_mi <- max(results_RH$MI)
min_mi <- min(results_RH$MI)

#' Correlation plot by lag
ggplot(results_RH, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    x = "Relative Humidity lag (k)",
    y = "Correlation coefficient"
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
  annotate("text", x = which.max(results_RH$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5) +
  annotate("text", x = which.min(results_RH$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5) +
  annotate("text", x = which.max(results_RH$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5) +
  annotate("text", x = which.min(results_RH$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5) +
  annotate("text", x = which.max(results_RH$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5) +
  annotate("text", x = which.min(results_RH$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5)

#' Best DLNM model evaluation for relative humidity (RH) using max lag = 9
res <- buscar_mejor_modelo(
  data_pc,
  "RH",
  "egreso_semana",
  max_lag = 9,
  max_df_var = 3,
  max_df_lag = 3
)

#' Display results using an interactive datatable
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

## 4.5 Correlation analysis for AOD index (Aerosol) -----

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
  X_lagged <- head(data_pc$aerosol, length(data_pc$aerosol) - lag)
  Y_lagged <- tail(data_pc$egreso_semana, length(data_pc$egreso_semana) - lag)
  
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

#' Print results to console
results_AOD

#' Calculate max and min values for each series
max_pearson <- max(results_AOD$Pearson)
min_pearson <- min(results_AOD$Pearson)

max_dcor <- max(results_AOD$dCor)
min_dcor <- min(results_AOD$dCor)

max_mi <- max(results_AOD$MI)
min_mi <- min(results_AOD$MI)

#' Plot correlation results by lag
ggplot(results_AOD, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    x = "AOD index lag (k)",
    y = "Correlation coefficient"
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
  
  #' Annotate max and min values for each method
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

#' DLNM model evaluation for AOD index (aerosol)
res <- buscar_mejor_modelo(
  data_pc,              # Dataset
  "aerosol",            # Predictor variable
  "egreso_semana",      # Response variable
  max_lag = 11,         # Maximum lag to consider
  max_df_var = 3,       # Maximum degrees of freedom for exposure spline
  max_df_lag = 3        # Maximum degrees of freedom for lag spline
)

#' Display model results in interactive datatable
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
#' variables (temperature, precipitation, relative humidity, and AOD), using
#' negative binomial models with distributed lags.
#' 
#' Inputs:
#' - data: full dataset.
#' - respuesta: name of the response variable (count).
#' - temp_var, prec_var, rh_var, aod_var: names of the predictor variables.
#' - max_lag_*: maximum lag for each predictor.
#' - max_df_var_*: maximum degrees of freedom for the variable function.
#' - max_df_lag_*: maximum degrees of freedom for the lag function.
#'
#' Output:
#' - A list containing a data.frame named 'resultados_completos' with the
#'   model fit metrics (AIC, BIC, MAE, MSE, RMSE, R2, MAPE, logLik, score)
#'   for each combination of evaluated parameters.
#'
#' Details:
#' - The fitted model is of type glm.nb.
#' - Includes seasonal effects using as.factor(epi.week).
#' - Crossbasis functions are generated using the crossbasis function (dlnm package).
#' ------------------------------------------------------------------------------

discrete_modeling_parameters <- function(
    data, respuesta,
    temp_var, prec_var, rh_var, aod_var,
    max_lag_temp, max_df_var_temp, max_df_lag_temp,
    max_lag_prec, max_df_var_prec, max_df_lag_prec,
    max_lag_rh, max_df_var_rh, max_df_lag_rh,
    max_lag_AOD, max_df_var_AOD, max_df_lag_AOD
) {
  
  # Define variables and their parameters
  variables <- list(
    temp = list(var = temp_var, max_lag = max_lag_temp, max_df_var = max_df_var_temp, max_df_lag = max_df_lag_temp),
    prec = list(var = prec_var, max_lag = max_lag_prec, max_df_var = max_df_var_prec, max_df_lag = max_df_lag_prec),
    rh   = list(var = rh_var,   max_lag = max_lag_rh,   max_df_var = max_df_var_rh,   max_df_lag = max_df_lag_rh),
    aod  = list(var = aod_var,  max_lag = max_lag_AOD,  max_df_var = max_df_var_AOD,  max_df_lag = max_df_lag_AOD)
  )
  
  # Dataframe to store results
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
  
  # Function to evaluate model and calculate metrics
  # R2 term is removed from the score
  evaluar_modelo <- function(modelo, data_filtrada, respuesta, tipo, params) {
    predicciones <- predict(modelo, type = "response")
    obs <- data_filtrada[[respuesta]]
    
    MAE    <- mean(abs(obs - predicciones))
    MSE    <- mean((obs - predicciones)^2)
    RMSE   <- sqrt(MSE)
    R2     <- cor(obs, predicciones)^2
    AIC_val <- AIC(modelo)
    BIC_val <- BIC(modelo)
    MAPE_val <- mean(abs((obs - predicciones) / obs)) * 100
    ll <- as.numeric(logLik(modelo))
    
    if (ll < 0) {
      logLik_term <- 1/abs(ll)
    } else {
      logLik_term <- ll
    }
    
    score <- (1/AIC_val) + (1/BIC_val) + (1/MAE) + (1/MSE) + (1/RMSE) +
      (1/MAPE_val) + logLik_term
    
    return(data.frame(
      modelo = tipo,
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
  
  # Function to generate crossbasis using max lag specified
  generar_crossbasis <- function(variable_info, data) {
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
  
  crossbases <- lapply(variables, generar_crossbasis, data = data)
  
  max_na_lags <- max(sapply(crossbases, function(cb) 
    max(sapply(cb, function(x) max(attr(x$crossbasis, "lag"))))
  ))
  data_filtrada <- data[(max_na_lags + 1):nrow(data), ]
  
  crossbases_filtradas <- lapply(crossbases, function(cb) {
    lapply(cb, function(x) list(
      crossbasis = x$crossbasis[(max_na_lags + 1):nrow(x$crossbasis), , drop = FALSE],
      fun_var = x$fun_var, df_var = x$df_var,
      fun_lag = x$fun_lag, df_lag = x$df_lag
    ))
  })
  
  for (cb_temp in crossbases_filtradas$temp) {
    for (cb_prec in crossbases_filtradas$prec) {
      for (cb_rh in crossbases_filtradas$rh) {
        for (cb_aod in crossbases_filtradas$aod) {
          
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
          
          modelo_ajustado <- glm.nb(data_filtrada[[respuesta]] ~ 
                                      cb_temp$crossbasis + cb_prec$crossbasis + 
                                      cb_rh$crossbasis + cb_aod$crossbasis +
                                      as.factor(epi.week), 
                                    link = "log", data = data_filtrada)
          
          resultados <- rbind(resultados, 
                              evaluar_modelo(modelo_ajustado, data_filtrada, respuesta, "modelo_optim", params))
        }
      }
    }
  }
  
  return(list(resultados_completos = resultados))
}
## 4.6 Scatter plots for variables with optimal lags ----

#' Scatter plot for Temp_media (lag 9)
d_temp <- data_pc %>%
  mutate(Temp_media_lag9 = lag(Temp_media, 9)) %>%
  slice(10:n()) # Remove missing values due to lag

ggplot(d_temp, aes(x = Temp_media_lag9, y = log(egreso_semana))) +
  geom_point(color = "#2c7fb8") + # Blue color
  labs(x = "Mean temperature (lag 9)", y = "Log of weekly discharges") +
  theme_bw()

#' Scatter plot for precip_media (lag 9)
d_precip <- data_pc %>%
  mutate(precip_media_lag9 = lag(precip_media, 9)) %>%
  slice(10:n())

ggplot(d_precip, aes(x = precip_media_lag9, y = log(egreso_semana))) +
  geom_point(color = "#f03b20") + # Red color
  labs(x = "Mean precipitation (lag 9)", y = "Log of weekly discharges") +
  theme_bw()

#' Scatter plot for aerosol (lag 11)
d_aerosol <- data_pc %>%
  mutate(aerosol_lag11 = lag(aerosol, 11)) %>%
  slice(12:n())

ggplot(d_aerosol, aes(x = aerosol_lag11, y = log(egreso_semana))) +
  geom_point(color = "#31a354") + # Green color
  labs(x = "AOD index (lag 11)", y = "Log of weekly discharges") +
  theme_bw()

#' Scatter plot for RH (lag 9)
d_rh <- data_pc %>%
  mutate(RH_lag9 = lag(RH, 9)) %>%
  slice(10:n())

ggplot(d_rh, aes(x = RH_lag9, y = log(egreso_semana))) +
  geom_point(color = "#756bb1") + # Purple color
  labs(x = "Relative humidity (lag 9)", y = "Log of weekly discharges") +
  theme_bw()

# 5. Cross-basis functions for climatic variables  ----------------------------

#' This code block creates cross-basis matrices using cubic splines
#' to model the non-linear relationship between climatic variables and hospital discharges.
#' Different lags are set for each variable according to the data's nature.

## 5.1 Mean temperature (lag = 9, cubic spline) ----
cb_temp_egre_pc <- crossbasis(
  data_pc$Temp_media,
  lag = 9,
  argvar = list(fun = "ns", df = 3), # cubic spline for the variable
  arglag = list(fun = "ns", df = 3)  # cubic spline for the lags
)

## 5.2 Mean precipitation (lag = 9, cubic spline) ----
cb_prec_egre_pc <- crossbasis(
  data_pc$precip_media,
  lag = 9,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.3 Relative humidity (lag = 9, cubic spline) ----
cb_rh_egre_pc <- crossbasis(
  data_pc$RH,
  lag = 9,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.4 Aerosol Optical Depth index AOD (lag = 11, cubic spline) ----
cb_aod_egre_pc <- crossbasis(
  data_pc$aerosol,
  lag = 11,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

# 6. DLNM model fitting for hospital discharges ----

#' This code block fits DLNM models using
#' Poisson and Negative Binomial distributions to evaluate
#' the effect of climatic variables on hospital discharges.

#' Remove the first 11 observations to complete the lags
df_pc_fitted_egresos <- data_pc %>%
  slice(-(1:11))

#' ----------------------------------------------------
## 6.1 DLNM model with Poisson distribution and seasonal factors ----
#' ----------------------------------------------------
dlnm_model_poi_estac_pc <- glm(
  egreso_semana ~ cb_temp_egre_pc + cb_prec_egre_pc +
    cb_rh_egre_pc + cb_aod_egre_pc +
    as.factor(epi.week),
  family = poisson(link = "log"),
  data = data_pc
)

#' ----------------------------------------------------
## 6.2 DLNM model with Poisson distribution (no seasonal factors) ----
#' ----------------------------------------------------
dlnm_model_poi_pc <- glm(
  egreso_semana ~ cb_temp_egre_pc + cb_prec_egre_pc +
    cb_rh_egre_pc + cb_aod_egre_pc,
  family = poisson(link = "log"),
  data = data_pc
)

#' ----------------------------------------------------
## 6.3 DLNM model with Negative Binomial distribution and seasonal factors ----
#' ----------------------------------------------------
dlnm_model_nb_estac_pc <- glm.nb(
  egreso_semana ~ cb_temp_egre_pc + cb_prec_egre_pc +
    cb_rh_egre_pc + cb_aod_egre_pc +
    as.factor(epi.week),
  data = data_pc
)

#' ----------------------------------------------------
## 6.4 DLNM model with Negative Binomial distribution (no seasonal factors) ----
#' ----------------------------------------------------
dlnm_model_nb_pc <- glm.nb(
  egreso_semana ~ cb_temp_egre_pc + cb_prec_egre_pc +
    cb_rh_egre_pc + cb_aod_egre_pc,
  data = data_pc
)

# 7. Fitting GLM models (Poisson and Negative Binomial) -----------------------

#' This block fits two classical GLM models (no lag structure):
#' - Poisson model
#' - Negative Binomial model
#' Used as a comparative reference to DLNM models.

## 7.1 GLM model with Poisson distribution -----
glm_model_poi_pc <- glm(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  family = poisson(link = "log"),
  data = df_pc_fitted_egresos
)

## 7.2 GLM model with Negative Binomial distribution ----
glm_model_nb_pc <- glm.nb(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  data = df_pc_fitted_egresos
)

# 8. Error metrics calculation for models  --------------------------

#' Generate fitted values for each model
df_pc_fitted_egresos <- df_pc_fitted_egresos %>%
  mutate(
    ajustado_dlnm_model_poi_estac_pc = fitted(dlnm_model_poi_estac_pc),
    ajustado_dlnm_model_poi_pc = fitted(dlnm_model_poi_pc),
    ajustado_dlnm_model_nb_estac_pc = fitted(dlnm_model_nb_estac_pc),
    ajustado_dlnm_model_nb_pc = fitted(dlnm_model_nb_pc),
    ajustado_glm_model_poi_pc = fitted(glm_model_poi_pc),
    ajustado_glm_model_nb_pc = fitted(glm_model_nb_pc)
  )
## 8.1 Function to compute error and information metrics ----

#' ------------------------------------------------------------------------------
#' calculate_metrics()
#' ------------------------------------------------------------------------------
#' Function to compute performance metrics for a fitted model and its
#' associated predictions. Includes classical error metrics, information
#' criteria, and a composite score summarizing overall performance.
#'
#' Parameters:
#' - model: fitted model object (e.g., glm, glm.nb)
#' - predictions: numeric vector of model predictions
#' - obs: numeric vector of observed values
#' - model_name: identifier string for the evaluated model
#'
#' Returns:
#' - Data frame with metrics: MAE, MSE, RMSE, MAPE, AIC, BIC, logLik, and Score
#' ------------------------------------------------------------------------------

calculate_metrics <- function(model, predictions, obs, model_name) {
  #' --------------------------------------------------------------------------
  #' Error metrics
  #' --------------------------------------------------------------------------
  MAE <- mean(abs(obs - predictions))
  MSE <- mean((obs - predictions)^2)
  RMSE <- sqrt(MSE)
  
  #' MAPE - includes correction to avoid division by zero
  epsilon <- 1e-10
  MAPE_val <- mean(abs((obs - predictions) / (obs + epsilon))) * 100
  
  #' --------------------------------------------------------------------------
  #' Information criteria and log-likelihood with error control
  #' --------------------------------------------------------------------------
  AIC_val <- tryCatch(AIC(model), error = function(e) NA)
  BIC_val <- tryCatch(BIC(model), error = function(e) NA)
  ll <- tryCatch(as.numeric(logLik(model)), error = function(e) NA)
  
  #' --------------------------------------------------------------------------
  #' Composite score: inverses + adjusted logLik
  #' --------------------------------------------------------------------------
  logLik_term <- if (!is.na(ll) && ll < 0) 1 / abs(ll) else if (!is.na(ll)) ll else 0
  score <- (1 / MAE) + (1 / MSE) + (1 / RMSE) + (1 / MAPE_val) + logLik_term
  
  #' --------------------------------------------------------------------------
  #' Return as data.frame
  #' --------------------------------------------------------------------------
  data.frame(
    model = model_name,
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

#' Compute metrics for each fitted model
results <- rbind(
  calculate_metrics(
    dlnm_model_poi_estac_pc,
    df_pc_fitted_egresos$ajustado_dlnm_model_poi_estac_pc,
    df_pc_fitted_egresos$egreso_semana,
    "DLNM_Poi_est_PC"
  ),
  calculate_metrics(
    dlnm_model_poi_pc,
    df_pc_fitted_egresos$ajustado_dlnm_model_poi_pc,
    df_pc_fitted_egresos$egreso_semana,
    "DLNM_Poi_PC"
  ),
  calculate_metrics(
    dlnm_model_nb_estac_pc,
    df_pc_fitted_egresos$ajustado_dlnm_model_nb_estac_pc,
    df_pc_fitted_egresos$egreso_semana,
    "DLNM_NB_est_PC"
  ),
  calculate_metrics(
    dlnm_model_nb_pc,
    df_pc_fitted_egresos$ajustado_dlnm_model_nb_pc,
    df_pc_fitted_egresos$egreso_semana,
    "DLNM_NB_PC"
  ),
  calculate_metrics(
    glm_model_poi_pc,
    df_pc_fitted_egresos$ajustado_glm_model_poi_pc,
    df_pc_fitted_egresos$egreso_semana,
    "GLM_Poi_PC"
  ),
  calculate_metrics(
    glm_model_nb_pc,
    df_pc_fitted_egresos$ajustado_glm_model_nb_pc,
    df_pc_fitted_egresos$egreso_semana,
    "GLM_NB_PC"
  )
)
#' Sort models by best score
results <- results %>%
  arrange(desc(Score))

#' Display results using an interactive table
datatable(results, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(results),
    color = "black",
    backgroundColor = "lightgray"
  )

## 8.2 Optimal model ----
dlnm_model_pc <- dlnm_model_nb_estac_pc

# Save the models in the folder
save(
  dlnm_model_nb_estac_pc,
  dlnm_model_nb_pc,
  dlnm_model_pc,
  dlnm_model_poi_estac_pc,
  dlnm_model_poi_pc,
  glm_model_nb_pc,
  glm_model_poi_pc,
  file = "fixed_effects_models/models_for_discharges_pc.RData"
)

# 9. Residual diagnostics for the optimal DLNM model ----

#' Create time series for discharges and fitted values
egresos_pc <- ts(
  df_pc_fitted_egresos$egreso_semana,
  start = c(2000, 12),
  frequency = 52
)

egresos_fitt_dlnm_pc <- ts(
  fitted(dlnm_model_pc),
  start = c(2000, 12),
  frequency = 52
)

#' Pearson and deviance residuals
resid_pearson_pc <- residuals(
  dlnm_model_pc,
  type = "pearson"
)

resid_deviance_pc <- residuals(
  dlnm_model_pc,
  type = "deviance"
)

#' Convert residuals to time series
resid_pearson_st <- ts(
  resid_pearson_pc,
  start = c(2000, 12),
  frequency = 52
)

resid_deviance_st <- ts(
  resid_deviance_pc,
  start = c(2000, 12),
  frequency = 52
)

## 9.1 Scatter plots ----------------------------------------------

#' Scatter plot: Pearson residuals vs. fitted values
plot(
  fitted(dlnm_model_pc), resid_pearson_pc,
  xlab = "Fitted values",
  ylab = "Pearson residuals"
)
abline(h = 0, col = "red")

#' Scatter plot: Deviance residuals vs. fitted values
plot(
  fitted(dlnm_model_pc), resid_deviance_pc,
  xlab = "Fitted values",
  ylab = "Deviance residuals"
)
abline(h = 0, col = "red")

## 9.2 Autocorrelation ----

#' ACF and PACF for Pearson residuals
ggtsdisplay(resid_pearson_st)

#' ACF and PACF for Deviance residuals
ggtsdisplay(resid_deviance_st)

#' Ljung-Box test for autocorrelation
ljbox_pearson_dlnm_pc <- Box.test(
  resid_pearson_pc,
  lag = 20,
  type = "Ljung-Box"
)

ljbox_deviance_dlnm_pc <- Box.test(
  resid_deviance_pc,
  lag = 20,
  type = "Ljung-Box"
)

## 9.3 Normality ----

#' QQ-plot for Pearson residuals
qqnorm(resid_pearson_pc, main = "QQ-plot of Pearson residuals")
qqline(resid_pearson_pc, col = "red")

#' QQ-plot for Deviance residuals
qqnorm(resid_deviance_pc, main = "QQ-plot of Deviance residuals")
qqline(resid_deviance_pc, col = "red")

# Shapiro-Wilk test
shapiro_test_pearson <- shapiro.test(resid_pearson_pc)
shapiro_test_deviance <- shapiro.test(resid_deviance_pc)

# Print test results
print(shapiro_test_pearson)
print(shapiro_test_deviance)

# 10. Model predictions ---------------------------------------------

## 10.1 Comparative plot of fitted series ----
#' ----------------------------------------------------------------------------
#' This code generates a comparison plot between observed values and fitted
#' values using DLNM Poisson models with and without seasonal factors,
#' as well as the GLM with NB distribution, for hospital discharges in region PC.
#' ----------------------------------------------------------------------------

#' Create time series for plotting
df_pc_fitted_egresos <- df_pc_fitted_egresos %>%
  mutate(
    egresos_ts = ts(egreso_semana, start = c(2000, 12), frequency = 52), # Observed discharges time series
    ajuste_DLNM = ts(ajustado_dlnm_model_nb_estac_pc, start = c(2000, 12), frequency = 52), # DLNM fit with seasonal factors
    ajuste_glm = ts(ajustado_glm_model_nb_pc, start = c(2000, 12), frequency = 52) # GLM fit
  )

#' Plot using autoplot to compare observed vs fitted values
autoplot(df_pc_fitted_egresos$egresos_ts, series = "Observed") +
  
  #' Line for DLNM fit with Negative Binomial and seasonal factors
  autolayer(df_pc_fitted_egresos$ajuste_DLNM,
            series = "DLNM NB fit"
  ) +
  
  #' Line for GLM NB fit
  autolayer(df_pc_fitted_egresos$ajuste_glm,
            series = "GLM NB fit"
  ) +
  
  #' Labels and theme
  labs(
    x = "time (weeks)",
    y = "hospital discharges"
  ) +
  theme_bw() + # White background
  
  #' Custom colors using consistent palette
  scale_color_manual(
    values = c(
      "Observed" = "green",
      "DLNM NB fit" = "blue",
      "GLM NB fit" = "red"
    )
  ) +
  
  #' Legend formatting
  theme(
    legend.title = element_blank(), # Remove legend title
    legend.position = "bottom" # Place legend at the bottom
  )


## 10.2 Lagged effect predictions  ----

#' ====================================================
#' This code block generates predictions from the fitted DLNM model
#' using crossbasis functions for temperature, precipitation,
#' relative humidity, and AOD.

#' ----------------------------------------------------
#' Predictions for mean temperature (crosspred)
#' ----------------------------------------------------
pred_dlnm_pc_temp <- crosspred(
  cb_temp_egre_pc,
  dlnm_model_pc,
  cen = mean(data_pc$Temp_media, na.rm = TRUE), # reference point (center)
  bylag = 0.1 # lag increments
)

#' ----------------------------------------------------
#' Predictions for mean precipitation (crosspred)
#' ----------------------------------------------------
pred_dlnm_pc_prec <- crosspred(
  cb_prec_egre_pc,
  dlnm_model_pc,
  cen = mean(data_pc$precip_media, na.rm = TRUE), # reference point (center)
  bylag = 0.1 # lag increments
)

#' ----------------------------------------------------
#' Predictions for relative humidity (crosspred)
#' ----------------------------------------------------
pred_dlnm_pc_rh <- crosspred(
  cb_rh_egre_pc,
  dlnm_model_pc,
  cen = mean(data_pc$RH, na.rm = TRUE), # reference point (center)
  bylag = 0.1 # lag increments
)

#' ----------------------------------------------------
#' Predictions for AOD index (crosspred)
#' ----------------------------------------------------
pred_dlnm_pc_aod <- crosspred(
  cb_aod_egre_pc,
  dlnm_model_pc,
  cen = mean(data_pc$aerosol, na.rm = TRUE), # reference point (center)
  bylag = 0.1 # lag increments
)

### 10.2.1 Predictions for mean temperature ----

#' ----------------------------------------------------
#' 3D plot (temperature - lag - relative risk)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_temp,
  xlab = "Mean temperature",
  zlab = "Relative risk (RR)",
  theta = 300, phi = 10, lphi = 100
)

#' ----------------------------------------------------
#' Contour plot (temperature - lag - RR)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_temp, "contour",
  xlab = "Mean temperature",
  ylab = "Lag",
  key.title = title("RR")
)

#' ----------------------------------------------------
#' Lag-response plot for a specific temperature value (31°C)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_temp, "slices",
  var = 31,
  col = 2,
  ylab = "Relative risk (RR)"
)

#' ----------------------------------------------------
#' Lag-response plot for a specific temperature value (27°C)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_temp, "slices",
  var = 27,
  col = 2,
  ylab = "Relative risk (RR)"
)

### 10.2.2 Predictions for mean precipitation ----

#' ----------------------------------------------------
#' 3D plot (precipitation - lag - relative risk)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_prec,
  xlab = "Mean precipitation",
  zlab = "Relative risk (RR)",
  theta = 280, phi = 10, lphi = 100
)

#' ----------------------------------------------------
#' Contour plot (precipitation - lag - RR)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_prec, "contour",
  xlab = "Mean precipitation",
  ylab = "Lag",
  key.title = title("RR")
)

#' ----------------------------------------------------
#' Lag-response plot for a specific precipitation value (5 mm)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_prec, "slices",
  var = 5,
  col = 2,
  ylab = "Relative risk (RR)"
)

#' ----------------------------------------------------
#' Lag-response plot for a specific precipitation value (55 mm)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_prec, "slices",
  var = 55,
  col = 2,
  ylab = "Relative risk (RR)"
)

### 10.2.3 Predictions for mean relative humidity ----

#' ----------------------------------------------------
#' 3D plot (humidity - lag - relative risk)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_rh,
  xlab = "Mean relative humidity",
  zlab = "Relative risk (RR)",
  theta = 300, phi = 10, lphi = 100
)

#' ----------------------------------------------------
#' Contour plot (humidity - lag - RR)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_rh, "contour",
  xlab = "Mean relative humidity",
  ylab = "Lag",
  key.title = title("RR")
)

#' ----------------------------------------------------
#' Lag-response plot for a specific humidity value (0.25)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_rh, "slices",
  var = 0.25,
  col = 2,
  ylab = "Relative risk (RR)"
)

#' ----------------------------------------------------
#' Lag-response plot for a specific humidity value (0.43)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_rh, "slices",
  var = 0.43,
  col = 2,
  ylab = "Relative risk (RR)"
)

### 10.2.4 Predictions for AOD index ----

#' ----------------------------------------------------
#' 3D plot (AOD - lag - relative risk)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_aod,
  xlab = "AOD index",
  zlab = "Relative risk (RR)",
  theta = 200, phi = 10, lphi = 100
)

#' ----------------------------------------------------
#' Contour plot (AOD - lag - RR)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_aod, "contour",
  xlab = "AOD index",
  ylab = "Lag",
  key.title = title("RR")
)

#' ----------------------------------------------------
#' Lag-response plot for a specific AOD value (1e-05)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_aod, "slices",
  var = 1e-05,
  col = 2,
  ylab = "Relative risk (RR)"
)

#' ----------------------------------------------------
#' Lag-response plot for a specific AOD value (4e-04)
#' ----------------------------------------------------
plot(
  pred_dlnm_pc_aod, "slices",
  var = 4e-04,
  col = 2,
  ylab = "Relative risk (RR)"
)
# 11. Forecasting ----

## 11.1 General data ----

# Construct dataframe with crossbasis covariates
df_con_crossbasis_egresos <- cbind(data_pc,
                                   setNames(as.data.frame(cb_temp_egre_pc), make.names(paste0("t_", colnames(cb_temp_egre_pc)), unique = TRUE)),
                                   setNames(as.data.frame(cb_prec_egre_pc), make.names(paste0("p_", colnames(cb_prec_egre_pc)), unique = TRUE)),
                                   setNames(as.data.frame(cb_rh_egre_pc), make.names(paste0("rh_", colnames(cb_rh_egre_pc)), unique = TRUE)),
                                   setNames(as.data.frame(cb_aod_egre_pc), make.names(paste0("aod_", colnames(cb_aod_egre_pc)), unique = TRUE))
)

# Remove NA from the dataset
df_con_crossbasis_egresos <- df_con_crossbasis_egresos %>%
  slice(-(1:11))

# Error metric function: computes MAE, MSE, RMSE, MAPE, SAMPE
calculate_errors <- function(obs, pred, model) {
  mae_val <- mae(obs, pred)
  mse_val <- mse(obs, pred)
  rmse_val <- rmse(obs, pred)
  mape_val <- mape(obs, pred) * 100
  sampe_val <- mean(2 * abs(pred - obs) / (abs(obs) + abs(pred))) * 100
  
  tibble(
    model = model,
    MAE = mae_val,
    MSE = mse_val,
    RMSE = rmse_val,
    MAPE = mape_val,
    SAMPE = sampe_val
  )
}

## 11.2 Forecast for 2017 ----

# Training-test split (excluding 2017)
df_train <- df_con_crossbasis_egresos %>% filter(year != 2017)
df_2017 <- df_con_crossbasis_egresos %>% filter(year == 2017)

# Get exposure column names (crossbasis)
columnas_exposicion_egresos <- colnames(df_con_crossbasis_egresos)[12:ncol(df_con_crossbasis_egresos)]

# Dynamic formula for DLNM model
formula_dlnm <- as.formula(
  paste("egreso_semana ~", paste(columnas_exposicion_egresos, collapse = " +"), "+ as.factor(epi.week)")
)

# DLNM model with crossbasis (excluding 2017)
dlnm_model_nb_estac_pc <- glm.nb(formula = formula_dlnm, data = df_train)

# Linear NB model without lags (excluding 2017)
glm_model_nb_pc <- glm.nb(egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week), data = df_train)

# Predictions on training data
df_train <- df_train %>%
  mutate(
    pred_dlnm_train = predict(dlnm_model_nb_estac_pc, type = "response"),
    pred_glm_train = predict(glm_model_nb_pc, type = "response")
  )

# Predictions on 2017
df_2017 <- df_2017 %>%
  mutate(
    pred_dlnm_2017 = predict(dlnm_model_nb_estac_pc, newdata = df_2017, type = "response"),
    pred_glm_2017 = predict(glm_model_nb_pc, newdata = df_2017, type = "response")
  )

### Error metrics ----
errors_2017 <- bind_rows(
  calculate_errors(df_train$egreso_semana, df_train$pred_glm_train, "GLM_train"),
  calculate_errors(df_train$egreso_semana, df_train$pred_dlnm_train, "DLNM_train"),
  calculate_errors(df_2017$egreso_semana, df_2017$pred_dlnm_2017, "DLNM_2017"),
  calculate_errors(df_2017$egreso_semana, df_2017$pred_glm_2017, "GLM_2017")
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

### Error metrics ----
errors_2018 <- bind_rows(
  calculate_errors(df_train$egreso_semana, df_train$pred_glm_train, "GLM_train"),
  calculate_errors(df_train$egreso_semana, df_train$pred_dlnm_train, "DLNM_train"),
  calculate_errors(df_2018$egreso_semana, df_2018$pred_dlnm_2018, "DLNM_2018"),
  calculate_errors(df_2018$egreso_semana, df_2018$pred_glm_2018, "GLM_2018")
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

### Error metrics ----
errors_2019 <- bind_rows(
  calculate_errors(df_train$egreso_semana, df_train$pred_glm_train, "GLM_train"),
  calculate_errors(df_train$egreso_semana, df_train$pred_dlnm_train, "DLNM_train"),
  calculate_errors(df_2019$egreso_semana, df_2019$pred_dlnm_2019, "DLNM_2019"),
  calculate_errors(df_2019$egreso_semana, df_2019$pred_glm_2019, "GLM_2019")
)

# Consolidated results
total_errors <- bind_rows(errors_2017, errors_2018, errors_2019)

datatable(total_errors, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(total_errors),
    color = "black",
    backgroundColor = "lightgray"
  )

## 11.5 Forecast plots ----

# Create full real series
real_series <- ts(df_con_crossbasis_egresos$egreso_semana, start = c(2000, 13), frequency = 52)

# Create ts forecasts, fill with NA except forecasted years
series_pred_2017 <- ts(rep(NA, length(real_series)), start = c(2000, 13), frequency = 52)
series_pred_2017[which(df_con_crossbasis_egresos$year == 2017)] <- df_2017$pred_dlnm_2017

series_pred_2018 <- ts(rep(NA, length(real_series)), start = c(2000, 13), frequency = 52)
series_pred_2018[which(df_con_crossbasis_egresos$year == 2018)] <- df_2018$pred_dlnm_2018

series_pred_2019 <- ts(rep(NA, length(real_series)), start = c(2000, 13), frequency = 52)
series_pred_2019[which(df_con_crossbasis_egresos$year == 2019)] <- df_2019$pred_dlnm_2019

# Plot
autoplot(real_series, series = "Observed", color = "green", size = 0.4) +
  autolayer(series_pred_2017, series = "Forecast 2017", color = "red", size = 0.9, alpha = 0.7) +
  autolayer(series_pred_2018, series = "Forecast 2018", color = "blue", size = 0.9, alpha = 0.7) +
  autolayer(series_pred_2019, series = "Forecast 2019", color = "#006600", size = 0.9, alpha = 0.7) +
  labs(
    x = "Epidemiological week",
    y = "Hospital discharges",
    color = "Series"
  ) 



