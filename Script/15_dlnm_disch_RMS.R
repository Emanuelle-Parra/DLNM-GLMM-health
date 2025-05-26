# 1. Libraries  --------------------------------------------------------------

## 1.1 Data manipulation and transformation ----

library(dplyr)       # Data manipulation
library(tidyr)       # Data transformation
library(tidyverse)   # Collection of data analysis packages

## 1.2 Visualization and graphics ----

library(ggplot2)     # ggplot-style plots
library(ggseas)      # Seasonal decomposition visualization
library(scales)      # Axis scale customization
library(viridis)     # Perceptually uniform color palettes
library(corrplot)    # Correlation plots
library(GGally)      # Enhanced plots for ggplot2 (e.g., matrix plots)

## 1.3 Statistical analysis and modeling ----

library(car)         # Linear model diagnostics
library(dlnm)        # Distributed Lag Nonlinear Models
library(forecast)    # Time series forecasting
library(glmmTMB)     # Generalized linear mixed models
library(lme4)        # Mixed-effects models
library(mgcv)        # GAM fitting
library(gamlss)      # GAMLSS models for zero-inflation
library(MASS)        # GLM fitting (e.g., glm.nb)
library(splines)     # Cubic splines
library(urca)        # Unit root tests
library(tseries)     # Time series analysis

## 1.4 Information theory and error metrics ----

library(energy)      # Distance correlation (dCor)
library(infotheo)    # Mutual Information (MI)
library(Metrics)     # Model performance metrics

## 1.5 Spatial data handling ----

library(sf)          # Simple Features for spatial data

## 1.6 Table creation ----

library(kableExtra)  # Table generation
library(DT)          # Interactive tables


# 2. Data loading ------------------------------------------------------------

## 2.1 General dataset ----
load(file = "datos_final1_para_analizar.RData")

## 2.2 Spatial data ----
subregion_st <- st_read("Subegiones Climáticas.shp")


# 3. Data generation by region and subregion ---------------------------

#' This block generates weekly summary statistics by region and subregion,
#' aggregating key variables per year and epidemiological week.
#' Variables include discharges, relative risk of infection, population,
#' precipitation, temperature, aerosol concentration, and relative humidity.


## 3.1 Subregion-level data by epidemiological week ----
data_rms_subreg <- datos_finales_subregion %>%
  filter(reg_climat == "RMS", epi.week != 53) %>%
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

## 3.2 Region-level data by epidemiological week ----

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

# Add region identifier and bind all datasets
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

#' This block defines a function to assess correlation between lagged predictors
#' and the response variable in region PC using various methods:
#' - Pearson correlation (linear)
#' - Distance correlation (dCor) (nonlinear)
#' - Mutual Information (MI) (nonlinear)
#' Statistical significance is assessed via permutation testing.

#' Define maximum lag to evaluate


## 4.1 Functions for crossbasis parameter setup ----

#' ------------------------------------------------------------------------------
#' Function: perm_test
#' ------------------------------------------------------------------------------
#' Description:
#' Performs a permutation test to evaluate the statistical dependency between two numeric vectors.
#' Supports three methods: Pearson correlation, distance correlation (dCor), and mutual information (MI).
#'
#' Arguments:
#' - x: numeric vector (independent variable).
#' - y: numeric vector (dependent variable).
#' - method: character string, method to use: "pearson" (default), "dcor", or "MI".
#' - n_perm: integer, number of permutations (default is 50).
#'
#' Returns:
#' - A numeric p-value between 0 and 1 from the permutation test.
#'
#' Details:
#' - For "pearson", computes |cor(x, y)| under permutations of x.
#' - For "dcor", uses dcor() from the energy package.
#' - For "MI", uses mutual information via mutinformation() from infotheo, after discretizing.
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
#' Function to identify the best DLNM model among combinations of nonlinear
#' exposure and lag structures, fitted using Poisson and Negative Binomial
#' regression. The optimal model is selected based on goodness-of-fit
#' and prediction performance metrics.
#'
#' Parameters:
#' - data: input data.frame
#' - variable: name of the exposure variable (character)
#' - respuesta: name of the response variable (character)
#' - max_lag: maximum number of lags to test
#' - max_df_var: max degrees of freedom for the exposure function
#' - max_df_lag: max degrees of freedom for the lag function
#'
#' Returns:
#' - A list with:
#'   - mejor_modelo: data.frame with the optimal model row
#'   - resultado_general: data.frame with all evaluated combinations
#' ------------------------------------------------------------------------------

buscar_mejor_modelo <- function(data, variable, respuesta, max_lag, max_df_var, max_df_lag) {
  
  # Create results container
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
  
  # Helper to compute metrics for a given model
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
  
  # First evaluation: linear function and strata lag
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
  
  # Evaluation with natural splines for both exposure and lag
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
  
  # Identify best model based on global score
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

# Compute correlations and p-values for each lag
max_lag <- 14
for (lag in 1:max_lag) {
  X_lagged <- head(data_rms$Temp_media, length(data_rms$Temp_media) - lag)
  Y_lagged <- tail(data_rms$egreso_semana, length(data_rms$egreso_semana) - lag)
  
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
  results_Temp <- rbind(
    results_Temp,
    data.frame(
      Lag = lag, Pearson = pearson_corr, Pearson_p = pearson_p,
      dCor = dcor_val, dCor_p = dcor_p,
      MI = MI_val, MI_p = MI_p
    )
  )
}

# Display results
results_Temp

# Compute max and min values for each method
max_pearson <- max(results_Temp$Pearson)
min_pearson <- min(results_Temp$Pearson)

max_dcor <- max(results_Temp$dCor)
min_dcor <- min(results_Temp$dCor)

max_mi <- max(results_Temp$MI)
min_mi <- min(results_Temp$MI)

# Correlation plot by method
ggplot(results_Temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature lag (k)", y = "Coefficient") +
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
           label = paste("Max: ", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Temp$Pearson), y = min_pearson,
           label = paste("Min: ", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_Temp$dCor), y = max_dcor,
           label = paste("Max: ", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Temp$dCor), y = min_dcor,
           label = paste("Min: ", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_Temp$MI), y = max_mi,
           label = paste("Max: ", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Temp$MI), y = min_mi,
           label = paste("Min: ", round(min_mi, 2)), color = "green", vjust = 1.5
  )

# Best DLNM model for temperature and discharges
res <- buscar_mejor_modelo(
  data = data_rms,
  variable = "Temp_media",
  respuesta = "egreso_semana",
  max_lag = 14,
  max_df_var = 3,
  max_df_lag = 3
)

# Display results in interactive table
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )


## 4.3 Correlation analysis for precipitation and discharges ----

results_Prec <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

# Compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  X_lagged <- head(data_rms$precip_media, length(data_rms$precip_media) - lag)
  Y_lagged <- tail(data_rms$egreso_semana, length(data_rms$egreso_semana) - lag)
  
  # Pearson correlation
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  # Distance correlation
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  # Mutual information
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

# Display results
results_Prec

# Compute extremes
max_pearson <- max(results_Prec$Pearson)
min_pearson <- min(results_Prec$Pearson)
max_dcor <- max(results_Prec$dCor)
min_dcor <- min(results_Prec$dCor)
max_mi <- max(results_Prec$MI)
min_mi <- min(results_Prec$MI)

# Correlation plot
ggplot(results_Prec, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Precipitation lag (k)", y = "Coefficient") +
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
           x = which.max(results_Prec$Pearson), y = max_pearson,
           label = paste("Max: ", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Prec$Pearson), y = min_pearson,
           label = paste("Min: ", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_Prec$dCor), y = max_dcor,
           label = paste("Max: ", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Prec$dCor), y = min_dcor,
           label = paste("Min: ", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_Prec$MI), y = max_mi,
           label = paste("Max: ", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_Prec$MI), y = min_mi,
           label = paste("Min: ", round(min_mi, 2)), color = "green", vjust = 1.5
  )

# DLNM model search for precipitation
res <- buscar_mejor_modelo(
  data_rms,
  "precip_media",
  "egreso_semana",
  max_lag = 11,
  max_df_var = 3,
  max_df_lag = 3
)

# Show results in interactive table
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

# Store results in a data frame
results_RH <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

# Compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  X_lagged <- head(data_rms$RH, length(data_rms$RH) - lag)
  Y_lagged <- tail(data_rms$egreso_semana, length(data_rms$egreso_semana) - lag)
  
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

# Display results
results_RH

# Compute max and min values
max_pearson <- max(results_RH$Pearson)
min_pearson <- min(results_RH$Pearson)

max_dcor <- max(results_RH$dCor)
min_dcor <- min(results_RH$dCor)

max_mi <- max(results_RH$MI)
min_mi <- min(results_RH$MI)

# Plot lagged correlations
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

# DLNM model evaluation for relative humidity (RH)
res <- buscar_mejor_modelo(
  data_rms,
  "RH",
  "egreso_semana",
  max_lag = 13,
  max_df_var = 3,
  max_df_lag = 3
)

# Display interactive table
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
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

# Compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  X_lagged <- head(data_rms$aerosol, length(data_rms$aerosol) - lag)
  Y_lagged <- tail(data_rms$egreso_semana, length(data_rms$egreso_semana) - lag)
  
  # Pearson and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  # dCor and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  # MI and p-value
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  # Store results
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

# Show results
results_AOD

# Max and min for each series
max_pearson <- max(results_AOD$Pearson)
min_pearson <- min(results_AOD$Pearson)
max_dcor <- max(results_AOD$dCor)
min_dcor <- min(results_AOD$dCor)
max_mi <- max(results_AOD$MI)
min_mi <- min(results_AOD$MI)

# Correlation plots
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
           label = paste("Max: ", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_AOD$Pearson), y = min_pearson,
           label = paste("Min: ", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_AOD$dCor), y = max_dcor,
           label = paste("Max: ", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_AOD$dCor), y = min_dcor,
           label = paste("Min: ", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_AOD$MI), y = max_mi,
           label = paste("Max: ", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_AOD$MI), y = min_mi,
           label = paste("Min: ", round(min_mi, 2)), color = "green", vjust = 1.5
  )

# DLNM model evaluation for AOD index
res <- buscar_mejor_modelo(
  data_rms,
  "aerosol",
  "egreso_semana",
  max_lag = 11,
  max_df_var = 3,
  max_df_lag = 3
)

# Show interactive results table
datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
#' ------------------------------------------------------------------------------
#' Function: discrete_modeling_parameters()
#' ------------------------------------------------------------------------------
#' Evaluates multiple combinations of nonlinear base functions for climatic
#' variables (temperature, precipitation, relative humidity, and AOD), using
#' negative binomial models with distributed lags.
#' 
#' Inputs:
#' - data: complete dataset.
#' - respuesta: name of the response variable (count).
#' - temp_var, prec_var, rh_var, aod_var: names of the predictor variables.
#' - max_lag_*: maximum lag for each predictor.
#' - max_df_var_*: maximum degrees of freedom for the variable function.
#' - max_df_lag_*: maximum degrees of freedom for the lag function.
#'
#' Output:
#' - A list containing a data.frame called 'resultados_completos' with
#'   performance metrics (AIC, BIC, MAE, MSE, RMSE, R2, MAPE, logLik, score)
#'   for each combination of parameters evaluated.
#'
#' Details:
#' - The fitted model is of type glm.nb.
#' - Includes seasonal effects via as.factor(epi.week).
#' - Uses crossbasis matrices generated with the crossbasis() function from the dlnm package.
#' ------------------------------------------------------------------------------

discrete_modeling_parameters <- function(
    data, respuesta,
    temp_var, prec_var, rh_var, aod_var,
    max_lag_temp, max_df_var_temp, max_df_lag_temp,
    max_lag_prec, max_df_var_prec, max_df_lag_prec,
    max_lag_rh, max_df_var_rh, max_df_lag_rh,
    max_lag_AOD, max_df_var_AOD, max_df_lag_AOD
) {
  
  # Define predictors and their parameter settings
  variables <- list(
    temp = list(var = temp_var, max_lag = max_lag_temp, max_df_var = max_df_var_temp, max_df_lag = max_df_lag_temp),
    prec = list(var = prec_var, max_lag = max_lag_prec, max_df_var = max_df_var_prec, max_df_lag = max_df_lag_prec),
    rh   = list(var = rh_var,   max_lag = max_lag_rh,   max_df_var = max_df_var_rh,   max_df_lag = max_df_lag_rh),
    aod  = list(var = aod_var,  max_lag = max_lag_AOD,  max_df_var = max_df_var_AOD,  max_df_lag = max_df_lag_AOD)
  )
  
  # Create results data frame
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
  
  # Internal function to fit the model and compute performance metrics
  evaluar_modelo <- function(modelo, data_filtrada, respuesta, tipo, params) {
    predicciones <- predict(modelo, type = "response")
    obs <- data_filtrada[[respuesta]]
    
    MAE <- mean(abs(obs - predicciones))
    MSE <- mean((obs - predicciones)^2)
    RMSE <- sqrt(MSE)
    R2 <- cor(obs, predicciones)^2
    AIC_val <- AIC(modelo)
    BIC_val <- BIC(modelo)
    MAPE_val <- mean(abs((obs - predicciones) / obs)) * 100
    ll <- as.numeric(logLik(modelo))
    
    # Use reciprocal of absolute log-likelihood when negative
    logLik_term <- if (ll < 0) 1 / abs(ll) else ll
    
    score <- (1/AIC_val) + (1/BIC_val) + (1/MAE) + (1/MSE) + (1/RMSE) + (1/MAPE_val) + logLik_term
    
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
  
  # Internal function to generate crossbasis combinations
  generar_crossbasis <- function(variable_info, data) {
    var <- variable_info$var
    max_lag <- variable_info$max_lag
    max_df_var <- variable_info$max_df_var
    max_df_lag <- variable_info$max_df_lag
    
    bases <- list()
    
    # Linear-linear crossbasis
    bases[["lin_lin"]] <- list(
      crossbasis = crossbasis(data[[var]], lag = max_lag,
                              argvar = list(fun = "lin"),
                              arglag = list(fun = "lin")),
      fun_var = "lin", df_var = NA,
      fun_lag = "lin", df_lag = NA
    )
    
    # Natural cubic splines for variable and lag
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
  
  # Generate crossbasis matrices for all predictors
  crossbases <- lapply(variables, generar_crossbasis, data = data)
  
  # Determine the maximum lag to filter dataset accordingly
  max_na_lags <- max(sapply(crossbases, function(cb)
    max(sapply(cb, function(x) max(attr(x$crossbasis, "lag"))))
  ))
  data_filtrada <- data[(max_na_lags + 1):nrow(data), ]
  
  # Filter crossbasis objects
  crossbases_filtradas <- lapply(crossbases, function(cb) {
    lapply(cb, function(x) list(
      crossbasis = x$crossbasis[(max_na_lags + 1):nrow(x$crossbasis), , drop = FALSE],
      fun_var = x$fun_var, df_var = x$df_var,
      fun_lag = x$fun_lag, df_lag = x$df_lag
    ))
  })
  
  # Nested loops to test all crossbasis combinations
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
          
          # Fit the model (Negative Binomial, can also use Poisson)
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

#' Scatter plot for Temp_media (lag 14)
d_temp <- data_rms %>%
  mutate(Temp_media_lag14 = lag(Temp_media, 14)) %>%
  slice(15:n()) # Remove NA values generated by lag

ggplot(d_temp, aes(x = Temp_media_lag14, y = log(egreso_semana))) +
  geom_point(color = "#2c7fb8") + # Blue color
  labs(x = "Mean temperature (lag 14)", y = "Log of weekly discharges") +
  theme_bw()


#' Scatter plot for precip_media (lag 11)
d_precip <- data_rms %>%
  mutate(precip_media_lag11 = lag(precip_media, 11)) %>%
  slice(12:n())

ggplot(d_precip, aes(x = precip_media_lag11, y = log(egreso_semana))) +
  geom_point(color = "#f03b20") + # Red color
  labs(x = "Mean precipitation (lag 11)", y = "Log of weekly discharges") +
  theme_bw()


#' Scatter plot for aerosol (lag 11)
d_aerosol <- data_rms %>%
  mutate(aerosol_lag11 = lag(aerosol, 11)) %>%
  slice(12:n())

ggplot(d_aerosol, aes(x = aerosol_lag11, y = log(egreso_semana))) +
  geom_point(color = "#31a354") + # Green color
  labs(x = "AOD index (lag 11)", y = "Log of weekly discharges") +
  theme_bw()


#' Scatter plot for RH (lag 13)
d_rh <- data_rms %>%
  mutate(RH_lag13 = lag(RH, 13)) %>%
  slice(14:n())

ggplot(d_rh, aes(x = RH_lag13, y = log(egreso_semana))) +
  geom_point(color = "#756bb1") + # Purple color
  labs(x = "Relative humidity (lag 13)", y = "Log of weekly discharges") +
  theme_bw()



# 5. Crossbasis for climatic variables  ----------------------------

#' This block creates crossbasis using cubic splines to model the
#' nonlinear relationship between climatic variables and hospital discharges.
#' Different lags are set for each variable based on their identified optimal lag.


## 5.1 Mean temperature (lag = 14, cubic spline) ----

cb_temp_egre_rms <- crossbasis(
  data_rms$Temp_media,
  lag = 14,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)


## 5.2 Mean precipitation (lag = 11, cubic spline) ----

cb_prec_egre_rms <- crossbasis(
  data_rms$precip_media,
  lag = 11,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.3 Relative humidity (lag = 13, cubic spline) ----

cb_rh_egre_rms <- crossbasis(
  data_rms$RH,
  lag = 13,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.4 AOD index (lag = 11, cubic spline) ----

cb_aod_egre_rms <- crossbasis(
  data_rms$aerosol,
  lag = 11,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)



# 6. DLNM model fitting for hospital discharges ----

#' This block fits DLNM models using Poisson and negative binomial
#' distributions to evaluate the effect of climatic variables on hospital discharges.

#' Remove the first 14 observations to ensure complete lag structure
df_rms_fitted_egresos <- data_rms %>%
  slice(-(1:14))

#' ----------------------------------------------------
## 6.1 DLNM with Poisson distribution and seasonal effects ----
#' ----------------------------------------------------
dlnm_model_poi_estac_rms <- glm(
  egreso_semana ~ cb_temp_egre_rms + cb_prec_egre_rms +
    cb_rh_egre_rms + cb_aod_egre_rms +
    as.factor(epi.week),
  family = poisson(link = "log"),
  data = data_rms
)

#' ----------------------------------------------------
## 6.2 DLNM with Poisson distribution (no seasonal effects) ----
#' ----------------------------------------------------
dlnm_model_poi_rms <- glm(
  egreso_semana ~ cb_temp_egre_rms + cb_prec_egre_rms +
    cb_rh_egre_rms + cb_aod_egre_rms,
  family = poisson(link = "log"),
  data = data_rms
)



#' ----------------------------------------------------
## 6.3 DLNM with negative binomial distribution and seasonal effects ----
#' ----------------------------------------------------
dlnm_model_nb_estac_rms <- glm.nb(
  egreso_semana ~ cb_temp_egre_rms + cb_prec_egre_rms +
    cb_rh_egre_rms + cb_aod_egre_rms +
    as.factor(epi.week),
  data = data_rms
)

#' ----------------------------------------------------
## 6.4 DLNM with negative binomial distribution (no seasonal effects) ----
#' ----------------------------------------------------
dlnm_model_nb_rms <- glm.nb(
  egreso_semana ~ cb_temp_egre_rms + cb_prec_egre_rms +
    cb_rh_egre_rms + cb_aod_egre_rms,
  data = data_rms
)



# 7. GLM model fitting (Poisson and Negative Binomial) -----------------------

#' This block fits two classical GLM models (without lag structure):
#' - Poisson model
#' - Negative binomial model
#' Used as benchmarks to compare with the DLNM models.

## 7.1 Poisson GLM model ----
glm_model_poi_rms <- glm(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + 
    as.factor(epi.week),
  family = poisson(link = "log"),
  data = df_rms_fitted_egresos
)

## 7.2 Negative Binomial GLM model ----
glm_model_nb_rms <- glm.nb(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol +
    as.factor(epi.week),
  data = df_rms_fitted_egresos
)


# 8. Error metric computation for models  --------------------------

#' Generate fitted values for each model
df_rms_fitted_egresos <- df_rms_fitted_egresos %>%
  mutate(
    ajustado_dlnm_model_poi_estac_rms = fitted(dlnm_model_poi_estac_rms),
    ajustado_dlnm_model_poi_rms = fitted(dlnm_model_poi_rms),
    ajustado_dlnm_model_nb_estac_rms = fitted(dlnm_model_nb_estac_rms),
    ajustado_dlnm_model_nb_rms = fitted(dlnm_model_nb_rms),
    ajustado_glm_model_poi_rms = fitted(glm_model_poi_rms),
    ajustado_glm_model_nb_rms = fitted(glm_model_nb_rms)
  )
## 8.1 Function to calculate error and information metrics ----

#' ------------------------------------------------------------------------------
#' calcular_metricas()
#' ------------------------------------------------------------------------------
#' Function to calculate performance metrics for a fitted model and its
#' associated predictions. Includes classical error metrics, information
#' criteria, and a composite score that summarizes overall performance.
#'
#' Parameters:
#' - modelo: the fitted model object (e.g., glm, glm.nb)
#' - predicciones: numeric vector of predicted values
#' - obs: numeric vector of observed values
#' - nombre_modelo: string identifier for the evaluated model
#'
#' Returns:
#' - Data frame with metrics: MAE, MSE, RMSE, MAPE, AIC, BIC, logLik, and Score
#' ------------------------------------------------------------------------------

calcular_metricas <- function(modelo, predicciones, obs, nombre_modelo) {
  #' --------------------------------------------------------------------------
  #' Error metric calculation
  #' --------------------------------------------------------------------------
  MAE <- mean(abs(obs - predicciones))
  MSE <- mean((obs - predicciones)^2)
  RMSE <- sqrt(MSE)
  
  #' MAPE - includes small epsilon to avoid division by zero
  epsilon <- 1e-10
  MAPE_val <- mean(abs((obs - predicciones) / (obs + epsilon))) * 100
  
  #' --------------------------------------------------------------------------
  #' Information criteria and log-likelihood (error-safe)
  #' --------------------------------------------------------------------------
  AIC_val <- tryCatch(AIC(modelo), error = function(e) NA)
  BIC_val <- tryCatch(BIC(modelo), error = function(e) NA)
  ll <- tryCatch(as.numeric(logLik(modelo)), error = function(e) NA)
  
  #' --------------------------------------------------------------------------
  #' Composite score: inverse metrics + adjusted logLik
  #' --------------------------------------------------------------------------
  logLik_term <- if (!is.na(ll) && ll < 0) 1 / abs(ll) else if (!is.na(ll)) ll else 0
  score <- (1 / MAE) + (1 / MSE) + (1 / RMSE) + (1 / MAPE_val) + logLik_term
  
  #' --------------------------------------------------------------------------
  #' Assemble the result as data.frame
  #' --------------------------------------------------------------------------
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

#' Calculate metrics for each model
resultados <- rbind(
  calcular_metricas(
    dlnm_model_poi_estac_rms,
    df_rms_fitted_egresos$ajustado_dlnm_model_poi_estac_rms,
    df_rms_fitted_egresos$egreso_semana,
    "DLNM_Poi_est_rms"
  ),
  calcular_metricas(
    dlnm_model_poi_rms,
    df_rms_fitted_egresos$ajustado_dlnm_model_poi_rms,
    df_rms_fitted_egresos$egreso_semana,
    "DLNM_Poi_rms"
  ),
  calcular_metricas(
    dlnm_model_nb_estac_rms,
    df_rms_fitted_egresos$ajustado_dlnm_model_nb_estac_rms,
    df_rms_fitted_egresos$egreso_semana,
    "DLNM_NB_est_rms"
  ),
  calcular_metricas(
    dlnm_model_nb_rms,
    df_rms_fitted_egresos$ajustado_dlnm_model_nb_rms,
    df_rms_fitted_egresos$egreso_semana,
    "DLNM_NB_rms"
  ),
  calcular_metricas(
    glm_model_poi_rms,
    df_rms_fitted_egresos$ajustado_glm_model_poi_rms,
    df_rms_fitted_egresos$egreso_semana,
    "GLM_Poi_rms"
  ),
  calcular_metricas(
    glm_model_nb_rms,
    df_rms_fitted_egresos$ajustado_glm_model_nb_rms,
    df_rms_fitted_egresos$egreso_semana,
    "GLM_NB_rms"
  )
)

#' Sort results by the best composite score
resultados <- resultados %>%
  arrange(desc(Score))

#' Display results in interactive table format
datatable(resultados, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados),
    color = "black",
    backgroundColor = "lightgray"
  )


## 8.2 Optimal model ----
dlnm_model_rms <- dlnm_model_poi_estac_rms

# Save fitted models to file
save(dlnm_model_nb_estac_rms,
     dlnm_model_nb_rms,
     dlnm_model_rms,
     dlnm_model_poi_estac_rms,
     dlnm_model_poi_rms,
     glm_model_nb_rms,
     glm_model_poi_rms,
     file = "fixed_effects_models/models_for_discharges_rms.RData"
)
# 9. Residual analysis for optimal DLNM model ----

#' Create time series for discharges and fitted values
egresos_rms <- ts(
  df_rms_fitted_egresos$egreso_semana,
  start = c(2000, 15),
  frequency = 52
)

egresos_fitt_dlnm_rms <- ts(
  fitted(dlnm_model_rms),
  start = c(2000, 15),
  frequency = 52
)

#' Pearson and deviance residuals
resid_pearson_rms <- residuals(
  dlnm_model_rms,
  type = "pearson"
)

resid_deviance_rms <- residuals(
  dlnm_model_rms,
  type = "deviance"
)

#' Convert residuals to time series
resid_pearson_st <- ts(
  resid_pearson_rms,
  start = c(2000, 15),
  frequency = 52
)

resid_deviance_st <- ts(
  resid_deviance_rms,
  start = c(2000, 15),
  frequency = 52
)

## 9.1 Dispersion ----------------------------------------------

#' Scatter plot of Pearson residuals
plot(
  fitted(dlnm_model_rms), resid_pearson_rms,
  xlab = "Fitted values",
  ylab = "Pearson residuals"
)
abline(h = 0, col = "red")

#' Scatter plot of deviance residuals
plot(
  fitted(dlnm_model_rms), resid_deviance_rms,
  xlab = "Fitted values",
  ylab = "Deviance residuals"
)
abline(h = 0, col = "red")

## 9.2 Autocorrelation ----

#' ACF and PACF plots for Pearson residuals
ggtsdisplay(resid_pearson_st)

#' ACF and PACF plots for deviance residuals
ggtsdisplay(resid_deviance_st)

#' Ljung-Box test for autocorrelation
ljbox_pearson_dlnm_rms <- Box.test(
  resid_pearson_rms,
  lag = 20,
  type = "Ljung-Box"
)

ljbox_deviance_dlnm_rms <- Box.test(
  resid_deviance_rms,
  lag = 20,
  type = "Ljung-Box"
)

## 9.3 Normality ----

#' QQ plot for Pearson residuals
qqnorm(resid_pearson_rms, main = "QQ plot of Pearson residuals")
qqline(resid_pearson_rms, col = "red")

#' QQ plot for deviance residuals
qqnorm(resid_deviance_rms, main = "QQ plot of deviance residuals")
qqline(resid_deviance_rms, col = "red")

# Perform Shapiro-Wilk test
shapiro_test_pearson <- shapiro.test(resid_pearson_rms)
shapiro_test_deviance <- shapiro.test(resid_deviance_rms)

# Display results
print(shapiro_test_pearson)
print(shapiro_test_deviance)
# 10. Model predictions ----------------------------------------------------

## 10.1 Comparative plot of fitted series ----
#' ----------------------------------------------------------------------------
#' This code generates a comparative plot between the observed values
#' and the fitted values using DLNM Poisson models with and without
#' seasonal factors, as well as a GLM model with Poisson distribution,
#' for hospital discharges in the RMS region.
#' ----------------------------------------------------------------------------

#' Create time series for the plot
df_rms_fitted_egresos <- df_rms_fitted_egresos %>%
  mutate(
    egresos_ts = ts(egreso_semana, start = c(2000, 15), frequency = 52), # Observed discharge series
    ajuste_dlnm = ts(ajustado_dlnm_model_poi_estac_rms, start = c(2000, 15), frequency = 52), # DLNM fit with seasonal factors
    ajuste_glm = ts(ajustado_glm_model_poi_rms, start = c(2000, 15), frequency = 52) # GLM fit
  )

#' Plot with autoplot to compare observed vs fitted values
autoplot(df_rms_fitted_egresos$egresos_ts, series = "Observed") +
  autolayer(df_rms_fitted_egresos$ajuste_dlnm, series = "DLNM Poisson Fit") +
  autolayer(df_rms_fitted_egresos$ajuste_glm, series = "GLM Poisson Fit") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Observed" = "green",
      "DLNM Poisson Fit" = "blue",
      "GLM Poisson Fit" = "red"
    )
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

## 10.2 Lagged effect predictions ----

#' ====================================================
#' This block generates predictions from the fitted DLNM model
#' using the crossbasis functions for temperature, precipitation,
#' relative humidity, and AOD.
#' ====================================================

# Temperature
pred_dlnm_rms_temp <- crosspred(
  cb_temp_egre_rms,
  dlnm_model_rms,
  cen = mean(data_rms$Temp_media, na.rm = TRUE),
  bylag = 0.1
)

# Precipitation
pred_dlnm_rms_prec <- crosspred(
  cb_prec_egre_rms,
  dlnm_model_rms,
  cen = mean(data_rms$precip_media, na.rm = TRUE),
  bylag = 0.1
)

# Relative humidity
pred_dlnm_rms_rh <- crosspred(
  cb_rh_egre_rms,
  dlnm_model_rms,
  cen = mean(data_rms$RH, na.rm = TRUE),
  bylag = 0.1
)

# AOD
pred_dlnm_rms_aod <- crosspred(
  cb_aod_egre_rms,
  dlnm_model_rms,
  cen = mean(data_rms$aerosol, na.rm = TRUE),
  bylag = 0.1
)

### 10.2.1 Temperature predictions ----

# 3D plot (temperature - lag - RR)
plot(pred_dlnm_rms_temp, xlab = "Mean temperature", zlab = "Relative risk (RR)",
     theta = 300, phi = 10, lphi = 100)

# Contour plot (temperature - lag - RR)
plot(pred_dlnm_rms_temp, "contour", xlab = "Mean temperature", ylab = "Lag",
     key.title = title("RR"))

# Lag-response at 21°C (temperature - lag - RR)
plot(pred_dlnm_rms_temp, "slices", var = 21, col = 2, ylab = "Relative risk (RR)")

# Lag-response at 25°C (temperature - lag - RR)
plot(pred_dlnm_rms_temp, "slices", var = 25, col = 2, ylab = "Relative risk (RR)")

### 10.2.2 Precipitation predictions ----

# 3D plot (precipitation - lag - RR)
plot(pred_dlnm_rms_prec, xlab = "Mean precipitation", zlab = "Relative risk (RR)",
     theta = 280, phi = 10, lphi = 100)

# Contour plot (precipitation - lag - RR)
plot(pred_dlnm_rms_prec, "contour", xlab = "Mean precipitation", ylab = "Lag",
     key.title = title("RR"))

# Lag-response at 8 mm (precipitation - lag - RR)
plot(pred_dlnm_rms_prec, "slices", var = 8, col = 2, ylab = "Relative risk (RR)")

# Lag-response at 60 mm (precipitation - lag - RR)
plot(pred_dlnm_rms_prec, "slices", var = 60, col = 2, ylab = "Relative risk (RR)")

### 10.2.3 Relative humidity predictions ----

# 3D plot (RH - lag - RR)
plot(pred_dlnm_rms_rh, xlab = "Mean relative humidity", zlab = "Relative risk (RR)",
     theta = 300, phi = 10, lphi = 100)

# Contour plot (RH - lag - RR)
plot(pred_dlnm_rms_rh, "contour", xlab = "Mean relative humidity", ylab = "Lag",
     key.title = title("RR"))

# Lag-response at 0.25 (RH - lag - RR)
plot(pred_dlnm_rms_rh, "slices", var = 0.25, col = 2, ylab = "Relative risk (RR)")

# Lag-response at 0.43 (RH - lag - RR)
plot(pred_dlnm_rms_rh, "slices", var = 0.43, col = 2, ylab = "Relative risk (RR)")

### 10.2.4 AOD index predictions ----

# 3D plot (AOD - lag - RR)
plot(pred_dlnm_rms_aod, xlab = "AOD index", zlab = "Relative risk (RR)",
     theta = 200, phi = 10, lphi = 100)

# Contour plot  (AOD - lag - RR)
plot(pred_dlnm_rms_aod, "contour", xlab = "AOD index", ylab = "Lag",
     key.title = title("RR"))

# Lag-response at 1e-05  (AOD - lag - RR)
plot(pred_dlnm_rms_aod, "slices", var = 1e-05, col = 2, ylab = "Relative risk (RR)")

# Lag-response at 4e-04 (AOD - lag - RR)
plot(pred_dlnm_rms_aod, "slices", var = 4e-04, col = 2, ylab = "Relative risk (RR)")


# 11. Forecasts ----------------------------------------------------------

## 11.1 General data ----

# Construct data frame with crossbasis columns
df_con_crossbasis_egresos <- cbind(data_rms,
                                   setNames(as.data.frame(cb_temp_egre_rms),
                                            make.names(paste0("t_", colnames(cb_temp_egre_rms)), unique = TRUE)),
                                   setNames(as.data.frame(cb_prec_egre_rms),
                                            make.names(paste0("p_", colnames(cb_prec_egre_rms)), unique = TRUE)),
                                   setNames(as.data.frame(cb_rh_egre_rms),
                                            make.names(paste0("rh_", colnames(cb_rh_egre_rms)), unique = TRUE)),
                                   setNames(as.data.frame(cb_aod_egre_rms),
                                            make.names(paste0("aod_", colnames(cb_aod_egre_rms)), unique = TRUE)))

# Remove initial rows with NA due to lags
df_con_crossbasis_egresos <- df_con_crossbasis_egresos %>%
  slice(-(1:14))

# Function to compute error metrics: MAE, MSE, RMSE, MAPE
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

# Split data into training and test (2017)
df_train <- df_con_crossbasis_egresos %>% filter(year != 2017)
df_2017 <- df_con_crossbasis_egresos %>% filter(year == 2017)

# Get crossbasis column names
columnas_exposicion_egresos <- colnames(df_con_crossbasis_egresos)[12:ncol(df_con_crossbasis_egresos)]

# Build DLNM formula dynamically
formula_dlnm <- as.formula(
  paste("egreso_semana ~", 
        paste(columnas_exposicion_egresos, collapse = " +"),
        "+ as.factor(epi.week)")
)

# Fit DLNM model (excluding 2017)
dlnm_model_nb_estac_pc <- glm.nb(formula = formula_dlnm, data = df_train)

# Fit NB linear model (no lags, excluding 2017)
glm_model_nb_pc <- glm.nb(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  data = df_train
)

# Predict on training set
df_train <- df_train %>%
  mutate(
    pred_dlnm_train = predict(dlnm_model_nb_estac_pc, type = "response"),
    pred_glm_train = predict(glm_model_nb_pc, type = "response")
  )

# Predict on 2017 test set
df_2017 <- df_2017 %>%
  mutate(
    pred_dlnm_2017 = predict(dlnm_model_nb_estac_pc, newdata = df_2017, type = "response"),
    pred_glm_2017 = predict(glm_model_nb_pc, newdata = df_2017, type = "response")
  )

# Compute forecast errors for 2017
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
glm_model_nb_pc <- glm.nb(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  data = df_train
)

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
glm_model_nb_pc <- glm.nb(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  data = df_train
)

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

# Combine and display errors
errores_totales <- bind_rows(errores_2017, errores_2018, errores_2019)

datatable(errores_totales, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(errores_totales),
    color = "black",
    backgroundColor = "lightgray"
  )

## 11.5 Forecast plots ----

# Create complete observed time series
serie_real <- ts(df_con_crossbasis_egresos$egreso_semana, start = c(2000, 13), frequency = 52)

# Create forecast time series, filling only predicted years
serie_pred_2017 <- ts(rep(NA, length(serie_real)), start = c(2000, 13), frequency = 52)
serie_pred_2017[which(df_con_crossbasis_egresos$year == 2017)] <- df_2017$pred_dlnm_2017

serie_pred_2018 <- ts(rep(NA, length(serie_real)), start = c(2000, 13), frequency = 52)
serie_pred_2018[which(df_con_crossbasis_egresos$year == 2018)] <- df_2018$pred_dlnm_2018

serie_pred_2019 <- ts(rep(NA, length(serie_real)), start = c(2000, 13), frequency = 52)
serie_pred_2019[which(df_con_crossbasis_egresos$year == 2019)] <- df_2019$pred_dlnm_2019

# Plot forecast vs actual series
autoplot(serie_real, series = "Observed", color = "green", size = 0.4) +
  autolayer(serie_pred_2017, series = "Forecast 2017", color = "red", size = 0.9, alpha = 0.7) +
  autolayer(serie_pred_2018, series = "Forecast 2018", color = "blue", size = 0.9, alpha = 0.7) +
  autolayer(serie_pred_2019, series = "Forecast 2019", color = "#006600", size = 0.9, alpha = 0.7) +
  labs(
    x = "Epidemiological week",
    y = "Hospital discharges",
    color = "Series"
  ) + theme_bw()
