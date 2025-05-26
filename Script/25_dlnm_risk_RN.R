# 1. Libraries  --------------------------------------------------------------

## 1.1 Data manipulation and transformation ----

library(dplyr) # Data manipulation
library(tidyr) # Data transformation
library(tidyverse) # Collection of packages for data analysis

## 1.2 Visualization and plotting ----

library(ggplot2) # ggplot-style plots
library(ggseas) # Visualization of seasonal components
library(scales) # Customization of plot scales
library(viridis) # Perceptual color palettes
library(corrplot) # Correlation plots
library(GGally) # Enhancements for ggplot2 (plot matrices)

## 1.3 Statistical analysis and modeling ----

library(car) # Analysis and diagnostics of linear models
library(dlnm) # Nonlinear models in time series
library(forecast) # Time series forecasting
library(glmmTMB) # Generalized nonlinear mixed models
library(lme4) # Mixed-effects models
library(mgcv) # Fitting GAM models
library(gamlss) # Fitting GAMLSS models for zero inflation
library(MASS) # GLM model fitting tools
library(splines) # Generation of cubic splines
library(urca) # Unit root tests in time series
library(tseries) # Time series analysis

## 1.4 Information analysis and metrics ----

library(energy) # Distance correlation (dCor)
library(infotheo) # Mutual Information (MI)
library(Metrics) # Model evaluation metrics

## 1.5 Handling spatial data ----

library(sf) # Handling spatial data

## 1.6 Table creation

library(kableExtra) # Table creation
library(DT) # Interactive tables


# 2. Data loading ----------------------------------------------------------

## 2.1 General database ----
load(file = "datos_final1_para_analizar.RData")

## 2.2 Spatial data ----
subregion_st <- st_read("Subegiones Climáticas.shp")


# 3. Data generation by region and subregion ---------------------------


#' This code block generates summarized data by region and subregion,
#' calculating key weekly statistics for each combination of year,
#' epidemiological week, and subregion.
#' The data include summarized values of hospital discharges, relative risk of infection,
#' population, precipitation, temperature, aerosol concentration, and
#' relative humidity.


## 3.1 Data by subregion and epidemiological week ----
data_rn_subreg <- datos_finales_subregion %>%
  filter(reg_climat == "RN", epi.week != 53) %>%
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

#' A column is added to identify each region, and all datasets are merged into a single dataset.

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
# 4. Correlation analysis via permutation tests ----------------------------

#' This block designs a correlation function between lagged predictors and
#' the response variable in region RN using different methods:
#' - Pearson correlation (linear)
#' - Distance correlation (dCor) (non-linear)
#' - Mutual information (MI) (non-linear)
#' Significance is assessed via permutation tests.

#' Define maximum lag to evaluate


## 4.1 Functions to establish crossbasis parameterization ----

#' ------------------------------------------------------------------------------
#' Function: perm_test
#' ------------------------------------------------------------------------------
#' Description:
#' Performs a permutation test to assess statistical dependence between two numeric vectors.
#' Supports three methods: Pearson correlation, distance correlation (dcor), and mutual information (MI).
#'
#' Arguments:
#' - x: numeric vector (independent variable).
#' - y: numeric vector (dependent variable).
#' - method: character string indicating the dependency method to use: "pearson" (default), "dcor", or "MI".
#' - n_perm: integer, number of permutations to perform (default 50).
#'
#' Returns:
#' - A numeric value between 0 and 1 corresponding to the p-value of the permutation test.
#'
#' Details:
#' - For "pearson", computes |cor(x, y)| with random permutations of x.
#' - For "dcor", uses the dcor() function from the energy package.
#' - For "MI", computes mutual information between discretizations using mutinformation() from infotheo.

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
#' Function to identify the best model among combinations of
#' distributed non-linear lag structures (DLNM), evaluating different types of
#' exposure and lag functions with Gamma and Normal regression models.
#' The optimal model is selected based on fit and prediction metrics.
#'
#' Parameters:
#' - data: data.frame containing input data
#' - variable: name of the exposure variable (string)
#' - respuesta: name of the response variable (string)
#' - max_lag: maximum number of lags to evaluate
#' - max_df_var: maximum degrees of freedom for the variable
#' - max_df_lag: maximum degrees of freedom for the lag
#'
#' Returns:
#' - A list with:
#'   - mejor_modelo: data.frame with the optimal model row
#'   - resultado_general: data.frame with all evaluated combinations
#' ------------------------------------------------------------------------------

buscar_mejor_modelo <- function(data, variable, respuesta, max_lag, max_df_var, max_df_lag) {
  
  resultados <- data.frame(
    modelo = character(),
    lag = integer(),
    df_var = character(),
    df_lag = character(),
    aic = numeric(),
    mae = numeric(),
    mse = numeric(),
    rmse = numeric(),
    r2 = numeric(),
    score = numeric()
  )
  
  evaluar_modelo <- function(modelo, data_filtrada, respuesta, lag, df_var, df_lag, tipo) {
    predicciones <- predict(modelo, type = "response")
    obs <- data_filtrada[[respuesta]]
    
    mae <- mean(abs(obs - predicciones))
    mse <- mean((obs - predicciones)^2)
    rmse <- sqrt(mse)
    r2 <- cor(obs, predicciones)^2
    aic_val <- AIC(modelo)
    
    score <- (1 / aic_val) + (1 / mae) + (1 / mse) + (1 / rmse) + r2
    
    data.frame(
      modelo = tipo,
      lag = lag,
      df_var = as.character(df_var),
      df_lag = as.character(df_lag),
      aic = aic_val,
      mae = mae,
      mse = mse,
      rmse = rmse,
      score = score
    )
  }
  
  for (lag in 2:max_lag) {
    cb_temp_ini <- crossbasis(
      data[[variable]],
      lag = lag,
      argvar = list(fun = "lin"),
      arglag = list(fun = "strata")
    )
    
    n_lags <- max(attr(cb_temp_ini, "lag"))
    data_filtrada <- data[(n_lags + 1):nrow(data), ]
    cb_temp_ini_filtrado <- cb_temp_ini[(n_lags + 1):nrow(cb_temp_ini), , drop = FALSE]
    
    modelo_gamma <- glm(
      data_filtrada[[respuesta]] ~ cb_temp_ini_filtrado,
      family = Gamma(link = "log"),
      data = data_filtrada
    )
    
    modelo_norm <- glm(
      data_filtrada[[respuesta]] ~ cb_temp_ini_filtrado,
      family = gaussian(link = "identity"),
      data = data_filtrada
    )
    
    resultados <- rbind(
      resultados,
      evaluar_modelo(modelo_gamma, data_filtrada, respuesta, lag, "lin", "strata", "Gamma"),
      evaluar_modelo(modelo_norm, data_filtrada, respuesta, lag, "lin", "strata", "Normal")
    )
  }
  
  for (lag in 2:max_lag) {
    for (df_var in 2:max_df_var) {
      for (df_lag in 2:max_df_lag) {
        cb_temp <- crossbasis(
          data[[variable]],
          lag = lag,
          argvar = list(fun = "ns", df = df_var),
          arglag = list(fun = "ns", df = df_lag)
        )
        
        n_lags <- max(attr(cb_temp, "lag"))
        data_filtrada <- data[(n_lags + 1):nrow(data), ]
        cb_temp_filtrado <- cb_temp[(n_lags + 1):nrow(cb_temp), , drop = FALSE]
        
        modelo_gamma <- glm(
          data_filtrada[[respuesta]] ~ cb_temp_filtrado,
          family = Gamma(link = "log"),
          data = data_filtrada
        )
        
        modelo_norm <- glm(
          data_filtrada[[respuesta]] ~ cb_temp_filtrado,
          family = gaussian(link = "identity"),
          data = data_filtrada
        )
        
        resultados <- rbind(
          resultados,
          evaluar_modelo(modelo_gamma, data_filtrada, respuesta, lag, df_var, df_lag, "Gamma"),
          evaluar_modelo(modelo_norm, data_filtrada, respuesta, lag, df_var, df_lag, "Normal")
        )
      }
    }
  }
  
  mejor_modelo <- resultados[resultados$score == max(resultados$score), ]
  
  cat("\nBest model based on score:\n")
  print(mejor_modelo)
  
  list(
    mejor_modelo = mejor_modelo,
    resultado_general = resultados
  )
}
## 4.2 Correlation analysis for mean temperature ----

results_temp <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)


#' Compute correlations and p-values for each lag

max_lag <- 14
for (lag in 1:max_lag) {
  X_lagged <- head(data_rn$Temp_media, length(data_rn$Temp_media) - lag)
  Y_lagged <- tail(data_rn$Riesgo_relativ_contagio,
                   length(data_rn$Riesgo_relativ_contagio) - lag)
  
  # Pearson correlation and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  # dCor and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  # MI and p-value
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  # Store results
  results_temp <- rbind(
    results_temp,
    data.frame(
      Lag = lag, Pearson = pearson_corr, Pearson_p = pearson_p,
      dCor = dcor_val, dCor_p = dcor_p,
      MI = MI_val, MI_p = MI_p
    )
  )
}


#' View results

results_temp

#' Compute max and min values for each series
max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)

max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)

max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)


#' Correlation plot for each method

ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature Lag (k)", y = "Coefficient") +
  scale_color_manual(
    values = c(
      "Pearson (linear)" = "blue",
      "dCor (non-linear)" = "red",
      "MI (non-linear)" = "green"
    )
  ) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_temp$Pearson), y = max_pearson,
           label = paste("Max: ", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$Pearson), y = min_pearson,
           label = paste("Min: ", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$dCor), y = max_dcor,
           label = paste("Max: ", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$dCor), y = min_dcor,
           label = paste("Min: ", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$MI), y = max_mi,
           label = paste("Max: ", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$MI), y = min_mi,
           label = paste("Min: ", round(min_mi, 2)), color = "green", vjust = 1.5
  )



#' Run best model search up to lag 9

res <- buscar_mejor_modelo(
  data = data_rn,
  variable = "Temp_media",
  respuesta = "Riesgo_relativ_contagio",
  max_lag = 7,
  max_df_var = 3,
  max_df_lag = 3
)


#' View results using DT::datatable

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultados_completos),
    color = "black", # Black text color
    backgroundColor = "lightgray" # Light gray background for better contrast
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
  # Shift the series to evaluate the lagged effect
  X_lagged <- head(data_rn$precip_media,
                   length(data_rn$precip_media) - lag)
  Y_lagged <- tail(data_rn$Riesgo_relativ_contagio,
                   length(data_rn$Riesgo_relativ_contagio) - lag)
  
  # Pearson correlation and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  # Distance correlation (dCor) and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  # Mutual Information (MI) and p-value
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
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Precipitation Lag (k)", y = "Coefficient") +
  
  #' Custom colors for each series
  scale_color_manual(
    values = c(
      "Pearson (linear)" = "blue",
      "dCor (non-linear)" = "red",
      "MI (non-linear)" = "green"
    )
  ) +
  
  #' Adjust x-axis to display integer values
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  
  #' Apply minimalist theme
  theme_minimal() +
  theme(legend.title = element_blank()) +
  
  #' Annotations for max and min values
  annotate(
    "text",
    x = which.max(results_Prec$Pearson), y = max_pearson,
    label = paste("Max: ", round(max_pearson, 2)),
    color = "blue", vjust = -0.5
  ) +
  annotate(
    "text",
    x = which.min(results_Prec$Pearson), y = min_pearson,
    label = paste("Min: ", round(min_pearson, 2)),
    color = "blue", vjust = 1.5
  ) +
  annotate(
    "text",
    x = which.max(results_Prec$dCor), y = max_dcor,
    label = paste("Max: ", round(max_dcor, 2)),
    color = "red", vjust = -0.5
  ) +
  annotate(
    "text",
    x = which.min(results_Prec$dCor), y = min_dcor,
    label = paste("Min: ", round(min_dcor, 2)),
    color = "red", vjust = 1.5
  ) +
  annotate(
    "text",
    x = which.max(results_Prec$MI), y = max_mi,
    label = paste("Max: ", round(max_mi, 2)),
    color = "green", vjust = -0.5
  ) +
  annotate(
    "text",
    x = which.min(results_Prec$MI), y = min_mi,
    label = paste("Min: ", round(min_mi, 2)),
    color = "green", vjust = 1.5
  )


#' Best model search for precipitation and discharges,
#' with a maximum of 9 lags

res <- buscar_mejor_modelo(
  data_rn,
  "precip_media",
  "Riesgo_relativ_contagio",
  max_lag = 10,
  max_df_var = 3,
  max_df_lag = 3
)


#' Display results using an interactive table

datatable(
  res$resultado_general,
  options = list(
    pageLength = 5, # Number of visible rows per page
    scrollX = TRUE  # Enable horizontal scrolling
  )
) %>%
  formatStyle(
    columns = names(res$resultado_general), # Apply to all columns
    color = "black", # Black text color
    backgroundColor = "lightgray" # Light gray background
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


#' Compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  #' Create lagged series
  X_lagged <- head(data_rn$RH, length(data_rn$RH) - lag)
  Y_lagged <- tail(data_rn$Riesgo_relativ_contagio,
                   length(data_rn$Riesgo_relativ_contagio) - lag)
  
  #' Pearson correlation and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  #' Distance correlation and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  #' Mutual Information and p-value
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


#' Plot obtained correlations

ggplot(results_RH, aes(x = Lag)) +
  #' Pearson
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  
  #' dCor
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  
  #' MI
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  
  #' Horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  
  #' Customize labels and scales
  labs(
    x = "Relative Humidity Lag (k)",
    y = "Coefficient"
  ) +
  scale_color_manual(
    values = c(
      "Pearson (linear)" = "blue",
      "dCor (non-linear)" = "red",
      "MI (non-linear)" = "green"
    )
  ) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  
  #' Theme and legend
  theme_minimal() +
  theme(legend.title = element_blank()) +
  
  #' Annotations for max and min values
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


#' Model evaluation for relative humidity (RH) with a maximum of
#' 9 lags, based on previous data
res <- buscar_mejor_modelo(
  data_rn,
  "RH",
  "Riesgo_relativ_contagio",
  max_lag = 13,
  max_df_var = 3,
  max_df_lag = 3
)


#' Display results in an interactive table

datatable(
  res$resultado_general,
  options = list(
    pageLength = 5, # Number of rows to display per page
    scrollX = TRUE  # Enable horizontal scrolling if many columns
  )
) %>%
  formatStyle(
    columns = names(res$resultado_general), #' Apply formatting to all columns
    color = "black", # Black text color
    backgroundColor = "lightgray" # Light gray background
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
  X_lagged <- head(data_rn$aerosol, length(data_rn$aerosol) - lag)
  Y_lagged <- tail(data_rn$Riesgo_relativ_contagio,
                   length(data_rn$Riesgo_relativ_contagio) - lag)
  
  #' Pearson correlation and p-value
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  #' Distance correlation and p-value
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  #' Mutual Information and p-value
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

#' Display results in console
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
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    x = "AOD Index Lag (k)",
    y = "Coefficient"
  ) +
  scale_color_manual(
    values = c(
      "Pearson (linear)" = "blue",
      "dCor (non-linear)" = "red",
      "MI (non-linear)" = "green"
    )
  ) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  
  #' Add annotations for max and min values of each series
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

#' Model evaluation for AOD index (Aerosol)
res <- buscar_mejor_modelo(
  data_rn,           # Dataset
  "aerosol",         # Predictor variable
  "Riesgo_relativ_contagio", # Response variable
  max_lag = 13,      # Maximum lag to consider
  max_df_var = 3,    # Maximum complexity for variable splines
  max_df_lag = 3     # Maximum complexity for lag splines
)


#' Display results in an interactive table
datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general), # Apply to all columns
    color = "black",
    backgroundColor = "lightgray" # Light gray background
  )

#' ------------------------------------------------------------------------------
#' Function: continuous_modeling_parameters()
#' ------------------------------------------------------------------------------
#' Evaluates multiple combinations of non-linear basis functions for climatic
#' variables (temperature, precipitation, relative humidity, and AOD) using
#' normal models with distributed lags.
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
#' - A list with a data.frame called 'resultados_completos' containing
#'   the fitting metrics (AIC, BIC, MAE, MSE, RMSE, R2, MAPE, logLik, score)
#'   for each evaluated parameter combination.
#'
#' Details:
#' - The fitted model is of type glm.
#' - Includes seasonal effects via as.factor(epi.week).
#' - Uses crossbasis terms generated with the crossbasis function (dlnm package).
#' ------------------------------------------------------------------------------

continuous_modeling_parameters <- function(
    data, respuesta,
    temp_var, prec_var, rh_var, aod_var,
    max_lag_temp, max_df_var_temp, max_df_lag_temp,
    max_lag_prec, max_df_var_prec, max_df_lag_prec,
    max_lag_rh, max_df_var_rh, max_df_lag_rh,
    max_lag_AOD, max_df_var_AOD, max_df_lag_AOD
) {
  
  # Definition of variables and their parameters
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
  
  # Function to evaluate the model and calculate metrics
  # R2 is excluded from the score.
  evaluar_modelo <- function(modelo, data_filtrada, respuesta, tipo, params) {
    predicciones <- predict(modelo, type = "response")
    obs <- data_filtrada[[respuesta]]
    
    MAE    <- mean(abs(obs - predicciones))
    MSE    <- mean((obs - predicciones)^2)
    RMSE   <- sqrt(MSE)
    R2     <- cor(obs, predicciones)^2
    AIC_val <- AIC(modelo)
    BIC_val <- BIC(modelo)
    MAPE_val <- mean(abs((obs - predicciones) / obs)) * 100  # Mean absolute percentage error
    ll <- as.numeric(logLik(modelo))
    
    # Condition for logLik term in score
    logLik_term <- if (ll < 0) 1 / abs(ll) else ll
    
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
  
  # Function to generate crossbasis using the specified maximum lag
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
  
  # Generate crossbases for each variable
  crossbases <- lapply(variables, generar_crossbasis, data = data)
  
  # Obtain maximum lag across all crossbases to filter the dataset
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
  
  # Nested loops to combine the specifications of each variable
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
          
          # MODEL (a different continuous function could be used for validation;
          # behavior is the same regarding parameters)
          modelo_ajustado <- glm(data_filtrada[[respuesta]] ~ 
                                   cb_temp$crossbasis + cb_prec$crossbasis + 
                                   cb_rh$crossbasis + cb_aod$crossbasis +
                                   as.factor(epi.week), family = gaussian(link = "identity"),
                                 data = data_filtrada)
          
          resultados <- rbind(resultados, 
                              evaluar_modelo(modelo_ajustado, data_filtrada, respuesta, "modelo_optim", params))
        }
      }
    }
  }
  
  return(list(resultados_completos = resultados))
}

## 4.6 Scatter plots for variables with optimal lags ----

#' Scatter plot for Temp_media (lag 7)
d_temp <- data_rn %>%
  mutate(Temp_media_lag7 = lag(Temp_media, 7)) %>%
  slice(8:n()) # Remove NA values due to lag

ggplot(d_temp, aes(x = Temp_media_lag7, y = Riesgo_relativ_contagio)) +
  geom_point(color = "#2c7fb8") + # Blue color
  labs(x = "Mean temperature (lag 7)", y = "Relative risk of hospital discharges") +
  theme_bw()


#' Scatter plot for precip_media (lag 10)

d_precip <- data_rn %>%
  mutate(precip_media_lag10 = lag(precip_media, 10)) %>%
  slice(11:n())

ggplot(d_precip, aes(x = precip_media_lag10, y = Riesgo_relativ_contagio)) +
  geom_point(color = "#f03b20") + # Red color
  labs(x = "Mean precipitation (lag 10)", y = "Relative risk of hospital discharges") +
  theme_bw()


#' Scatter plot for aerosol (lag 13)
d_aerosol <- data_rn %>%
  mutate(aerosol_lag13 = lag(aerosol, 13)) %>%
  slice(14:n())

ggplot(d_aerosol, aes(x = aerosol_lag13, y = Riesgo_relativ_contagio)) +
  geom_point(color = "#31a354") + # Green color
  labs(x = "AOD index (lag 13)", y = "Relative risk of hospital discharges") +
  theme_bw()


#' Scatter plot for RH (lag 11)
d_rh <- data_rn %>%
  mutate(RH_lag11 = lag(RH, 11)) %>%
  slice(12:n())

ggplot(d_rh, aes(x = RH_lag11, y = Riesgo_relativ_contagio)) +
  geom_point(color = "#756bb1") + # Purple color
  labs(x = "Relative humidity (lag 11)", y = "Relative risk of hospital discharges") +
  theme_bw()




# 5. Cross-bases for climatic variables  ----------------------------

#' This code block creates crossbasis objects using cubic splines to model
#' the non-linear relationship between climatic variables and hospital
#' discharge risks.
#' Different lags are set for each variable depending on the nature of
#' the data.


## 5.1 Mean temperature (lag = 7, cubic spline) ----

cb_temp_egre_rn <- crossbasis(
  data_rn$Temp_media,
  lag = 7,
  argvar = list(fun = "ns", df = 3), # cubic spline for the variable
  arglag = list(fun = "ns", df = 3)  # cubic spline for lags
)


## 5.2 Mean precipitation (lag = 10, cubic spline) ----

cb_prec_egre_rn <- crossbasis(
  data_rn$precip_media,
  lag = 10,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.3 Relative humidity (lag = 11, cubic spline) ----

cb_rh_egre_rn <- crossbasis(
  data_rn$RH,
  lag = 11,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

## 5.4 Aerosol index AOD (lag = 13, cubic spline) ----

cb_aod_egre_rn <- crossbasis(
  data_rn$aerosol,
  lag = 13,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)
# 6. DLNM model fitting for hospital discharge risk ----

#' This code block fits DLNM models using Gamma and Normal distributions
#' to evaluate the effect of climatic variables on hospital discharge risks.

#' Remove the first 13 observations to ensure lag completeness
df_rn_fitted_riesgo <- data_rn %>%
  slice(-(1:13))

#' ----------------------------------------------------
## 6.1 DLNM model with Gamma distribution including weekly factors ----
#' ----------------------------------------------------
dlnm_model_gamma_estac_rn <- glm(
  Riesgo_relativ_contagio ~ cb_temp_egre_rn + cb_prec_egre_rn +
    cb_rh_egre_rn + cb_aod_egre_rn +
    as.factor(epi.week),
  family = Gamma(link = "log"),
  data = data_rn)


#' ----------------------------------------------------
## 6.2 DLNM model with Gamma distribution ----
#' ----------------------------------------------------
dlnm_model_gamma_rn <- glm(
  Riesgo_relativ_contagio ~ cb_temp_egre_rn + cb_prec_egre_rn +
    cb_rh_egre_rn + cb_aod_egre_rn,
  family = Gamma(link = "log"),
  data = data_rn
)


#' ----------------------------------------------------
## 6.3 DLNM model with Normal distribution including weekly factors ----
#' ----------------------------------------------------
dlnm_model_normal_estac_rn <- glm(
  Riesgo_relativ_contagio ~ cb_temp_egre_rn + cb_prec_egre_rn +
    cb_rh_egre_rn + cb_aod_egre_rn +
    as.factor(epi.week),
  family = gaussian(link = "identity"),
  data = data_rn
)

#' ----------------------------------------------------
## 6.4 DLNM model with Normal distribution ----
#' ----------------------------------------------------
dlnm_model_normal_rn <- glm(
  Riesgo_relativ_contagio ~ cb_temp_egre_rn + cb_prec_egre_rn +
    cb_rh_egre_rn + cb_aod_egre_rn,
  family = gaussian(link = "identity"),
  data = data_rn
)


# 7. GLM model fitting (Gamma and Normal) -----------------------

#' This block fits two classic GLM models (without lag structure):
#' - Gamma model
#' - Normal model
#' Used as comparative references against the DLNM models.

## 7.1 GLM model with Gamma distribution -----
glm_model_gamma_rn <- glm(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol +
    as.factor(epi.week),
  family = Gamma(link = "log"),
  data = df_rn_fitted_riesgo
)

## 7.2 GLM model with Normal distribution ----
glm_model_normal_rn <- glm(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol +
    as.factor(epi.week),
  family = gaussian(link = "identity"),
  data = df_rn_fitted_riesgo
)

# 8. Error metric calculation for models  --------------------------

#' Generate fitted predictions for each model
df_rn_fitted_riesgo <- df_rn_fitted_riesgo %>%
  mutate(
    ajustado_dlnm_model_gamma_rn = fitted(dlnm_model_gamma_rn),
    ajustado_dlnm_model_gamma_estac_rn = fitted(dlnm_model_gamma_estac_rn),
    ajustado_dlnm_model_normal_rn = fitted(dlnm_model_normal_rn),
    ajustado_dlnm_model_normal_estac_rn = fitted(dlnm_model_normal_estac_rn),
    ajustado_glm_model_gamma_rn = fitted(glm_model_gamma_rn),
    ajustado_glm_model_normal_rn = fitted(glm_model_normal_rn)
  )
## 8.1 Function to calculate error and information metrics ----

#' ------------------------------------------------------------------------------
#' calcular_metricas()
#' ------------------------------------------------------------------------------
#' Function to compute performance metrics for a fitted model and its
#' associated predictions. Includes classical error metrics, information criteria,
#' and a composite score summarizing overall performance.
#'
#' Parameters:
#' - modelo: fitted model object.
#' - predicciones: numeric vector with model predictions
#' - obs: numeric vector with observed values
#' - nombre_modelo: string identifier for the evaluated model
#'
#' Returns:
#' - Data frame with MAE, MSE, RMSE, MAPE, AIC, BIC, logLik, and Score metrics
#' ------------------------------------------------------------------------------

calcular_metricas <- function(modelo, predicciones, obs, nombre_modelo) {
  #' --------------------------------------------------------------------------
  #' Error metrics calculation
  #' --------------------------------------------------------------------------
  MAE <- mean(abs(obs - predicciones))
  MSE <- mean((obs - predicciones)^2)
  RMSE <- sqrt(MSE)
  
  #' MAPE - includes correction to avoid division by zero
  epsilon <- 1e-10
  MAPE_val <- mean(abs((obs - predicciones) / (obs + epsilon))) * 100
  
  #' --------------------------------------------------------------------------
  #' Information criteria and log-likelihood with error control
  #' --------------------------------------------------------------------------
  AIC_val <- tryCatch(AIC(modelo), error = function(e) NA)
  BIC_val <- tryCatch(BIC(modelo), error = function(e) NA)
  ll <- tryCatch(as.numeric(logLik(modelo)), error = function(e) NA)
  
  #' --------------------------------------------------------------------------
  #' Composite score calculation: inverses + adjusted logLik
  #' --------------------------------------------------------------------------
  logLik_term <- if (!is.na(ll) && ll < 0) 1 / abs(ll) else if (!is.na(ll)) ll else 0
  score <- (1 / MAE) + (1 / MSE) + (1 / RMSE) + (1 / MAPE_val) + logLik_term
  
  #' --------------------------------------------------------------------------
  #' Assemble results as data.frame
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


#' Compute metrics for each model
resultados <- rbind(
  calcular_metricas(
    dlnm_model_gamma_rn,
    df_rn_fitted_riesgo$ajustado_dlnm_model_gamma_rn,
    df_rn_fitted_riesgo$Riesgo_relativ_contagio,
    "DLNM_Gamma_RN"
  ),
  calcular_metricas(
    dlnm_model_gamma_estac_rn,
    df_rn_fitted_riesgo$ajustado_dlnm_model_gamma_estac_rn,
    df_rn_fitted_riesgo$Riesgo_relativ_contagio,
    "DLNM_Gamma_est_RN"
  ),
  calcular_metricas(
    dlnm_model_normal_rn,
    df_rn_fitted_riesgo$ajustado_dlnm_model_normal_rn,
    df_rn_fitted_riesgo$Riesgo_relativ_contagio,
    "DLNM_Normal_RN"
  ),
  calcular_metricas(
    dlnm_model_normal_estac_rn,
    df_rn_fitted_riesgo$ajustado_dlnm_model_normal_estac_rn,
    df_rn_fitted_riesgo$Riesgo_relativ_contagio,
    "DLNM_Normal_est_RN"
  ),
  calcular_metricas(
    glm_model_gamma_rn,
    df_rn_fitted_riesgo$ajustado_glm_model_gamma_rn,
    df_rn_fitted_riesgo$Riesgo_relativ_contagio,
    "GLM_Gamma_RN"
  ),
  calcular_metricas(
    glm_model_normal_rn,
    df_rn_fitted_riesgo$ajustado_glm_model_normal_rn,
    df_rn_fitted_riesgo$Riesgo_relativ_contagio,
    "GLM_Normal_RN"
  )
)

#' Sort results by best score
resultados <- resultados %>%
  arrange(desc(Score))

#' Display results using an interactive table
datatable(resultados, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados),
    color = "black",
    backgroundColor = "lightgray"
  )
## 8.2 Optimal model ----
dlnm_model_rn <- dlnm_model_gamma_estac_rn

# Save the models inside the folder
save(dlnm_model_gamma_estac_rn,
     dlnm_model_gamma_rn,
     dlnm_model_normal_estac_rn,
     dlnm_model_normal_rn,
     dlnm_model_rn,
     glm_model_gamma_rn,
     glm_model_normal_rn,
     file = "fixed_effects_models/models_for_risk_rn.RData"
)

# 9. Residual analysis for the optimal DLNM model ----

#' Create time series of discharges and fitted values
riesgos_rn <- ts(
  df_rn_fitted_riesgo$Riesgo_relativ_contagio,
  start = c(2000, 14),
  frequency = 52
)

riesgos_fitt_dlnm_rn <- ts(
  fitted(dlnm_model_rn),
  start = c(2000, 14),
  frequency = 52
)

#' Pearson and deviance residuals
resid_pearson_rn <- residuals(
  dlnm_model_rn,
  type = "pearson"
)

resid_deviance_rn <- residuals(
  dlnm_model_rn,
  type = "deviance"
)

#' Convert residuals to time series
resid_pearson_st <- ts(
  resid_pearson_rn,
  start = c(2000, 14),
  frequency = 52
)

resid_deviance_st <- ts(
  resid_deviance_rn,
  start = c(2000, 14),
  frequency = 52
)


## 9.1 Scatter plots ----------------------------------------------

#' Scatter plot of Pearson residuals
plot(
  fitted(dlnm_model_rn), resid_pearson_rn,
  xlab = "Fitted values",
  ylab = "Pearson residuals"
)
abline(h = 0, col = "red")

#' Scatter plot of deviance residuals
plot(
  fitted(dlnm_model_rn), resid_deviance_rn,
  xlab = "Fitted values",
  ylab = "Deviance residuals"
)
abline(h = 0, col = "red")

## 9.2 Autocorrelation ----

#' ACF and PACF plots of Pearson residuals
ggtsdisplay(resid_pearson_st)

#' ACF and PACF plots of deviance residuals
ggtsdisplay(resid_deviance_st)

#' Ljung-Box test for autocorrelation
ljbox_pearson_dlnm_rn <- Box.test(
  resid_pearson_rn,
  lag = 20,
  type = "Ljung-Box"
)

ljbox_deviance_dlnm_rn <- Box.test(
  resid_deviance_rn,
  lag = 20,
  type = "Ljung-Box"
)


## 9.3 Normality ----

#' QQ plot for Pearson residuals
qqnorm(resid_pearson_rn, main = "QQ plot of Pearson residuals")
qqline(resid_pearson_rn, col = "red")

#' QQ plot for deviance residuals
qqnorm(resid_deviance_rn, main = "QQ plot of deviance residuals")
qqline(resid_deviance_rn, col = "red")

# Perform Shapiro-Wilk test
shapiro_test_pearson <- shapiro.test(resid_pearson_rn)
shapiro_test_deviance <- shapiro.test(resid_deviance_rn)

# Display the results
print(shapiro_test_pearson)
print(shapiro_test_deviance)
# 10. Model predictions ---------------------------------------------

## 10.1 Comparative plot of fitted series ----
#' ----------------------------------------------------------------------------
#' This code generates a comparison plot between observed values and fitted
#' values using DLNM models with seasonal factors, as well as a GLM model,
#' for hospital discharge risks in the RN region.
#' ----------------------------------------------------------------------------

#' Create time series for the plot
df_rn_fitted_riesgo <- df_rn_fitted_riesgo %>%
  mutate(
    riesgo_ts = ts(riesgos_rn, start = c(2000, 14), frequency = 52), # Observed risk series
    ajuste_DLNM = ts(ajustado_dlnm_model_gamma_estac_rn, start = c(2000, 14), frequency = 52), # DLNM fit with seasonal factors
    ajuste_glm = ts(ajustado_glm_model_gamma_rn, start = c(2000, 14), frequency = 52) # GLM fit
  )

#' Plot using autoplot to compare observed vs fitted values
autoplot(df_rn_fitted_riesgo$riesgo_ts, series = "Observed") +
  
  #' Line for DLNM fit with seasonal factors
  autolayer(df_rn_fitted_riesgo$ajuste_DLNM,
            series = "DLNM Fit"
  ) +
  
  #' Line for GLM fit
  autolayer(df_rn_fitted_riesgo$ajuste_glm,
            series = "GLM Fit"
  ) +
  
  #' Labels and theme
  labs(
    x = "Time (weeks)",
    y = "Hospital discharge risk"
  ) +
  theme_bw() + # White background
  
  #' Custom colors using consistent palette
  scale_color_manual(
    values = c(
      "Observed" = "green", # Green
      "DLNM Fit" = "blue", # Blue
      "GLM Fit" = "red" # Red
    )
  ) +
  
  #' Legend adjustments
  theme(
    legend.title = element_blank(), # Remove legend title
    legend.position = "bottom" # Place legend at the bottom
  )



## 10.2 Lagged effects from the model ----
#' ====================================================
#' This code block generates predictions from the fitted DLNM model
#' using crossbasis objects for temperature, precipitation,
#' relative humidity, and AOD.

#' ----------------------------------------------------
#' Predictions for mean temperature (crosspred)
#' ----------------------------------------------------
pred_dlnm_rn_temp <- crosspred(
  cb_temp_egre_rn,
  dlnm_model_rn,
  cen = median(data_rn$Temp_media, na.rm = TRUE), # reference point (center)
  bylag = 0.1 # lag increments
)

#' ----------------------------------------------------
#' Predictions for mean precipitation (crosspred)
#' ----------------------------------------------------
pred_dlnm_rn_prec <- crosspred(
  cb_prec_egre_rn,
  dlnm_model_rn,
  cen = median(data_rn$precip_media, na.rm = TRUE), # reference point (center)
  bylag = 0.1 # lag increments
)

#' ----------------------------------------------------
#' Predictions for relative humidity (crosspred)
#' ----------------------------------------------------
pred_dlnm_rn_rh <- crosspred(
  cb_rh_egre_rn,
  dlnm_model_rn,
  cen = median(data_rn$RH, na.rm = TRUE), # reference point (center)
  bylag = 0.1 # lag increments
)

#' ----------------------------------------------------
#' Predictions for aerosol index (crosspred)
#' ----------------------------------------------------
pred_dlnm_rn_aod <- crosspred(
  cb_aod_egre_rn,
  dlnm_model_rn,
  cen = median(data_rn$aerosol, na.rm = TRUE), # reference point (center)
  bylag = 0.1 # lag increments
)
### 10.2.1 Predictions for mean temperature ----

#' ----------------------------------------------------
#' 3D plot (temperature - lag - relative risk)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_temp,
  xlab = "Mean temperature",
  zlab = "Relative risk (RR)",
  theta = 300, phi = 10, lphi = 100
)

#' ----------------------------------------------------
#' Contour plot ( temperature - lag - RR)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_temp, "contour",
  xlab = "Mean temperature",
  ylab = "Lag",
  key.title = title("RR")
)

#' ----------------------------------------------------
#' Lag-response plot for a specific temperature value (31.1°C)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_temp, "slices",
  var = 31.1,
  col = 2,
  ylab = "Relative risk (RR)"
)

#' ----------------------------------------------------
#' Lag-response plot for a specific temperature value (24.2°C)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_temp, "slices",
  var = min(pred_dlnm_rn_temp$predvar),
  col = 2,
  ylab = "Relative risk (RR)"
)

### 10.2.2 Predictions for mean precipitation ----

#' ----------------------------------------------------
#' 3D plot ( precipitation - lag - relative risk)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_prec,
  xlab = "Mean precipitation",
  zlab = "Relative risk (RR)",
  theta = 300, phi = 10, lphi = 100
)

#' ----------------------------------------------------
#' Contour plot ( precipitation - lag - RR)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_prec, "contour",
  xlab = "Mean precipitation",
  ylab = "Lag",
  key.title = title("RR")
)

#' ----------------------------------------------------
#' Lag-response plot for a specific precipitation value (24 mm)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_prec, "slices",
  var = 24,
  col = 2,
  ylab = "Relative risk (RR)"
)

#' ----------------------------------------------------
#' Lag-response plot for a specific precipitation value (100 mm)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_prec, "slices",
  var = 100,
  col = 2,
  ylab = "Relative risk (RR)"
)

### 10.2.3 Predictions for mean relative humidity ----

#' ----------------------------------------------------
#' 3D plot ( humidity - lag - relative risk)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_rh,
  xlab = "Mean relative humidity",
  zlab = "Relative risk (RR)",
  theta = 300, phi = 10, lphi = 100
)

#' ----------------------------------------------------
#' Contour plot ( humidity - lag - RR)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_rh, "contour",
  xlab = "Mean relative humidity",
  ylab = "Lag",
  key.title = title("RR")
)

#' ----------------------------------------------------
#' Lag-response plot for a specific humidity value (0.25)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_rh, "slices",
  var = 0.25,
  col = 2,
  ylab = "Relative risk (RR)"
)

#' ----------------------------------------------------
#' Lag-response plot for a specific humidity value (0.43)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_rh, "slices",
  var = 0.45,
  col = 2,
  ylab = "Relative risk (RR)"
)

### 10.2.4 Predictions for AOD index ----

#' ----------------------------------------------------
#' 3D plot ( AOD - lag - relative risk)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_aod,
  xlab = "AOD index",
  zlab = "Relative risk (RR)",
  theta = 220, phi = 10, lphi = 100
)

#' ----------------------------------------------------
#' Contour plot ( AOD - lag - RR)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_aod, "contour",
  xlab = "AOD index",
  ylab = "Lag",
  key.title = title("RR")
)

#' ----------------------------------------------------
#' Lag-response plot for a specific AOD value (0)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_aod, "slices",
  var = 0,
  col = 2,
  ylab = "Relative risk (RR)"
)

#' ----------------------------------------------------
#' Lag-response plot for a specific AOD value (4e-04)
#' ----------------------------------------------------
plot(
  pred_dlnm_rn_aod, "slices",
  var = 0.00040,
  col = 2,
  ylab = "Relative risk (RR)"
)
# 11. Forecasting ----

## 11.1 General data ----

# Construct dataframe with crossbasis components

df_con_crossbasis_riesgos <- cbind(data_rn,
                                   setNames(
                                     as.data.frame(cb_temp_egre_rn),
                                     make.names(paste0("t_", colnames(cb_temp_egre_rn)), unique = TRUE)),
                                   setNames(
                                     as.data.frame(cb_prec_egre_rn),
                                     make.names(paste0("p_", colnames(cb_prec_egre_rn)), unique = TRUE)),
                                   setNames(
                                     as.data.frame(cb_rh_egre_rn),
                                     make.names(paste0("rh_", colnames(cb_rh_egre_rn)), unique = TRUE)),
                                   setNames(
                                     as.data.frame(cb_aod_egre_rn),
                                     make.names(paste0("aod_", colnames(cb_aod_egre_rn)), unique = TRUE)))

# Remove initial NAs from the dataset
df_con_crossbasis_riesgos <- df_con_crossbasis_riesgos %>%
  slice(-(1:13))


# Error metrics function
# Computes MAE, MSE, RMSE, MAPE
calcular_errores <- function(obs, pred, modelo) {
  mae_val <- mae(obs, pred)
  mse_val <- mse(obs, pred)
  rmse_val <- rmse(obs, pred)
  mape_val <- mape(obs, pred) * 100
  sampe_val = mean(2 * abs(pred - obs) / (abs(obs) + abs(pred))) * 100
  
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

# Train-test split (2017 as test)
df_train <- df_con_crossbasis_riesgos %>% filter(year != 2017)
df_2017 <- df_con_crossbasis_riesgos %>% filter(year == 2017)

# Extract crossbasis column names
columnas_exposicion_riesgos <- colnames(df_con_crossbasis_riesgos)[12:ncol(df_con_crossbasis_riesgos)]

# Dynamic formula for DLNM model
formula_dlnm <- as.formula(
  paste("Riesgo_relativ_contagio ~", 
        paste(columnas_exposicion_riesgos, collapse = " +"),
        "+ as.factor(epi.week)")
)

# DLNM model excluding 2017
dlnm_model_gamma_estac_pc <- glm(
  formula = formula_dlnm,
  family = Gamma(link = "log"),
  data = df_train
)

# GLM model without lags excluding 2017
glm_model_gamma_pc <- glm(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  family = Gamma(link = "log"),
  data = df_train
)

# Predictions on training data
df_train <- df_train %>%
  mutate(
    pred_dlnm_train = predict(dlnm_model_gamma_estac_pc, type = "response"),
    pred_glm_train = predict(glm_model_gamma_pc, type = "response")
  )

# Predictions for 2017
df_2017 <- df_2017 %>%
  mutate(
    pred_dlnm_2017 = predict(dlnm_model_gamma_estac_pc, newdata = df_2017, type = "response"),
    pred_glm_2017 = predict(glm_model_gamma_pc, newdata = df_2017, type = "response")
  )

# Error metrics for 2017 forecast
errores_2017 <- bind_rows(
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$pred_dlnm_train, "DLNM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$pred_glm_train, "GLM_train"),
  calcular_errores(df_2017$Riesgo_relativ_contagio, df_2017$pred_dlnm_2017, "DLNM_2017"),
  calcular_errores(df_2017$Riesgo_relativ_contagio, df_2017$pred_glm_2017, "GLM_2017")
)


## 11.3 Forecast for 2018 ----

# Train-test split (2018 as test)
df_train <- df_con_crossbasis_riesgos %>% filter(year != 2018)
df_2018 <- df_con_crossbasis_riesgos %>% filter(year == 2018)

# DLNM model excluding 2018
dlnm_model_gamma_estac_pc <- glm(
  formula = formula_dlnm,
  family = Gamma(link = "log"),
  data = df_train
)

# GLM model excluding 2018
glm_model_gamma_pc <- glm(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  family = Gamma(link = "log"),
  data = df_train
)

# Predictions on training data
df_train <- df_train %>%
  mutate(
    pred_dlnm_train = predict(dlnm_model_gamma_estac_pc, type = "response"),
    pred_glm_train = predict(glm_model_gamma_pc, type = "response")
  )

# Predictions for 2018
df_2018 <- df_2018 %>%
  mutate(
    pred_dlnm_2018 = predict(dlnm_model_gamma_estac_pc, newdata = df_2018, type = "response"),
    pred_glm_2018 = predict(glm_model_gamma_pc, newdata = df_2018, type = "response")
  )

# Error metrics for 2018 forecast
errores_2018 <- bind_rows(
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$pred_dlnm_train, "DLNM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$pred_glm_train, "GLM_train"),
  calcular_errores(df_2018$Riesgo_relativ_contagio, df_2018$pred_dlnm_2018, "DLNM_2018"),
  calcular_errores(df_2018$Riesgo_relativ_contagio, df_2018$pred_glm_2018, "GLM_2018")
)


## 11.4 Forecast for 2019 ----

# Train-test split (2019 as test)
df_train <- df_con_crossbasis_riesgos %>% filter(year != 2019)
df_2019 <- df_con_crossbasis_riesgos %>% filter(year == 2019)

# DLNM model excluding 2019
dlnm_model_gamma_estac_pc <- glm(
  formula = formula_dlnm,
  family = Gamma(link = "log"),
  data = df_train
)

# GLM model excluding 2019
glm_model_gamma_pc <- glm(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  family = Gamma(link = "log"),
  data = df_train
)

# Predictions on training data
df_train <- df_train %>%
  mutate(
    pred_dlnm_train = predict(dlnm_model_gamma_estac_pc, type = "response"),
    pred_glm_train = predict(glm_model_gamma_pc, type = "response")
  )

# Predictions for 2019
df_2019 <- df_2019 %>%
  mutate(
    pred_dlnm_2019 = predict(dlnm_model_gamma_estac_pc, newdata = df_2019, type = "response"),
    pred_glm_2019 = predict(glm_model_gamma_pc, newdata = df_2019, type = "response")
  )

# Error metrics for 2019 forecast
errores_2019 <- bind_rows(
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$pred_dlnm_train, "DLNM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$pred_glm_train, "GLM_train"),
  calcular_errores(df_2019$Riesgo_relativ_contagio, df_2019$pred_dlnm_2019, "DLNM_2019"),
  calcular_errores(df_2019$Riesgo_relativ_contagio, df_2019$pred_glm_2019, "GLM_2019")
)

# Combine and display all error results
errores_totales <- bind_rows(errores_2017, errores_2018, errores_2019)

datatable(errores_totales, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(errores_totales),
    color = "black",
    backgroundColor = "lightgray"
  )             


## 11.5 Forecast plots ----

# Create full real series
serie_real <- ts(df_con_crossbasis_riesgos$Riesgo_relativ_contagio, start = c(2000, 14), frequency = 52)

# Create forecast series as ts, inserting NA where no forecast exists
serie_pred_2017 <- ts(rep(NA, length(serie_real)), start = c(2000, 14), frequency = 52)
serie_pred_2017[which(df_con_crossbasis_riesgos$year == 2017)] <- df_2017$pred_dlnm_2017

serie_pred_2018 <- ts(rep(NA, length(serie_real)), start = c(2000, 14), frequency = 52)
serie_pred_2018[which(df_con_crossbasis_riesgos$year == 2018)] <- df_2018$pred_dlnm_2018

serie_pred_2019 <- ts(rep(NA, length(serie_real)), start = c(2000, 14), frequency = 52)
serie_pred_2019[which(df_con_crossbasis_riesgos$year == 2019)] <- df_2019$pred_dlnm_2019

# Plot forecasts
autoplot(serie_real, series = "Observed", color = "green", size = 0.4) +
  autolayer(serie_pred_2017, series = "Forecast 2017", color = "red", size = 0.9, alpha = 0.7) +
  autolayer(serie_pred_2018, series = "Forecast 2018", color = "blue", size = 0.9, alpha = 0.7) +
  autolayer(serie_pred_2019, series = "Forecast 2019", color = "#006600", size = 0.9, alpha = 0.7) +
  labs(
    x = "Epidemiological week",
    y = "Relative risk",
    color = "Series"
  ) + theme_bw()
