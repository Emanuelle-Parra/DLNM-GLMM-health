# 1. Libraries  --------------------------------------------------------------

## 1.1 Data manipulation and transformation ----

library(dplyr) # Data manipulation
library(tidyr) # Data transformation
library(tidyverse) # Collection of packages for data analysis

## 1.2 Visualization and plotting packages ----

library(ggplot2) # ggplot-style plots
library(ggseas) # Visualization of seasonal components
library(scales) # Customization of scales in plots
library(viridis) # Perceptually uniform color palettes
library(corrplot) # Correlation plots
library(GGally) # Enhancements for ggplot2 (plot matrices)

## 1.3 Statistical analysis and modeling packages ----

library(car) # Linear model analysis and diagnostics
library(dlnm) # Distributed lag non-linear models for time series
library(forecast) # Time series forecasting
library(glmmTMB) # Generalized nonlinear mixed models
library(lme4) # Mixed effects models
library(mgcv) # Fitting Generalized Additive Models (GAMs)
library(gamlss) # Fitting GAMLSS models for zero-inflation
library(MASS) # GLM model fitting tools
library(splines) # Generation of cubic splines
library(urca) # Unit root tests for time series
library(tseries) # Time series analysis

## 1.4 Information analysis and metrics packages ----

library(energy) # Distance correlation (dCor)
library(infotheo) # Mutual Information (MI)
library(Metrics) # Model performance metrics

## 1.5 Spatial data packages ----

library(sf) # Handling spatial data

## 1.6 Table creation packages ----

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
#' The data include summarized values of discharges, relative risk of infection,
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

data_region7 <- datos_finales_subregion %>%
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

# A column is added to identify each region, and all data are combined into a single dataset.

data_region1$region <- "PC"
data_region2$region <- "PN"
data_region3$region <- "PS"
data_region4$region <- "RMS"
data_region5$region <- "VC"
data_region6$region <- "RA"
data_region7$region <- "RN"

data_pc <- data_region1
data_ps <- data_region2
data_pn <- data_region3
data_rms <- data_region4
data_vc <- data_region5
data_ra <- data_region6
data_ra <- data_region7
# 4. Correlational analysis using permutation tests ---------------

#' This block defines a correlation function between lagged predictor variables
#' and the response variable in region RA using different methods:
#' - Pearson correlation (linear)
#' - Distance correlation (dCor) (non-linear)
#' - Mutual information (MI) (non-linear)
#' Significance is evaluated through permutation tests.


## 4.1 Functions to configure the crossbasis parameterization ----

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
#' - method: character, specifies the dependency method to use: "pearson" (default), "dcor", or "MI".
#' - n_perm: integer, number of permutations to perform (default is 50).
#'
#' Value:
#' - A numeric value between 0 and 1 representing the p-value of the permutation test.
#'
#' Details:
#' - For "pearson", |cor(x, y)| is calculated with random permutations of x.
#' - For "dcor", the dcor() function from the energy package is used.
#' - For "MI", mutual information between discretized vectors is computed using mutinformation() from infotheo.

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
  retura(p_value)
}


#' ------------------------------------------------------------------------------
#' buscar_mejor_modelo()
#' ------------------------------------------------------------------------------
#' Function to identify the best model among combinations of distributed
#' non-linear lag structures (DLNM), evaluating different types of exposure and lag
#' functions, using Poisson and negative binomial regression models.
#' The optimal model is selected based on goodness-of-fit and prediction metrics.
#'
#' Parameters:
#' - data: data.frame containing the input data
#' - variable: name of the exposure variable (string)
#' - respuesta: name of the response variable (string)
#' - max_lag: maximum number of lags to evaluate
#' - max_df_var: maximum degrees of freedom for the exposure variable
#' - max_df_lag: maximum degrees of freedom for the lag
#'
#' Returns:
#' - A list containing:
#'   - mejor_modelo: data.frame with the row of the optimal model
#'   - resultado_general: data.frame with all evaluated combinations
#' ------------------------------------------------------------------------------

buscar_mejor_modelo <- function(data, variable, respuesta, max_lag, max_df_var, max_df_lag) {
  #' --------------------------------------------------------------------------
  #' Create structure to store results of the process
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
  #' Auxiliary function to evaluate fit metrics for a given model
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
  #' Initial base evaluation: linear function and strata for lag
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
  #' Evaluation with cubic spline functions for variable and lag
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
  #' Identify the model with the best overall score
  #' --------------------------------------------------------------------------
  mejor_modelo <- resultados[resultados$score == max(resultados$score), ]
  
  cat("\nBest model based on score:\n")
  print(mejor_modelo)
  list(
    mejor_modelo = mejor_modelo,
    resultado_general = resultados
  )
}


max_lag <- 14

## 4.2 Correlational analysis in subregion RA1 ----

### 4.2.1 Mean temperature ----

#' initialize results data frame
results_temp <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA1")
  
  X_lagged <- head(data_lag$Temp_media, length(data_lag$Temp_media) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store
  results_temp <- results_temp %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric
max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' visualization of correlations by lag
ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (non-linear)" = "red",
    "MI (non-linear)" = "green"
  )) +
  labs(x = "Temperature lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  # annotate max and min values
  annotate("text",
           x = which.max(results_temp$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )


#' evaluation of univariate DLNM models for temperature in subregion RA1

data_ra1 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA1")

res <- buscar_mejor_modelo(
  data = data_ra1,
  variable = "Temp_media",
  respuesta = "egreso_semana",
  max_lag = 3,
  max_df_var = 3,
  max_df_lag = 3
)

#' interactive table visualization of results

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.2.2 Mean precipitation ----

#' initialize data.frame to store results by lag

results_prec <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)

#' compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_ra1 <- data_ra_subreg %>% filter(Subreg_climat == "RA1")
  
  X_lagged <- head(data_ra1$precip_media, n = length(data_ra1$precip_media) - lag)
  Y_lagged <- tail(data_ra1$egreso_semana, n = length(data_ra1$egreso_semana) - lag)
  
  #' compute metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store
  results_prec <- rbind(
    results_prec,
    data.frame(
      Lag = lag,
      Pearson = pearson_corr, Pearson_p = pearson_p,
      dCor = dcor_val, dCor_p = dcor_p,
      MI = MI_val, MI_p = MI_p
    )
  )
}

#' identify extreme values for annotations

max_pearson <- max(results_prec$Pearson)
min_pearson <- min(results_prec$Pearson)
max_dcor <- max(results_prec$dCor)
min_dcor <- min(results_prec$dCor)
max_mi <- max(results_prec$MI)
min_mi <- min(results_prec$MI)

#' correlation plot by lag

ggplot(results_prec, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Precipitation lag (k)", y = "Coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (non-linear)" = "red",
    "MI (non-linear)" = "green"
  )) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_prec$Pearson), y = max_pearson,
           label = paste("Max: ", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$Pearson), y = min_pearson,
           label = paste("Min: ", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$dCor), y = max_dcor,
           label = paste("Max: ", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$dCor), y = min_dcor,
           label = paste("Min: ", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$MI), y = max_mi,
           label = paste("Max: ", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$MI), y = min_mi,
           label = paste("Min: ", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' evaluation of univariate DLNM models for precipitation in subregion RA1

res <- buscar_mejor_modelo(
  data_ra_subreg %>% filter(Subreg_climat == "RA1"), # Filter subregion RA1
  variable = "precip_media", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 6, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' interactive table showing all evaluated models sorted by combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )

### 4.2.3 Mean relative humidity ----

#' initialize dataframe to store results by lag

results_rh <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)

#' compute correlations for each lag from 1 to `max_lag`
for (lag in 1:max_lag) {
  df_filtrada <- data_ra_subreg %>% filter(Subreg_climat == "RA1")
  
  X_lagged <- head(df_filtrada$RH, length(df_filtrada$RH) - lag)
  Y_lagged <- tail(df_filtrada$egreso_semana, length(df_filtrada$egreso_semana) - lag)
  
  #' correlations and p-values
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store results
  results_rh <- rbind(
    results_rh,
    data.frame(
      Lag = lag,
      Pearson = pearson_corr, Pearson_p = pearson_p,
      dCor = dcor_val, dCor_p = dcor_p,
      MI = MI_val, MI_p = MI_p
    )
  )
}

#' compute extremes for annotations in the plot

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' plot correlations by lag

ggplot(results_rh, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    x = "Relative Humidity lag (k)",
    y = "Coefficient"
  ) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (non-linear)" = "red",
    "MI (non-linear)" = "green"
  )) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_rh$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )


#' evaluation of univariate DLNM models for relative humidity in subregion RA1

res <- buscar_mejor_modelo(
  data_ra_subreg %>% filter(Subreg_climat == "RA1"), # filter subregion RA1
  variable = "RH", # explanatory variable
  respuesta = "egreso_semana", # response variable
  max_lag = 11, # maximum lag
  max_df_var = 3, # maximum df for the variable
  max_df_lag = 3 # maximum df for the lags
)

#' interactive table displaying all evaluated models sorted by combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general), # apply style to all columns
    color = "black", # text color
    backgroundColor = "lightgray" # light gray background
  )

### 4.2.4 AOD Index ----

#' initialize dataframe to store results by lag

results_aod <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations for each lag

for (lag in 1:max_lag) {
  datos <- data_ra_subreg %>% filter(Subreg_climat == "RA1")
  
  X_lagged <- head(datos$aerosol, length(datos$aerosol) - lag)
  Y_lagged <- tail(datos$egreso_semana, length(datos$egreso_semana) - lag)
  
  # Pearson correlation
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  # distance correlation
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  # mutual information
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  # store
  results_aod <- rbind(
    results_aod,
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

#' plot correlation coefficients for each method
ggplot(results_aod, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "AOD index lag (k)", y = "Coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (non-linear)" = "red",
    "MI (non-linear)" = "green"
  )) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_aod$Pearson), y = max(results_aod$Pearson),
           label = paste("Max:", round(max(results_aod$Pearson), 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$Pearson), y = min(results_aod$Pearson),
           label = paste("Min:", round(min(results_aod$Pearson), 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$dCor), y = max(results_aod$dCor),
           label = paste("Max:", round(max(results_aod$dCor), 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$dCor), y = min(results_aod$dCor),
           label = paste("Min:", round(min(results_aod$dCor), 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$MI), y = max(results_aod$MI),
           label = paste("Max:", round(max(results_aod$MI), 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$MI), y = min(results_aod$MI),
           label = paste("Min:", round(min(results_aod$MI), 2)), color = "green", vjust = 1.5
  )

#' evaluation of univariate DLNM models for AOD in subregion RA1
res <- buscar_mejor_modelo(
  data_ra_subreg %>% filter(Subreg_climat == "RA1"), # Filter subregion RA1
  variable = "aerosol", # Explanatory variable (AOD)
  respuesta = "egreso_semana", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' interactive table displaying all evaluated models sorted by combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
## 4.3 Correlational analysis in subregion RA2 ----

### 4.3.1 Mean temperature ----

#' initialize results data frame
results_temp <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA2")
  
  X_lagged <- head(data_lag$Temp_media, length(data_lag$Temp_media) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  # correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  # store
  results_temp <- results_temp %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric
max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' visualization of correlations by lag
ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (non-linear)" = "red",
    "MI (non-linear)" = "green"
  )) +
  labs(x = "Temperature lag (k)", y = "Coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_temp$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' selection of best DLNM model for mean temperature in subregion RA2

#' filter climatic subregion RA2 and fit DLNM models
res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA2"), # Filter subregion RA2
  variable = "Temp_media", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' interactive table displaying all evaluated models sorted by combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )

### 4.3.2 Mean precipitation -------

#' initialize results data frame

results_prec <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA2")
  
  X_lagged <- head(data_lag$precip_media, length(data_lag$precip_media) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store
  results_prec <- results_prec %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric

max_pearson <- max(results_prec$Pearson)
min_pearson <- min(results_prec$Pearson)
max_dcor <- max(results_prec$dCor)
min_dcor <- min(results_prec$dCor)
max_mi <- max(results_prec$MI)
min_mi <- min(results_prec$MI)

#' visualization of correlations by lag

ggplot(results_prec, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (non-linear)" = "red",
    "MI (non-linear)" = "green"
  )) +
  labs(x = "Precipitation lag (k)", y = "Coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  #' annotations for max and min values
  annotate("text",
           x = which.max(results_prec$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' evaluation of univariate DLNM models for precipitation in subregion RA2
res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA2"), # Filter subregion RA2
  variable = "precip_media", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' interactive table displaying all evaluated models sorted by combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.3.3 Average Relative Humidity -----

#' initialize results data frame

results_rh <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA2")
  
  X_lagged <- head(data_lag$RH, length(data_lag$RH) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  results_rh <- results_rh %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute maxima and minima for plot annotations

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' visualization with ggplot2
ggplot(results_rh, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Relative Humidity Lag (k)", y = "coefficient") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_rh$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' fit DLNM models with different combinations of functions and degrees of freedom

res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA2"), # Filter subregion RA2
  variable = "RH", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 9, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models ordered by combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.3.4 AOD Index -----

#' initialize results data frame
results_aod <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>% filter(Subreg_climat == "RA2")
  
  X_lagged <- head(data_lag$aerosol, length(data_lag$aerosol) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  results_aod <- results_aod %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric

max_pearson <- max(results_aod$Pearson)
min_pearson <- min(results_aod$Pearson)
max_dcor <- max(results_aod$dCor)
min_dcor <- min(results_aod$dCor)
max_mi <- max(results_aod$MI)
min_mi <- min(results_aod$MI)

#' lagged correlation visualization

ggplot(results_aod, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "AOD Index Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_aod$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' fit DLNM models with different combinations of functions and degrees of freedom

res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA2"), # Filter subregion RA2
  variable = "aerosol", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 14, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models ordered by combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
## 4.4 Correlation Analysis in Subregion RA3 ----

### 4.4.1 Mean Temperature ----

#' initialize results data frame
results_temp <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  X_lagged <- head(
    (data_ra_subreg %>% filter(Subreg_climat == "RA3"))$Temp_media,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA3"))$Temp_media) - lag
  )
  Y_lagged <- tail(
    (data_ra_subreg %>% filter(Subreg_climat == "RA3"))$egreso_semana,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA3"))$egreso_semana) - lag
  )
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store results
  results_temp <- rbind(
    results_temp,
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

#' compute extrema for plot annotation

max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' plot lagged correlations

ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature Lag (k)", y = "coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_temp$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' filter climatic subregion RA3

data_ra3 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA3")

#' search for best model using predefined function

res <- buscar_mejor_modelo(
  data = data_ra3,
  variable = "Temp_media",
  respuesta = "egreso_semana",
  max_lag = 8,
  max_df_var = 3,
  max_df_lag = 3
)

#' display results in interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.4.2 Mean Precipitation ----

#' initialize results data frame

results_prec <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA3")
  
  X_lagged <- head(data_lag$precip_media, length(data_lag$precip_media) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store results
  results_prec <- results_prec %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric

max_pearson <- max(results_prec$Pearson)
min_pearson <- min(results_prec$Pearson)
max_dcor <- max(results_prec$dCor)
min_dcor <- min(results_prec$dCor)
max_mi <- max(results_prec$MI)
min_mi <- min(results_prec$MI)

#' lagged correlation visualization

ggplot(results_prec, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "Precipitation Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  #' annotations of maxima and minima
  annotate("text",
           x = which.max(results_prec$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' filter climatic subregion RA3

data_ra3 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA3")

#' search for best model using predefined function

res <- buscar_mejor_modelo(
  data = data_ra3,
  variable = "precip_media", # explanatory variable
  respuesta = "egreso_semana", # response variable
  max_lag = 10, # maximum lag to evaluate
  max_df_var = 3, # degrees of freedom for the variable
  max_df_lag = 3 # degrees of freedom for the lags
)

#' display results in interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.4.3 Mean Relative Humidity ----

#' initialize results data frame

results_rh <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA3")
  
  X_lagged <- head(data_lag$RH, length(data_lag$RH) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  results_rh <- results_rh %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' lagged correlation visualization

ggplot(results_rh, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "Relative Humidity Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_rh$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' fit DLNM models with different combinations of functions and degrees of freedom

res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA3"), # Filter subregion RA3
  variable = "RH", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 4, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models ordered by combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general), # Apply to all columns
    color = "black", # Text color
    backgroundColor = "lightgray" # Background color
  )
### 4.4.4 AOD Index ----

#' initialize results data frame

results_aod <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA3")
  
  X_lagged <- head(data_lag$aerosol, length(data_lag$aerosol) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  results_aod <- rbind(results_aod, data.frame(
    Lag = lag,
    Pearson = pearson_corr,
    Pearson_p = pearson_p,
    dCor = dcor_val,
    dCor_p = dcor_p,
    MI = MI_val,
    MI_p = MI_p
  ))
}

#' compute extreme values per metric

max_pearson <- max(results_aod$Pearson)
min_pearson <- min(results_aod$Pearson)
max_dcor <- max(results_aod$dCor)
min_dcor <- min(results_aod$dCor)
max_mi <- max(results_aod$MI)
min_mi <- min(results_aod$MI)

#' lagged correlation visualization

ggplot(results_aod, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "AOD Index Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_aod$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' fit DLNM models with different combinations of functions and degrees of freedom

res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA3"), # Filter subregion RA3
  variable = "aerosol", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 10, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models ordered by combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
## 4.5 Correlation Analysis in Subregion RA4 ----

### 4.5.1 Mean Temperature ----

#' initialize results data frame
results_temp <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  X_lagged <- head(
    (data_ra_subreg %>% filter(Subreg_climat == "RA4"))$Temp_media,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA4"))$Temp_media) - lag
  )
  Y_lagged <- tail(
    (data_ra_subreg %>% filter(Subreg_climat == "RA4"))$egreso_semana,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA4"))$egreso_semana) - lag
  )
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store results
  results_temp <- rbind(
    results_temp,
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

#' compute extrema for plot annotation

max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' plot lagged correlations

ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature Lag (k)", y = "coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_temp$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' filter climatic subregion RA4

data_ra4 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA4")

#' search for best model using predefined function

res <- buscar_mejor_modelo(
  data = data_ra4,
  variable = "Temp_media",
  respuesta = "egreso_semana",
  max_lag = 12,
  max_df_var = 3,
  max_df_lag = 3
)

#' display results in interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.5.2 Mean Precipitation ----

#' initialize results data frame

results_prec <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA4")
  
  X_lagged <- head(data_lag$precip_media, length(data_lag$precip_media) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store results
  results_prec <- results_prec %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric

max_pearson <- max(results_prec$Pearson)
min_pearson <- min(results_prec$Pearson)
max_dcor <- max(results_prec$dCor)
min_dcor <- min(results_prec$dCor)
max_mi <- max(results_prec$MI)
min_mi <- min(results_prec$MI)

#' lagged correlation visualization

ggplot(results_prec, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "Precipitation Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  #' annotations of maxima and minima
  annotate("text",
           x = which.max(results_prec$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' filter climatic subregion RA4

data_ra4 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA4")

#' search for best model using predefined function

res <- buscar_mejor_modelo(
  data = data_ra4,
  variable = "precip_media", # explanatory variable
  respuesta = "egreso_semana", # response variable
  max_lag = 4, # maximum lag to evaluate
  max_df_var = 3, # degrees of freedom for the variable
  max_df_lag = 3 # degrees of freedom for the lags
)

#' display results in interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.5.3 Mean Relative Humidity ----

#' initialize results data frame

results_rh <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA4")
  
  X_lagged <- head(data_lag$RH, length(data_lag$RH) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  results_rh <- results_rh %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' lagged correlation visualization

ggplot(results_rh, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "Relative Humidity Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_rh$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' fit DLNM models with different combinations of functions and degrees of freedom

res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA4"), # Filter subregion RA4
  variable = "RH", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 4, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models ordered by combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general), # Apply to all columns
    color = "black", # Text color
    backgroundColor = "lightgray" # Background color
  )
### 4.5.4 AOD Index ----

#' initialize results data frame

results_aod <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA4")
  
  X_lagged <- head(data_lag$aerosol, length(data_lag$aerosol) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  results_aod <- rbind(results_aod, data.frame(
    Lag = lag,
    Pearson = pearson_corr,
    Pearson_p = pearson_p,
    dCor = dcor_val,
    dCor_p = dcor_p,
    MI = MI_val,
    MI_p = MI_p
  ))
}

#' compute extreme values per metric
max_pearson <- max(results_aod$Pearson)
min_pearson <- min(results_aod$Pearson)
max_dcor <- max(results_aod$dCor)
min_dcor <- min(results_aod$dCor)
max_mi <- max(results_aod$MI)
min_mi <- min(results_aod$MI)

#' lagged correlation visualization
ggplot(results_aod, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "AOD Index Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_aod$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' fit DLNM models with different combinations of functions and degrees of freedom
res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA4"), # Filter subregion RA4
  variable = "aerosol", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 13, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models ordered by combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
## 4.6 Correlation Analysis in Subregion RA5 ----

### 4.6.1 Mean Temperature ----

#' initialize results data frame
results_temp <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  X_lagged <- head(
    (data_ra_subreg %>% filter(Subreg_climat == "RA5"))$Temp_media,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA5"))$Temp_media) - lag
  )
  Y_lagged <- tail(
    (data_ra_subreg %>% filter(Subreg_climat == "RA5"))$egreso_semana,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA5"))$egreso_semana) - lag
  )
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store results
  results_temp <- rbind(
    results_temp,
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

#' compute extrema for plot annotation

max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' plot lagged correlations

ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature Lag (k)", y = "coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_temp$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_temp$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_temp$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' filter climatic subregion RA5

data_ra5 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA5")

#' search for best model using predefined function

res <- buscar_mejor_modelo(
  data = data_ra5,
  variable = "Temp_media",
  respuesta = "egreso_semana",
  max_lag = 9,
  max_df_var = 3,
  max_df_lag = 3
)

#' display results in interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.6.2 Mean Precipitation ----

#' initialize results data frame

results_prec <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA5")
  
  X_lagged <- head(data_lag$precip_media, length(data_lag$precip_media) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' store results
  results_prec <- results_prec %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric

max_pearson <- max(results_prec$Pearson)
min_pearson <- min(results_prec$Pearson)
max_dcor <- max(results_prec$dCor)
min_dcor <- min(results_prec$dCor)
max_mi <- max(results_prec$MI)
min_mi <- min(results_prec$MI)

#' lagged correlation visualization

ggplot(results_prec, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "Precipitation Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  #' annotations of maxima and minima
  annotate("text",
           x = which.max(results_prec$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_prec$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_prec$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' filter climatic subregion RA5

data_ra5 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA5")

#' search for best model using predefined function

res <- buscar_mejor_modelo(
  data = data_ra5,
  variable = "precip_media", # explanatory variable
  respuesta = "egreso_semana", # response variable
  max_lag = 12, # maximum lag to evaluate
  max_df_var = 3, # degrees of freedom for the variable
  max_df_lag = 3 # degrees of freedom for the lags
)

#' display results in interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.6.3 Mean Relative Humidity ----

#' initialize results data frame

results_rh <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA5")
  
  X_lagged <- head(data_lag$RH, length(data_lag$RH) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  results_rh <- results_rh %>%
    add_row(
      Lag = lag,
      Pearson = pearson_corr,
      Pearson_p = pearson_p,
      dCor = dcor_val,
      dCor_p = dcor_p,
      MI = MI_val,
      MI_p = MI_p
    )
}

#' compute extreme values per metric

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' lagged correlation visualization

ggplot(results_rh, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "Relative Humidity Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_rh$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_rh$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_rh$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' fit DLNM models with different combinations of functions and degrees of freedom

res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA5"), # Filter subregion RA5
  variable = "RH", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 11, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models ordered by combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general), # Apply to all columns
    color = "black", # Text color
    backgroundColor = "lightgray" # Background color
  )
### 4.6.4 AOD Index ----

#' initialize results data frame

results_aod <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' compute correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA5")
  
  X_lagged <- head(data_lag$aerosol, length(data_lag$aerosol) - lag)
  Y_lagged <- tail(data_lag$egreso_semana, length(data_lag$egreso_semana) - lag)
  
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  results_aod <- rbind(results_aod, data.frame(
    Lag = lag,
    Pearson = pearson_corr,
    Pearson_p = pearson_p,
    dCor = dcor_val,
    dCor_p = dcor_p,
    MI = MI_val,
    MI_p = MI_p
  ))
}

#' compute extreme values per metric
max_pearson <- max(results_aod$Pearson)
min_pearson <- min(results_aod$Pearson)
max_dcor <- max(results_aod$dCor)
min_dcor <- min(results_aod$dCor)
max_mi <- max(results_aod$MI)
min_mi <- min(results_aod$MI)

#' lagged correlation visualization
ggplot(results_aod, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
  )) +
  labs(x = "AOD Index Lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text",
           x = which.max(results_aod$Pearson), y = max_pearson,
           label = paste("Max:", round(max_pearson, 2)), color = "blue", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$Pearson), y = min_pearson,
           label = paste("Min:", round(min_pearson, 2)), color = "blue", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$dCor), y = max_dcor,
           label = paste("Max:", round(max_dcor, 2)), color = "red", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$dCor), y = min_dcor,
           label = paste("Min:", round(min_dcor, 2)), color = "red", vjust = 1.5
  ) +
  annotate("text",
           x = which.max(results_aod$MI), y = max_mi,
           label = paste("Max:", round(max_mi, 2)), color = "green", vjust = -0.5
  ) +
  annotate("text",
           x = which.min(results_aod$MI), y = min_mi,
           label = paste("Min:", round(min_mi, 2)), color = "green", vjust = 1.5
  )

#' fit DLNM models with different combinations of functions and degrees of freedom
res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA5"), # Filter subregion RA5
  variable = "aerosol", # Explanatory variable
  respuesta = "egreso_semana", # Response variable
  max_lag = 14, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models ordered by combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )

## 4.7 Combined Correlation Modeling Function ----

#' ------------------------------------------------------------------------------
#' Function: discrete_modeling_parameters()
#' ------------------------------------------------------------------------------
#' Evaluates multiple combinations of nonlinear basis functions for climate
#' variables (temperature, precipitation, relative humidity, and AOD), using
#' negative binomial distributed lag models. This function was implemented at the 
#' subregional level using all exposure variables in order to adequately 
#' parameterize the non-linear base functions.
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
#' - A list with a data.frame named 'resultados_completos' containing the
#'   model fitting metrics (AIC, BIC, MAE, MSE, RMSE, R2, MAPE, logLik, score)
#'   for each evaluated parameter combination.
#'
#' Details:
#' - The fitted model is of class glm.nb.
#' - Seasonal effects are included using as.factor(epi.week).
#' - Crossbasis terms are generated using the crossbasis function (dlnm package).
#' ------------------------------------------------------------------------------

discrete_modeling_parameters <- function(
    data, respuesta,
    temp_var, prec_var, rh_var, aod_var,
    max_lag_temp, max_df_var_temp, max_df_lag_temp,
    max_lag_prec, max_df_var_prec, max_df_lag_prec,
    max_lag_rh, max_df_var_rh, max_df_lag_rh,
    max_lag_AOD, max_df_var_AOD, max_df_lag_AOD
) {
  
  # Variable definitions and parameters
  variables <- list(
    temp = list(var = temp_var, max_lag = max_lag_temp, max_df_var = max_df_var_temp, max_df_lag = max_df_lag_temp),
    prec = list(var = prec_var, max_lag = max_lag_prec, max_df_var = max_df_var_prec, max_df_lag = max_df_lag_prec),
    rh   = list(var = rh_var,   max_lag = max_lag_rh,   max_df_var = max_df_var_rh,   max_df_lag = max_df_lag_rh),
    aod  = list(var = aod_var,  max_lag = max_lag_AOD,  max_df_var = max_df_var_AOD,  max_df_lag = max_df_lag_AOD)
  )
  
  # Data frame to store results
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
  
  # Model evaluation function to compute fitting metrics
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
  
  # Crossbasis generation function using max lag
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
  
  # Generate crossbases for all variables
  crossbases <- lapply(variables, generar_crossbasis, data = data)
  
  # Filter dataset based on max lag across all crossbases
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
  
  # Nested loops to combine model configurations across variables
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
          
          model_fit <- glm.nb(data_filtrada[[respuesta]] ~ 
                                cb_temp$crossbasis + cb_prec$crossbasis + 
                                cb_rh$crossbasis + cb_aod$crossbasis +
                                as.factor(epi.week),
                              link = "log", data = data_filtrada)
          
          resultados <- rbind(resultados,
                              evaluar_modelo(model_fit, data_filtrada, respuesta, "modelo_optim", params))
        }
      }
    }
  }
  
  return(list(resultados_completos = resultados))
}
# 5. Construction of Crossbasis Terms by Climatic Subregion ----

#' This block defines the crossbasis functions required for
#' DLNM-GLMM modeling of hospital discharges in climatic subregions
#' RA1, RA2, RA3, RA4, and RA5, based on climate and air pollution variables.

#' Sort the dataset by climatic subregion

data_ra_subreg <- data_ra_subreg %>%
  arrange(Subreg_climat)

## 5.1 Crossbasis for Subregion RA1 ----

cb_t1 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(Temp_media),
                    lag = 3, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p1 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(precip_media),
                    lag = 6, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh1 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(RH),
                     lag = 11, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod1 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(aerosol),
                      lag = 12, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.3 Crossbasis for Subregion RA2 ----

cb_t2 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA2") %>% pull(Temp_media),
                    lag = 5, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p2 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA2") %>% pull(precip_media),
                    lag = 12, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh2 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA2") %>% pull(RH),
                     lag = 9, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod2 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA2") %>% pull(aerosol),
                      lag = 13, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.4 Crossbasis for Subregion RA3 ----

cb_t3 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA3") %>% pull(Temp_media),
                    lag = 7, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p3 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA3") %>% pull(precip_media),
                    lag = 10, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh3 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA3") %>% pull(RH),
                     lag = 4, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod3 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA3") %>% pull(aerosol),
                      lag = 10, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.5 Crossbasis for Subregion RA4 ----

cb_t4 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA4") %>% pull(Temp_media),
                    lag = 3, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p4 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA4") %>% pull(precip_media),
                    lag = 4, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh4 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA4") %>% pull(RH),
                     lag = 6, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod4 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA4") %>% pull(aerosol),
                      lag = 13, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.6 Crossbasis for Subregion RA5 ----

cb_t5 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA5") %>% pull(Temp_media),
                    lag = 3, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p5 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA5") %>% pull(precip_media),
                    lag = 11, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh5 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA5") %>% pull(RH),
                     lag = 11, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod5 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA5") %>% pull(aerosol),
                      lag = 14, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.5 Merging Crossbasis Terms -----

#' Combine dataset with crossbasis matrices

df_con_crossbasis_egresos <- cbind(
  data_ra_subreg,
  
  #' temperature
  setNames(
    as.data.frame(rbind(cb_t1, cb_t2, cb_t3, cb_t4, cb_t5)),
    make.names(paste0("T_", colnames(cb_t1)), unique = TRUE)
  ),
  
  #' precipitation
  setNames(
    as.data.frame(rbind(cb_p1, cb_p2, cb_p3, cb_p4, cb_p5)),
    make.names(paste0("P_", colnames(cb_p1)), unique = TRUE)
  ),
  
  #' relative humidity
  setNames(
    as.data.frame(rbind(cb_rh1, cb_rh2, cb_rh3, cb_rh4, cb_rh5)),
    make.names(paste0("RH_", colnames(cb_rh1)), unique = TRUE)
  ),
  
  #' aerosol optical depth index (AOD)
  setNames(
    as.data.frame(rbind(cb_aod1, cb_aod2, cb_aod3, cb_aod4, cb_aod5)),
    make.names(paste0("AOD_", colnames(cb_aod1)), unique = TRUE)
  )
)

#' Remove initial observations per group based on maximum lag (lag = 14)

df_con_crossbasis_egresos <- df_con_crossbasis_egresos %>%
  group_by(Subreg_climat) %>%
  slice(-(1:14)) %>%
  ungroup()

#' Retrieve names of exposure-related columns generated by crossbasis

columnas_exposicion_egresos <- colnames(df_con_crossbasis_egresos)[12:ncol(df_con_crossbasis_egresos)]
columnas_exposicion_egresos

df_con_crossbasis_egresos$Subreg_climat <- as.factor(df_con_crossbasis_egresos$Subreg_climat)


## 5.6 Requirement for Zero-Inflated Models ----

#' Percentage of zeros across all subregions
summary(data_ra_subreg$egreso_semana)

100 * sum(data_ra_subreg$egreso_semana == 0) / length(data_ra_subreg$egreso_semana)

#' Percentage of zeros in subregion RA1
egreso_cero_ra1 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA1") %>%
  pull(egreso_semana) 

100 * sum(egreso_cero_ra1 == 0, na.rm = TRUE) / length(egreso_cero_ra1)

#' Percentage of zeros in subregion RA2
egreso_cero_ra2 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA2") %>%
  pull(egreso_semana) 

100 * sum(egreso_cero_ra2 == 0, na.rm = TRUE) / length(egreso_cero_ra2)

#' Percentage of zeros in subregion RA3
egreso_cero_ra3 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA3") %>%
  pull(egreso_semana) 

100 * sum(egreso_cero_ra3 == 0, na.rm = TRUE) / length(egreso_cero_ra3)

#' Percentage of zeros in subregion RA4
egreso_cero_ra4 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA4") %>%
  pull(egreso_semana) 

100 * sum(egreso_cero_ra4 == 0, na.rm = TRUE) / length(egreso_cero_ra4)

#' Percentage of zeros in subregion RA5
egreso_cero_ra5 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA5") %>%
  pull(egreso_semana) 

100 * sum(egreso_cero_ra5 == 0, na.rm = TRUE) / length(egreso_cero_ra5)

# 6. Poisson DLNM-GLMM and DLNM-GAMLSS Models ----

## 6.1 Simple Poisson GLMM Model ----

modelo_glmm_poi_ra <- glmmTMB(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + offset(logpop) +
    as.factor(epi.week),
  data = df_con_crossbasis_egresos,
  family = poisson(link = "log"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

## 6.2 DLNM + GLMM Poisson Model (2): with weekly seasonal factors ----

modelo_dlnm_glmm_poi_ra_2 <- glmmTMB(
  egreso_semana ~
    T_v1.l1 + T_v1.l2 + T_v1.l3 +
    T_v2.l1 + T_v2.l2 + T_v2.l3 +
    T_v3.l1 + T_v3.l2 + T_v3.l3 +
    P_v1.l1 + P_v1.l2 + P_v1.l3 +
    P_v2.l1 + P_v2.l2 + P_v2.l3 +
    P_v3.l1 + P_v3.l2 + P_v3.l3 +
    RH_v1.l1 + RH_v1.l2 + RH_v1.l3 +
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 +
    RH_v3.l1 + RH_v3.l2 + RH_v3.l3 +
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 +
    AOD_v2.l1 + AOD_v2.l2 + AOD_v2.l3 +
    AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +
    offset(logpop) + as.factor(epi.week) +
    (1 | Subreg_climat),
  data = df_con_crossbasis_egresos,
  family = poisson(link = "log"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

## 6.4 DLNM + GAMLSS Poisson Model (3): with weekly seasonal factors ----

modelo_gamlss_poi_3 <- gamlss(
  egreso_semana ~
    T_v1.l1 + T_v1.l2 + T_v1.l3 +
    T_v2.l1 + T_v2.l2 + T_v2.l3 +
    T_v3.l1 + T_v3.l2 + T_v3.l3 +
    P_v1.l1 + P_v1.l2 + P_v1.l3 +
    P_v2.l1 + P_v2.l2 + P_v2.l3 +
    P_v3.l1 + P_v3.l2 + P_v3.l3 +
    RH_v1.l1 + RH_v1.l2 + RH_v1.l3 +
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 +
    RH_v3.l1 + RH_v3.l2 + RH_v3.l3 +
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 +
    AOD_v2.l1 + AOD_v2.l2 + AOD_v2.l3 +
    AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +
    offset(logpop) + as.factor(epi.week) +
    random(Subreg_climat),
  data = df_con_crossbasis_egresos,
  family = ZIP(),
  control = gamlss.control(n.cyc = 50)
)

# 7. Negative Binomial DLNM-GLMM and DLNM-GAMLSS Models ----

## 7.1 Simple NB GLMM Model ----

modelo_glmm_nb_ra <- glmmTMB(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol + offset(logpop) +
    as.factor(epi.week),
  data = df_con_crossbasis_egresos,
  family = nbinom2(link = "log"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

## 7.2 DLNM + GLMM NB Model (2): with weekly seasonal factors ----

modelo_dlnm_glmm_nb_ra_2 <- glmmTMB(
  egreso_semana ~
    T_v1.l1 + T_v1.l2 + T_v1.l3 +
    T_v2.l1 + T_v2.l2 + T_v2.l3 +
    T_v3.l1 + T_v3.l2 + T_v3.l3 +
    P_v1.l1 + P_v1.l2 + P_v1.l3 +
    P_v2.l1 + P_v2.l2 + P_v2.l3 +
    P_v3.l1 + P_v3.l2 + P_v3.l3 +
    RH_v1.l1 + RH_v1.l2 + RH_v1.l3 +
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 +
    RH_v3.l1 + RH_v3.l2 + RH_v3.l3 +
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 +
    AOD_v2.l1 + AOD_v2.l2 + AOD_v2.l3 +
    AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +
    offset(logpop) + as.factor(epi.week) +
    (1 | Subreg_climat),
  data = df_con_crossbasis_egresos,
  family = nbinom2(link = "log"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

## 7.4 DLNM + GAMLSS NB Model (3): with weekly seasonal factors ----

modelo_gamlss_nb_3 <- gamlss(
  egreso_semana ~
    T_v1.l1 + T_v1.l2 + T_v1.l3 +
    T_v2.l1 + T_v2.l2 + T_v2.l3 +
    T_v3.l1 + T_v3.l2 + T_v3.l3 +
    P_v1.l1 + P_v1.l2 + P_v1.l3 +
    P_v2.l1 + P_v2.l2 + P_v2.l3 +
    P_v3.l1 + P_v3.l2 + P_v3.l3 +
    RH_v1.l1 + RH_v1.l2 + RH_v1.l3 +
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 +
    RH_v3.l1 + RH_v3.l2 + RH_v3.l3 +
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 +
    AOD_v2.l1 + AOD_v2.l2 + AOD_v2.l3 +
    AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +
    offset(logpop) + as.factor(epi.week)  +
    random(Subreg_climat),
  data = df_con_crossbasis_egresos,
  family = ZINBI()
)

# 8. Optimal Model Selection ----

#' This block applies the `calcular_metricas()` function to the fitted models
#' (GLMM and GAMLSS, with different distributions), and builds a table sorted
#' by best fit score.

#' Sort predicted values within the dataset
df_con_crossbasis_egresos_ra <- df_con_crossbasis_egresos %>%
  mutate(
    #' GLMM Poisson Models
    ajustado_glmm_poi = fitted(modelo_glmm_poi_ra),
    
    #' DLNM + GLMM Poisson Models
    ajustado_dlnm_glmm_poi_2 = fitted(modelo_dlnm_glmm_poi_ra_2),
    
    #' GAMLSS Poisson Models
    ajustado_gamlss_poi_3 = fitted(modelo_gamlss_poi_3),
    
    #' GLMM Negative Binomial Models
    ajustado_glmm_nb = fitted(modelo_glmm_nb_ra),
    
    #' DLNM + GLMM Negative Binomial Models
    ajustado_dlnm_glmm_nb_2 = fitted(modelo_dlnm_glmm_nb_ra_2),
    
    #' GAMLSS Negative Binomial Models
    ajustado_gamlss_nb_3 = fitted(modelo_gamlss_nb_3)
  )
## 8.1 Performance Metrics for Each Model ----

#' ------------------------------------------------------------------------------
#' calcular_metricas()
#' ------------------------------------------------------------------------------
#' Function to compute performance metrics for a fitted model and its associated
#' predictions. Includes classical error metrics, information criteria, and a 
#' composite score summarizing overall performance.
#'
#' Parameters:
#' - modelo: fitted model object
#' - predicciones: numeric vector with model predictions
#' - obs: numeric vector with observed values
#' - nombre_modelo: string identifying the evaluated model
#'
#' Returns:
#' - A data frame with MAE, MSE, RMSE, MAPE, AIC, BIC, logLik, and Score metrics
#' ------------------------------------------------------------------------------

calcular_metricas <- function(modelo, predicciones, obs, nombre_modelo) {
  # Error metrics
  MAE <- mean(abs(obs - predicciones))
  MSE <- mean((obs - predicciones)^2)
  RMSE <- sqrt(MSE)
  
  # MAPE with epsilon correction
  epsilon <- 1e-10
  MAPE_val <- mean(abs((obs - predicciones) / (obs + epsilon))) * 100
  
  # Information criteria and log-likelihood
  AIC_val <- tryCatch(AIC(modelo), error = function(e) NA)
  BIC_val <- tryCatch(BIC(modelo), error = function(e) NA)
  ll <- tryCatch(as.numeric(logLik(modelo)), error = function(e) NA)
  
  # Composite score: inverse error + adjusted logLik
  logLik_term <- if (!is.na(ll) && ll < 0) 1 / abs(ll) else if (!is.na(ll)) ll else 0
  score <- (1 / MAE) + (1 / MSE) + (1 / RMSE) + (1 / MAPE_val) + logLik_term
  
  # Output data frame
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

#' Combine results from all models

resultados_modelos <- rbind(
  calcular_metricas(
    modelo_glmm_poi_ra,
    df_con_crossbasis_egresos_ra$ajustado_glmm_poi,
    df_con_crossbasis_egresos_ra$egreso_semana,
    "GLMM_Poi"
  ),
  calcular_metricas(
    modelo_dlnm_glmm_poi_ra_2,
    df_con_crossbasis_egresos_ra$ajustado_dlnm_glmm_poi_2,
    df_con_crossbasis_egresos_ra$egreso_semana,
    "DLNM_GLMM_Poi_2"
  ),
  calcular_metricas(
    modelo_gamlss_poi_3,
    df_con_crossbasis_egresos_ra$ajustado_gamlss_poi_3,
    df_con_crossbasis_egresos_ra$egreso_semana,
    "GAMLSS_Poi_3"
  ),
  calcular_metricas(
    modelo_glmm_nb_ra,
    df_con_crossbasis_egresos_ra$ajustado_glmm_nb,
    df_con_crossbasis_egresos_ra$egreso_semana,
    "GLMM_NB"
  ),
  calcular_metricas(
    modelo_dlnm_glmm_nb_ra_2,
    df_con_crossbasis_egresos_ra$ajustado_dlnm_glmm_nb_2,
    df_con_crossbasis_egresos_ra$egreso_semana,
    "DLNM_GLMM_NB_2"
  ),
  calcular_metricas(
    modelo_gamlss_nb_3,
    df_con_crossbasis_egresos_ra$ajustado_gamlss_nb_3,
    df_con_crossbasis_egresos_ra$egreso_semana,
    "GAMLSS_NB_3"
  )
)

#' Sort models from best to worst by composite score

resultados_modelos <- resultados_modelos %>%
  arrange(desc(Score))

#' Display results in an interactive table

datatable(resultados_modelos, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados_modelos),
    color = "black",
    backgroundColor = "lightgray"
  )
## 8.2 Best Performing Model -----

dlnm_glmm_model_ra <- modelo_gamlss_poi_3
summary(dlnm_glmm_model_ra)


## 8.3 Subregional Metrics ----

#' Construct time series per model for RA1
df_ra1 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA1") %>%
  mutate(
    egresos_ts = ts(egreso_semana, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_glmm_nb, start = c(2000, 15), frequency = 52),
    gamlss_ts = ts(ajustado_gamlss_poi_3, start = c(2000, 15), frequency = 52)
  )

#' Construct time series per model for RA2
df_ra2 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA2") %>%
  mutate(
    egresos_ts = ts(egreso_semana, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_glmm_nb, start = c(2000, 15), frequency = 52),
    gamlss_ts = ts(ajustado_gamlss_poi_3, start = c(2000, 15), frequency = 52)
  )

#' Construct time series per model for RA3
df_ra3 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA3") %>%
  mutate(
    egresos_ts = ts(egreso_semana, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_glmm_nb, start = c(2000, 15), frequency = 52),
    gamlss_ts = ts(ajustado_gamlss_poi_3, start = c(2000, 15), frequency = 52)
  )

#' Construct time series per model for RA4
df_ra4 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA4") %>%
  mutate(
    egresos_ts = ts(egreso_semana, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_glmm_nb, start = c(2000, 15), frequency = 52),
    gamlss_ts = ts(ajustado_gamlss_poi_3, start = c(2000, 15), frequency = 52)
  )

#' Construct time series per model for RA5
df_ra5 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA5") %>%
  mutate(
    egresos_ts = ts(egreso_semana, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_glmm_nb, start = c(2000, 15), frequency = 52),
    gamlss_ts = ts(ajustado_gamlss_poi_3, start = c(2000, 15), frequency = 52)
  )


calcular_metricas_sub <- function(df, subregion, col_obs = "egresos_ts") {
  # Compute error metrics for fitted models by subregion
  # Returns the two best models according to lowest Score
  
  columnas_modelo <- setdiff(
    names(df)[grepl("_ts$", names(df))],
    col_obs
  )
  
  obs <- df[[col_obs]]
  epsilon <- 1e-10
  
  resultados <- lapply(columnas_modelo, function(nombre_modelo) {
    pred <- df[[nombre_modelo]]
    
    MAE <- mean(abs(obs - pred))
    MSE <- mean((obs - pred)^2)
    RMSE <- sqrt(MSE)
    MAPE_val <- mean(abs((obs - pred) / (obs + epsilon))) * 100
    
    score <- (1 / MAE) + (1 / MSE) + (1 / RMSE) + (1 / MAPE_val)
    
    data.frame(
      Subregión = subregion,
      Modelo = nombre_modelo,
      MAE = round(MAE, 3),
      MSE = round(MSE, 3),
      RMSE = round(RMSE, 3),
      MAPE = round(MAPE_val, 2),
      Score = round(score, 3)
    )
  }) %>%
    bind_rows() %>%
    arrange(Score)
  
  return(head(resultados, 2))
}

#' Apply function by subregion
resultados_modelos <- rbind(
  calcular_metricas_sub(df_ra1, "RA1", "egreso_semana"),
  calcular_metricas_sub(df_ra2, "RA2", "egreso_semana"),
  calcular_metricas_sub(df_ra3, "RA3", "egreso_semana"),
  calcular_metricas_sub(df_ra4, "RA4", "egreso_semana"),
  calcular_metricas_sub(df_ra5, "RA5", "egreso_semana")
)

#' Sort models by descending score
resultados_modelos <- resultados_modelos %>%
  arrange(desc(Score))

#' Display in interactive table
datatable(resultados_modelos, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados_modelos),
    color = "black",
    backgroundColor = "lightgray"
  )
# 9. Residual Diagnostics for Optimal Model ----

#' Simple residuals (similar to response or deviance residuals)
resid_simple_ra <- residuals(dlnm_glmm_model_ra, type = "simple")

#' Weighted residuals (similar to Pearson residuals)
resid_weighted_ra <- residuals(dlnm_glmm_model_ra, type = "weighted")

#' Partial residuals (used for assessing partial effects)
resid_partial_ra <- residuals(dlnm_glmm_model_ra, type = "partial")

#' Merge residuals into analysis dataframe
df_con_crossbasis_egresos_ra <- df_con_crossbasis_egresos_ra %>%
  mutate(
    resid_simple_ra = resid_simple_ra,
    resid_weighted_ra = resid_weighted_ra,
    resid_partial_ra = resid_partial_ra
  )


## 9.1 Residual Diagnostics ----

#' Q-Q plots to assess normality of global residuals
qqnorm(resid_simple_ra, main = "Simple Residuals")
qqline(resid_simple_ra, col = "red")

qqnorm(resid_weighted_ra, main = "Weighted Residuals")
qqline(resid_weighted_ra, col = "red")


#' Shapiro-Wilk test for normality
shapiro_test_simple <- shapiro.test(resid_simple_ra)
shapiro_test_weighted <- shapiro.test(resid_weighted_ra)

#' Display results
print(shapiro_test_simple)
print(shapiro_test_weighted)


#' Extract simple residuals by climate subregion

resid_simple_ra1 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA1") %>%
  pull(resid_simple_ra)

resid_simple_ra2 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA2") %>%
  pull(resid_simple_ra)

resid_simple_ra3 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA3") %>%
  pull(resid_simple_ra)

resid_simple_ra4 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA4") %>%
  pull(resid_simple_ra)

resid_simple_ra5 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA5") %>%
  pull(resid_simple_ra)


#' Extract weighted residuals by climate subregion

resid_weighted_ra1 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA1") %>%
  pull(resid_weighted_ra)

resid_weighted_ra2 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA2") %>%
  pull(resid_weighted_ra)

resid_weighted_ra3 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA3") %>%
  pull(resid_weighted_ra)

resid_weighted_ra4 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA4") %>%
  pull(resid_weighted_ra)

resid_weighted_ra5 <- df_con_crossbasis_egresos_ra %>%
  filter(Subreg_climat == "RA5") %>%
  pull(resid_weighted_ra)


#' Time series diagnostic plots for simple residuals by subregion
ggtsdisplay(resid_simple_ra1) # Subregion RA1
ggtsdisplay(resid_simple_ra2) # Subregion RA2
ggtsdisplay(resid_simple_ra3) # Subregion RA3
ggtsdisplay(resid_simple_ra4) # Subregion RA4
ggtsdisplay(resid_simple_ra5) # Subregion RA5

#' Time series diagnostic plots for weighted residuals by subregion
ggtsdisplay(resid_weighted_ra1) # Subregion RA1
ggtsdisplay(resid_weighted_ra2) # Subregion RA2
ggtsdisplay(resid_weighted_ra3) # Subregion RA3
ggtsdisplay(resid_weighted_ra4) # Subregion RA4
ggtsdisplay(resid_weighted_ra5) # Subregion RA5

# 10. Adjusted Effects from the Optimal Model ----

## 10.1 Comparative Prediction Plots ----

#' Comparative plot of predicted values - Subregion RA1
#' Comparison of observed vs. GLMM and GAMLSS models
autoplot(df_ra1$egresos_ts, series = "Observed") +
  autolayer(df_ra1$glmm_ts, series = "GLMM NB") +
  autolayer(df_ra1$gamlss_ts, series = "DLNM - GAMLSS NB") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM NB" = "blue",
    "DLNM - GAMLSS NB" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plot of predicted values - Subregion RA2
autoplot(df_ra2$egresos_ts, series = "Observed") +
  autolayer(df_ra2$glmm_ts, series = "GLMM NB") +
  autolayer(df_ra2$gamlss_ts, series = "DLNM - GAMLSS NB") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM NB" = "blue",
    "DLNM - GAMLSS NB" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plot of predicted values - Subregion RA3
autoplot(df_ra3$egresos_ts, series = "Observed") +
  autolayer(df_ra3$glmm_ts, series = "GLMM NB") +
  autolayer(df_ra3$gamlss_ts, series = "DLNM - GAMLSS NB") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM NB" = "blue",
    "DLNM - GAMLSS NB" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plot of predicted values - Subregion RA4
autoplot(df_ra4$egresos_ts, series = "Observed") +
  autolayer(df_ra4$glmm_ts, series = "GLMM NB") +
  autolayer(df_ra4$gamlss_ts, series = "DLNM - GAMLSS NB") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM NB" = "blue",
    "DLNM - GAMLSS NB" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plot of predicted values - Subregion RA5
autoplot(df_ra5$egresos_ts, series = "Observed") +
  autolayer(df_ra5$glmm_ts, series = "GLMM NB") +
  autolayer(df_ra5$gamlss_ts, series = "DLNM - GAMLSS NB") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM NB" = "blue",
    "DLNM - GAMLSS NB" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
## 10.2 Predictions ----
#' This section illustrates an example prediction behavior for one subregion.

#' Extract fixed coefficients and full variance-covariance matrix from the DLNM-GAMLSS Poisson model
coef_modelo <- coef(modelo_gamlss_poi_3)
vcov_modelo <- vcov(modelo_gamlss_poi_3, full = TRUE)

# Create directory if it doesn't exist
if (!dir.exists("random_effects_models")) {
  dir.create("random_effects_models")
}

# Save models and related objects
save(
  modelo_glmm_poi_ra,
  modelo_dlnm_glmm_poi_ra_2,
  modelo_gamlss_poi_3,
  modelo_glmm_nb_ra,
  modelo_dlnm_glmm_nb_ra_2,
  modelo_gamlss_nb_3,
  dlnm_glmm_model_ra,
  coef_modelo,
  vcov_modelo,
  file = "random_effects_models/dlnm_models_for_discharges_ra.RData"
)

# Load the saved models
load("random_effects_models/dlnm_models_for_discharges_ra.RData")

# Extract coefficients and covariance matrices for each variable in subregion RA1
coef_t <- coef_modelo[grep("^T_", names(coef_modelo))]
vcov_t <- as.matrix(vcov_modelo[grep("^T_", names(coef_modelo)), grep("^T_", names(coef_modelo))])

coef_p <- coef_modelo[grep("^P_", names(coef_modelo))]
vcov_p <- as.matrix(vcov_modelo[grep("^P_", names(coef_modelo)), grep("^P_", names(coef_modelo))])

coef_rh <- coef_modelo[grep("^RH_", names(coef_modelo))]
vcov_rh <- as.matrix(vcov_modelo[grep("^RH_", names(coef_modelo)), grep("^RH_", names(coef_modelo))])

coef_aod <- coef_modelo[grep("^AOD_", names(coef_modelo))]
vcov_aod <- as.matrix(vcov_modelo[grep("^AOD_", names(coef_modelo)), grep("^AOD_", names(coef_modelo))])

# Generate crosspred predictions for each exposure in subregion RA1
pred_dlnm_glmm_ra1_temp <- crosspred(
  cb_t1, coef = coef_t, vcov = vcov_t, model.link = "log",
  cen = mean(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(Temp_media), na.rm = TRUE),
  bylag = 0.1
)

pred_dlnm_glmm_ra1_prec <- crosspred(
  cb_p1, coef = coef_p, vcov = vcov_p, model.link = "log",
  cen = mean(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(precip_media), na.rm = TRUE),
  bylag = 0.1
)

pred_dlnm_glmm_ra1_rh <- crosspred(
  cb_rh1, coef = coef_rh, vcov = vcov_rh, model.link = "log",
  cen = mean(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(RH), na.rm = TRUE),
  bylag = 0.1
)

pred_dlnm_glmm_ra1_aod <- crosspred(
  cb_aod1, coef = coef_aod, vcov = vcov_aod, model.link = "log",
  cen = mean(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(aerosol), na.rm = TRUE),
  bylag = 0.1
)

### 10.2.1 Mean Temperature ----

#' 3D lag-response plot for mean temperature
plot(pred_dlnm_glmm_ra1_temp,
     xlab = "Mean temperature",
     zlab = "RR",
     theta = 300,
     phi = 10,
     lphi = 100)

#' Contour lag-response plot for mean temperature
plot(pred_dlnm_glmm_ra1_temp,
     "contour",
     xlab = "Mean temperature",
     ylab = "Lag",
     key.title = title("RR"))

#' Lag-response plot for specific temperature value (min observed)
plot(pred_dlnm_glmm_ra1_temp,
     "slices",
     var = min(pred_dlnm_glmm_ra1_temp$predvar),
     col = 2,
     ylab = "RR")

#' Lag-response plot for specific temperature value (max observed)
plot(pred_dlnm_glmm_ra1_temp,
     "slices",
     var = max(pred_dlnm_glmm_ra1_temp$predvar),
     col = 2,
     ylab = "RR")

#' Overall cumulative effect plot for mean temperature
plot(pred_dlnm_glmm_ra1_temp,
     "overall",
     xlab = "Mean temperature",
     ylab = "RR")
### 10.2.2 Mean Precipitation ----

#' 3D lag-response plot for mean precipitation
plot(
  pred_dlnm_glmm_ra1_prec,
  xlab = "Mean precipitation",
  zlab = "RR",
  theta = 330,
  phi = 10,
  lphi = 100
)

#' Contour lag-response plot for mean precipitation
plot(
  pred_dlnm_glmm_ra1_prec,
  "contour",
  xlab = "Mean precipitation",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response plot for a specific precipitation value (var = 20 mm)
plot(
  pred_dlnm_glmm_ra1_prec,
  "slices",
  var = 20,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific precipitation value (var = 60 mm)
plot(
  pred_dlnm_glmm_ra1_prec,
  "slices",
  var = 60,
  col = 2,
  ylab = "RR"
)

#' Overall cumulative effect plot for mean precipitation
plot(
  pred_dlnm_glmm_ra1_prec,
  "overall",
  xlab = "Mean precipitation",
  ylab = "RR"
)

### 10.2.3 Mean Relative Humidity ----

#' 3D lag-response plot for mean relative humidity
plot(
  pred_dlnm_glmm_ra1_rh,
  xlab = "Relative humidity",
  zlab = "RR",
  theta = 300,
  phi = 10,
  lphi = 100
)

#' Contour lag-response plot for relative humidity
plot(
  pred_dlnm_glmm_ra1_rh,
  "contour",
  xlab = "Relative humidity",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response plot for a specific humidity value (var = 0.24)
plot(
  pred_dlnm_glmm_ra1_rh,
  "slices",
  var = 0.24,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific humidity value (var = 0.43)
plot(
  pred_dlnm_glmm_ra1_rh,
  "slices",
  var = 0.43,
  col = 2,
  ylab = "RR"
)

#' Overall cumulative effect plot for relative humidity
plot(
  pred_dlnm_glmm_ra1_rh,
  "overall",
  xlab = "Mean relative humidity",
  ylab = "RR"
)
### 10.2.4 Mean AOD Index ----

#' 3D lag-response plot for mean AOD index
plot(
  pred_dlnm_glmm_ra1_aod,
  xlab = "Mean AOD index",
  zlab = "RR",
  theta = 300,
  phi = 10,
  lphi = 100
)

#' Contour lag-response plot for AOD
plot(
  pred_dlnm_glmm_ra1_aod,
  "contour",
  xlab = "Mean AOD index",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response plot for a specific AOD value (var = 1e-05)
plot(
  pred_dlnm_glmm_ra1_aod,
  "slices",
  var = 1e-05,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific AOD value (var = 4e-04)
plot(
  pred_dlnm_glmm_ra1_aod,
  "slices",
  var = 4e-04,
  col = 2,
  ylab = "RR"
)

#' Overall cumulative effect plot for AOD
plot(
  pred_dlnm_glmm_ra1_aod,
  "overall",
  xlab = "Mean AOD index",
  ylab = "RR"
)

### Subregion RA2

#' Generate predictions for temperature using crosspred (DLNM-GLMM, RA2)
pred_dlnm_glmm_ra2_temp <- crosspred(
  cb_t2,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA2") %>%
    pull(Temp_media) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for precipitation using crosspred (DLNM-GLMM, RA2)
pred_dlnm_glmm_ra2_prec <- crosspred(
  cb_p2,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA2") %>%
    pull(precip_media) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for relative humidity using crosspred (DLNM-GLMM, RA2)
pred_dlnm_glmm_ra2_rh <- crosspred(
  cb_rh2,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA2") %>%
    pull(RH) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for AOD using crosspred (DLNM-GLMM, RA2)
pred_dlnm_glmm_ra2_aod <- crosspred(
  cb_aod2,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA2") %>%
    pull(aerosol) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)
#' Subregion RA3

#' Generate predictions for temperature using crosspred (DLNM-GLMM, RA3)
pred_dlnm_glmm_ra3_temp <- crosspred(
  cb_t3,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA3") %>%
    pull(Temp_media) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for precipitation using crosspred (DLNM-GLMM, RA3)
pred_dlnm_glmm_ra3_prec <- crosspred(
  cb_p3,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA3") %>%
    pull(precip_media) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for relative humidity using crosspred (DLNM-GLMM, RA3)
pred_dlnm_glmm_ra3_rh <- crosspred(
  cb_rh3,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA3") %>%
    pull(RH) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for AOD using crosspred (DLNM-GLMM, RA3)
pred_dlnm_glmm_ra3_aod <- crosspred(
  cb_aod3,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA3") %>%
    pull(aerosol) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)
#' Subregion RA4

#' Generate predictions for temperature using crosspred (DLNM-GLMM, RA4)
pred_dlnm_glmm_ra4_temp <- crosspred(
  cb_t4,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA4") %>%
    pull(Temp_media) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for precipitation using crosspred (DLNM-GLMM, RA4)
pred_dlnm_glmm_ra4_prec <- crosspred(
  cb_p4,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA4") %>%
    pull(precip_media) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for relative humidity using crosspred (DLNM-GLMM, RA4)
pred_dlnm_glmm_ra4_rh <- crosspred(
  cb_rh4,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA4") %>%
    pull(RH) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for AOD using crosspred (DLNM-GLMM, RA4)
pred_dlnm_glmm_ra4_aod <- crosspred(
  cb_aod4,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA4") %>%
    pull(aerosol) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Subregion RA5

#' Generate predictions for temperature using crosspred (DLNM-GLMM, RA5)
pred_dlnm_glmm_ra5_temp <- crosspred(
  cb_t5,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA5") %>%
    pull(Temp_media) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for precipitation using crosspred (DLNM-GLMM, RA5)
pred_dlnm_glmm_ra5_prec <- crosspred(
  cb_p5,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA5") %>%
    pull(precip_media) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for relative humidity using crosspred (DLNM-GLMM, RA5)
pred_dlnm_glmm_ra5_rh <- crosspred(
  cb_rh5,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA5") %>%
    pull(RH) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for AOD using crosspred (DLNM-GLMM, RA5)
pred_dlnm_glmm_ra5_aod <- crosspred(
  cb_aod5,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA5") %>%
    pull(aerosol) %>%
    mean(na.rm = TRUE),
  bylag = 0.1
)

## 11. Forecasts ----

## 11.1 Forecast for 2017 ----

# year to forecast
anio_pronostico <- 2017

# training set: excludes the forecast year
df_train <- df_con_crossbasis_egresos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# test set: only the forecast year
df_test <- df_con_crossbasis_egresos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# set common levels for year in train and test sets
niveles_year <- sort(unique(df_con_crossbasis_egresos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# full GAMLSS formula with random effects by subregion
formula_dlnm_gamlss <- egreso_semana ~
  T_v1.l1 + T_v1.l2 + T_v1.l3 + T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 +
  P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + P_v3.l1 + P_v3.l2 + P_v3.l3 +
  RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 +
  AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +
  offset(logpop) + as.factor(epi.week)  + random(Subreg_climat)

# fit GAMLSS model
modelo_gamlss_nb <- gamlss(
  formula = formula_dlnm_gamlss,
  data = df_train,
  family = ZIP()
)

# prediction and forecasting with GAMLSS
df_train <- df_train %>%
  mutate(ajust_gamlss = predict(modelo_gamlss_nb, type = "response"))

df_test <- df_test %>%
  mutate(pred_gamlss = predict(modelo_gamlss_nb, newdata = df_test, type = "response"))

# fit GLMM model (glmmTMB)
modelo_glmm_nb <- glmmTMB(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol +
    offset(logpop) + as.factor(epi.week),
  data = df_train,
  family = nbinom2(link = "log"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# prediction and forecasting with GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_nb, type = "response"))

df_test_2017 <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_nb, newdata = df_test, type = "response"))

### Error metrics ----
# function to calculate error metrics
calcular_errores <- function(obs, pred, modelo) {
  tibble(
    modelo = modelo,
    MAE = mae(obs, pred),
    MSE = mse(obs, pred),
    RMSE = rmse(obs, pred),
    SAMPE = mean(2 * abs(pred - obs) / (abs(obs) + abs(pred))) * 100
  )
}

# summary of errors
errores_2017 <- bind_rows(
  calcular_errores(df_train$egreso_semana, df_train$ajust_gamlss, "GAMLSS_train"),
  calcular_errores(df_train$egreso_semana, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test_2017$egreso_semana, df_test_2017$pred_gamlss, paste0("GAMLSS_", anio_pronostico)),
  calcular_errores(df_test_2017$egreso_semana, df_test_2017$pred_glmm, paste0("GLMM_", anio_pronostico))
)
## 11.2 Forecast for 2018 ----

# year to forecast
anio_pronostico <- 2018

# training set: excludes the forecast year
df_train <- df_con_crossbasis_egresos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# test set: only the forecast year
df_test <- df_con_crossbasis_egresos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# set common levels for year in train and test sets
niveles_year <- sort(unique(df_con_crossbasis_egresos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# full GAMLSS formula with random effects by subregion
formula_dlnm_gamlss <- egreso_semana ~
  T_v1.l1 + T_v1.l2 + T_v1.l3 + T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 +
  P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + P_v3.l1 + P_v3.l2 + P_v3.l3 +
  RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 +
  AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +
  offset(logpop) + as.factor(epi.week)  + random(Subreg_climat)

# fit GAMLSS model
modelo_gamlss_nb <- gamlss(
  formula = formula_dlnm_gamlss,
  data = df_train,
  family = ZIP()
)

# prediction and forecasting with GAMLSS
df_train <- df_train %>%
  mutate(ajust_gamlss = predict(modelo_gamlss_nb, type = "response"))

df_test <- df_test %>%
  mutate(pred_gamlss = predict(modelo_gamlss_nb, newdata = df_test, type = "response"))

# fit GLMM model (glmmTMB)
modelo_glmm_nb <- glmmTMB(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol +
    offset(logpop) + as.factor(epi.week),
  data = df_train,
  family = nbinom2(link = "log"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# prediction and forecasting with GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_nb, type = "response"))

df_test_2018 <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_nb, newdata = df_test, type = "response"))

### Error metrics ----
# function to calculate error metrics
calcular_errores <- function(obs, pred, modelo) {
  tibble(
    modelo = modelo,
    MAE = mae(obs, pred),
    MSE = mse(obs, pred),
    RMSE = rmse(obs, pred),
    SAMPE = mean(2 * abs(pred - obs) / (abs(obs) + abs(pred))) * 100
  )
}

# Summary of errors
errores_2018 <- bind_rows(
  calcular_errores(df_train$egreso_semana, df_train$ajust_gamlss, "GAMLSS_train"),
  calcular_errores(df_train$egreso_semana, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test_2018$egreso_semana, df_test_2018$pred_gamlss, paste0("GAMLSS_", anio_pronostico)),
  calcular_errores(df_test_2018$egreso_semana, df_test_2018$pred_glmm, paste0("GLMM_", anio_pronostico))
)
## 11.3 Forecast for 2019 ----

# year to forecast
anio_pronostico <- 2019

# training set: excludes the forecast year
df_train <- df_con_crossbasis_egresos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# test set: only the forecast year
df_test <- df_con_crossbasis_egresos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# set common levels for year in train and test sets
niveles_year <- sort(unique(df_con_crossbasis_egresos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# full GAMLSS formula with random effects by subregion
formula_dlnm_gamlss <- egreso_semana ~
  T_v1.l1 + T_v1.l2 + T_v1.l3 + T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 +
  P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + P_v3.l1 + P_v3.l2 + P_v3.l3 +
  RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 +
  AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +
  offset(logpop) + as.factor(epi.week)  + random(Subreg_climat)

# fit GAMLSS model
modelo_gamlss_nb <- gamlss(
  formula = formula_dlnm_gamlss,
  data = df_train,
  family = ZIP()
)

# prediction and forecasting with GAMLSS
df_train <- df_train %>%
  mutate(ajust_gamlss = predict(modelo_gamlss_nb, type = "response"))

df_test <- df_test %>%
  mutate(pred_gamlss = predict(modelo_gamlss_nb, newdata = df_test, type = "response"))

# fit GLMM model (glmmTMB)
modelo_glmm_nb <- glmmTMB(
  egreso_semana ~ Temp_media + precip_media + RH + aerosol +
    offset(logpop) + as.factor(epi.week),
  data = df_train,
  family = nbinom2(link = "log"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# prediction and forecasting with GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_nb, type = "response"))

df_test_2019 <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_nb, newdata = df_test, type = "response"))

### Error metrics ----
# function to calculate error metrics
calcular_errores <- function(obs, pred, modelo) {
  tibble(
    modelo = modelo,
    MAE = mae(obs, pred),
    MSE = mse(obs, pred),
    RMSE = rmse(obs, pred),
    SAMPE = mean(2 * abs(pred - obs) / (abs(obs) + abs(pred))) * 100
  )
}

# Summary of errors
errores_2019 <- bind_rows(
  calcular_errores(df_train$egreso_semana, df_train$ajust_gamlss, "GAMLSS_train"),
  calcular_errores(df_train$egreso_semana, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test_2019$egreso_semana, df_test_2019$pred_gamlss, paste0("GAMLSS_", anio_pronostico)),
  calcular_errores(df_test_2019$egreso_semana, df_test_2019$pred_glmm, paste0("GLMM_", anio_pronostico))
)

# print results
errores_totales <- bind_rows(errores_2017,
                             errores_2018,
                             errores_2019)

datatable(errores_totales, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(errores_totales),
    color = "black",
    backgroundColor = "lightgray"
  )
