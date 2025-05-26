# 1. Libraries  --------------------------------------------------------------

## 1.1 Data manipulation and transformation ----

library(dplyr) # Data manipulation
library(tidyr) # Data transformation
library(tidyverse) # Collection of packages for data analysis

## 1.2 Visualization and plotting packages ----

library(ggplot2) # ggplot-style plots
library(ggseas) # Visualization of seasonal components
library(scales) # Scale customization in plots
library(viridis) # Perceptually uniform color palettes
library(corrplot) # Correlation plots
library(GGally) # Enhancements for ggplot2 (plot matrices)

## 1.3 Packages for statistical analysis and modeling ----

library(car) # Analysis and diagnostics of linear models
library(dlnm) # Non-linear modeling in time series
library(forecast) # Time series forecasting
library(glmmTMB) # Generalized nonlinear mixed models
library(lme4) # Mixed-effects models
library(mgcv) # Fitting GAM models
library(gamlss) # Fitting GAMLSS models for zero-inflated data
library(MASS) # Adjustments for GLM models
library(splines) # Generation of cubic splines
library(urca) # Unit root tests for time series
library(tseries) # Time series analysis

## 1.4 Packages for information analysis and metrics ----

library(energy) # Distance correlation (dCor)
library(infotheo) # Mutual Information (MI)
library(Metrics) # Model metrics

## 1.5 Packages for spatial data ----

library(sf) # Handling spatial data

## 1.6 Packages for table creation ----

library(kableExtra) # Table creation
library(DT) # Interactive tables


# 2. Data loading ----------------------------------------------------------



## 2.1 General database ----
load(file = "datos_final1_para_analizar.RData")

## 2.2 Spatial data ----
subregion_st <- st_read("Subegiones Climáticas.shp")


# 3. Data generation by region and subregion ---------------------------


# This code block generates summary data by region and subregion,
# calculating key weekly statistics for each combination of year,
# epidemiological week, and subregion.
# The data includes summarized values of discharges, relative risk of contagion,
# population, precipitation, temperature, aerosol concentration, and relative humidity.


## 3.1 Data by subregion and epidemiological week ----
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

# A column is added to identify each region and all the data is combined into a single dataset.

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
# 4. Correlational analysis using permutation tests ---------------

#' This block defines a function for assessing correlation between lagged predictor variables
#' and the response variable in the RMS region using different methods:
#' - Pearson correlation (linear)
#' - Distance correlation (dCor) (non-linear)
#' - Mutual Information (MI) (non-linear)
#' Statistical significance is assessed via permutation testing.


## 4.1 Functions to define crossbasis parameterization ----

#' ------------------------------------------------------------------------------
#' Function: perm_test
#' ------------------------------------------------------------------------------
#' Description:
#' Conducts a permutation test to assess statistical dependence between two numeric vectors.
#' Supports three methods: Pearson correlation, distance correlation (dCor), and mutual information (MI).
#'
#' Arguments:
#' - x: numeric vector (independent variable).
#' - y: numeric vector (dependent variable).
#' - method: character string specifying the dependency method: "pearson" (default), "dcor", or "MI".
#' - n_perm: integer, number of permutations to perform (default is 50).
#'
#' Value:
#' - A numeric value between 0 and 1 representing the p-value from the permutation test.
#'
#' Details:
#' - For "pearson", computes |cor(x, y)| with random permutations of x.
#' - For "dcor", uses the dcor() function from the energy package.
#' - For "MI", computes mutual information between discretized variables using mutinformation() from infotheo.

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
#' Function to identify the optimal model among combinations of distributed
#' lag non-linear model (DLNM) structures by evaluating different exposure
#' and lag function types using a Normal model. The optimal model is selected
#' based on performance and prediction metrics.
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
#' - A list with:
#'   - mejor_modelo: data.frame with the optimal model row
#'   - resultado_general: data.frame with all evaluated combinations
#' ------------------------------------------------------------------------------

buscar_mejor_modelo <- function(data, variable, respuesta, max_lag, max_df_var, max_df_lag) {
  resultados <- data.frame(
    modelo = character(), lag = integer(), df_var = character(), df_lag = character(), 
    AIC = numeric(), BIC = numeric(), MAE = numeric(), MSE = numeric(), RMSE = numeric(), 
    MAPE = numeric(), logLik = numeric(), score = numeric()
  )
  
  evaluar_modelo <- function(modelo, data_filtrada, respuesta, lag, df_var, df_lag) {
    predicciones <- predict(modelo, type = "response")
    obs <- data_filtrada[[respuesta]]
    
    # Fit metrics
    MAE    <- mean(abs(obs - predicciones))
    MSE    <- mean((obs - predicciones)^2)
    RMSE   <- sqrt(MSE)
    AIC_val <- AIC(modelo)
    BIC_val <- BIC(modelo)
    
    # Calculate MAPE (avoid division by zero)
    if (any(obs == 0)) {
      MAPE_val <- mean(abs((obs + 1e-6 - predicciones) / (obs + 1e-6))) * 100
    } else {
      MAPE_val <- mean(abs((obs - predicciones) / obs)) * 100
    }
    
    # LogLikelihood
    ll <- as.numeric(logLik(modelo))
    
    # Condition for the logLik term in the score
    if (ll < 0) {
      logLik_term <- 1 / abs(ll)
    } else {
      logLik_term <- ll
    }
    
    # Combined score
    score <- (1/AIC_val) + (1/BIC_val) + (1/MAE) + (1/MSE) + (1/RMSE) +
      (1/MAPE_val) + logLik_term
    
    data.frame(
      modelo = "Normal", lag = lag, df_var = as.character(df_var), df_lag = as.character(df_lag), 
      AIC = AIC_val, BIC = BIC_val, MAE = MAE, MSE = MSE, RMSE = RMSE, 
      MAPE = MAPE_val, logLik = ll, score = score
    )
  }
  
  # Evaluate combinations of lags and degrees of freedom
  for (lag in 2:max_lag) {
    for (df_var in 2:max_df_var) {
      for (df_lag in 2:max_df_lag) {
        # Create crossbasis with natural cubic splines
        cb_temp <- crossbasis(data[[variable]], lag = lag, 
                              argvar = list(fun = "ns", df = df_var), 
                              arglag = list(fun = "ns", df = df_lag))
        
        # Remove NAs generated by lags
        n_lags <- max(attr(cb_temp, "lag"))
        data_filtrada <- data[(n_lags + 1):nrow(data), ]
        cb_temp_filtrado <- cb_temp[(n_lags + 1):nrow(cb_temp), , drop = FALSE]
        
        # Fit Normal model
        try({
          modelo_norm <- glm(data_filtrada[[respuesta]] ~ cb_temp_filtrado, 
                             family = gaussian(link = "identity"), data = data_filtrada)
          
          resultado <- evaluar_modelo(modelo_norm, data_filtrada, respuesta, lag, df_var, df_lag)
          if (!is.null(resultado)) {
            resultados <- rbind(resultados, resultado)
          }
        }, silent = TRUE)
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

## 4.2 Correlational analysis in the RMS1 subregion ----

### 4.2.1 Mean temperature ----

#' Initialize results data frame
results_temp <- tibble(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

max_lag <- 14
#' Compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_lag <- data_rms_subreg %>%
    filter(Subreg_climat == "RMS1")
  
  X_lagged <- head(data_lag$Temp_media, length(data_lag$Temp_media) - lag)
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
  #' Correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' Store results
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


#' Compute extreme values for each metric
max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)


#' Visualization of correlations by lag
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
  # Annotate maximum and minimum values
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


#' Evaluation of univariate DLNM models for temperature in RMS1 subregion

data_rms1 <- data_rms_subreg %>%
  filter(Subreg_climat == "RMS1")

res <- buscar_mejor_modelo(
  data = data_rms1,
  variable = "Temp_media",
  respuesta = "Riesgo_relativ_contagio",
  max_lag = 13,
  max_df_var = 3,
  max_df_lag = 3
)

#' Visualize results in an interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.2.2 Mean precipitation ----

#' Initialize data.frame to store results by lag

results_prec <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)

#' Compute correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_rms1 <- data_rms_subreg %>% filter(Subreg_climat == "RMS1")
  
  X_lagged <- head(data_rms1$precip_media, n = length(data_rms1$precip_media) - lag)
  Y_lagged <- tail(data_rms1$Riesgo_relativ_contagio,
                   n = length(data_rms1$Riesgo_relativ_contagio) - lag)
  
  #' Compute metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' Store results
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

#' Identify extreme values for annotation

max_pearson <- max(results_prec$Pearson)
min_pearson <- min(results_prec$Pearson)
max_dcor <- max(results_prec$dCor)
min_dcor <- min(results_prec$dCor)
max_mi <- max(results_prec$MI)
min_mi <- min(results_prec$MI)

#' Correlation plot by lag

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

#' Evaluation of univariate DLNM models for precipitation in RMS1 subregion

res <- buscar_mejor_modelo(
  data_rms_subreg %>% filter(Subreg_climat == "RMS1"), # Filter RMS1 subregion
  variable = "precip_media", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 14, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for lags
)

#' Visualize all evaluated models ordered by combined score

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

#' Initialize the dataframe to store results for each lag

results_rh <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)

#' Compute correlations for each lag from 1 to `max_lag`
for (lag in 1:max_lag) {
  df_filtered <- data_rms_subreg %>% filter(Subreg_climat == "RMS1")
  
  X_lagged <- head(df_filtered$RH, length(df_filtered$RH) - lag)
  Y_lagged <- tail(df_filtered$Riesgo_relativ_contagio,
                   length(df_filtered$Riesgo_relativ_contagio) - lag)
  
  #' Correlations and p-values
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' Store results
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

#' Compute extremes for annotations in the plot

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' Plot correlations by lag

ggplot(results_rh, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    x = "Relative Humidity Lag (k)",
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


#' Evaluation of univariate DLNM models for relative humidity in RMS1 subregion

res <- buscar_mejor_modelo(
  data_rms_subreg %>% filter(Subreg_climat == "RMS1"), # Filter RMS1 subregion
  variable = "RH", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Visualize all evaluated models ordered by combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general), # Apply style to all columns
    color = "black", # Text color
    backgroundColor = "lightgray" # Light gray background
  )
### 4.2.4 AOD Index ----

#' Initialize the dataframe to store results by lag

results_aod <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' Compute correlations for each lag

for (lag in 1:max_lag) {
  datos <- data_rms_subreg %>% filter(Subreg_climat == "RMS1")
  
  X_lagged <- head(datos$aerosol, length(datos$aerosol) - lag)
  Y_lagged <- tail(datos$Riesgo_relativ_contagio,
                   length(datos$Riesgo_relativ_contagio) - lag)
  
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

#' Plot correlation coefficients for each method
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

#' Evaluation of univariate DLNM models for AOD in RMS1 subregion
res <- buscar_mejor_modelo(
  data_rms_subreg %>% filter(Subreg_climat == "RMS1"), # Filter RMS1 subregion
  variable = "aerosol", # Explanatory variable (AOD)
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Visualize all evaluated models ordered by the combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )

## 4.3 Correlational analysis in the RMS2 subregion ----

### 4.3.1 Mean temperature ----

#' Initialize results data frame
results_temp <- tibble(
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
  data_lag <- data_rms_subreg %>%
    filter(Subreg_climat == "RMS2")
  
  X_lagged <- head(data_lag$Temp_media, length(data_lag$Temp_media) - lag)
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
  # Correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  # Store results
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

#' Compute extreme values per metric
max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' Visualization of correlations by lag
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

#' Selection of the best DLNM model for mean temperature in the RMS2 subregion

#' Filter RMS2 climatic subregion and fit DLNM models
res <- buscar_mejor_modelo(
  data = data_rms_subreg %>% filter(Subreg_climat == "RMS2"), # Filter RMS2 subregion
  variable = "Temp_media", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for lags
)

#' Visualize all evaluated models ordered by combined score
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

#' Initialize results data frame

results_prec <- tibble(
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
  data_lag <- data_rms_subreg %>%
    filter(Subreg_climat == "RMS2")
  
  X_lagged <- head(data_lag$precip_media, length(data_lag$precip_media) - lag)
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
  #' Correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' Store results
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

#' Compute extreme values for each metric

max_pearson <- max(results_prec$Pearson)
min_pearson <- min(results_prec$Pearson)
max_dcor <- max(results_prec$dCor)
min_dcor <- min(results_prec$dCor)
max_mi <- max(results_prec$MI)
min_mi <- min(results_prec$MI)

#' Visualization of correlations by lag

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
  #' Annotate maximum and minimum values
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

#' Evaluation of univariate DLNM models for precipitation in RMS2 subregion
res <- buscar_mejor_modelo(
  data = data_rms_subreg %>% filter(Subreg_climat == "RMS2"), # Filter RMS2 subregion
  variable = "precip_media", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Visualize all evaluated models ordered by the combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.3.3 Mean relative humidity -----

#' Initialize results data frame

results_rh <- tibble(
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
  data_lag <- data_rms_subreg %>%
    filter(Subreg_climat == "RMS2")
  
  X_lagged <- head(data_lag$RH, length(data_lag$RH) - lag)
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
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

#' Compute maxima and minima for annotations in the plot

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' Visualization with ggplot2
ggplot(results_rh, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Relative humidity lag (k)", y = "Coefficient") +
  scale_x_continuous(breaks = seq(1, max_lag, by = 1)) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (non-linear)" = "red",
    "MI (non-linear)" = "green"
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

#' Fit DLNM models with different combinations of functions and degrees of freedom

res <- buscar_mejor_modelo(
  data = data_rms_subreg %>% filter(Subreg_climat == "RMS2"), # Filter RMS2 subregion
  variable = "RH", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 14, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Visualize all evaluated models ordered by the combined score
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

#' Initialize results data frame
results_aod <- tibble(
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
  data_lag <- data_rms_subreg %>% filter(Subreg_climat == "RMS2")
  
  X_lagged <- head(data_lag$aerosol, length(data_lag$aerosol) - lag)
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
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

#' Compute extreme values for each metric

max_pearson <- max(results_aod$Pearson)
min_pearson <- min(results_aod$Pearson)
max_dcor <- max(results_aod$dCor)
min_dcor <- min(results_aod$dCor)
max_mi <- max(results_aod$MI)
min_mi <- min(results_aod$MI)

#' Visualization of correlations by lag

ggplot(results_aod, aes(x = Lag)) +
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
  labs(x = "AOD index lag (k)", y = "Coefficient") +
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

#' Fit DLNM models with different combinations of functions and degrees of freedom

res <- buscar_mejor_modelo(
  data = data_rms_subreg %>% filter(Subreg_climat == "RMS2"), # Filter RMS2 subregion
  variable = "aerosol", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 13, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Visualize all evaluated models ordered by the combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )

## 4.4 Function for combined correlation analysis -----

#' ------------------------------------------------------------------------------
#' Function: continuous_modeling_parameters()
#' ------------------------------------------------------------------------------
#' Evaluates multiple combinations of non-linear base functions for climatic
#' variables (temperature, precipitation, relative humidity, and AOD), using
#' normal models with distributed lags. This function was implemented at the 
#' subregional level using all exposure variables to adequately 
#' parameterize the non-linear base functions.
#' 
#' Inputs:
#' - data: full dataset.
#' - respuesta: name of the response variable (count).
#' - temp_var, prec_var, rh_var, aod_var: names of the predictor variables.
#' - max_lag_*: maximum lag for each predictor.
#' - max_df_var_*: maximum df for the variable function.
#' - max_df_lag_*: maximum df for the lag function.
#'
#' Output:
#' - A list containing a data.frame named 'resultados_completos' with
#'   the fit metrics (AIC, BIC, MAE, MSE, RMSE, R2, MAPE, logLik, score)
#'   for each combination of evaluated parameters.
#'
#' Details:
#' - The fitted model is of type glm
#' - Seasonal effects are included through as.factor(epi.week).
#' - Cross-basis functions are generated using the crossbasis function (dlnm package).
#' ------------------------------------------------------------------------------
continuous_modeling_parameters <- function(
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
      logLik_term <- 1 / abs(ll)
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
# 5. Construction of cross-basis functions by climatic subregion ----

#' This block defines the cross-basis functions required for
#' DLNM-GLMM modeling of hospital discharge risk in subregions
#' RMS1, RMS2, and RMS3, based on climatic and air pollution variables.

#' Sort dataset by climatic subregion

data_rms_subreg <- data_rms_subreg %>%
  arrange(Subreg_climat)

## 5.1 Cross-basis for RMS1 subregion ----

cb_t1 <- crossbasis(data_rms_subreg %>% filter(Subreg_climat == "RMS1") %>% pull(Temp_media),
                    lag = 13, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p1 <- crossbasis(data_rms_subreg %>% filter(Subreg_climat == "RMS1") %>% pull(precip_media),
                    lag = 3, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh1 <- crossbasis(data_rms_subreg %>% filter(Subreg_climat == "RMS1") %>% pull(RH),
                     lag = 4, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod1 <- crossbasis(data_rms_subreg %>% filter(Subreg_climat == "RMS1") %>% pull(aerosol),
                      lag = 8, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.3 Cross-basis for RMS2 subregion ----

cb_t2 <- crossbasis(data_rms_subreg %>% filter(Subreg_climat == "RMS2") %>% pull(Temp_media),
                    lag = 12, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p2 <- crossbasis(data_rms_subreg %>% filter(Subreg_climat == "RMS2") %>% pull(precip_media),
                    lag = 11, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh2 <- crossbasis(data_rms_subreg %>% filter(Subreg_climat == "RMS2") %>% pull(RH),
                     lag = 14, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod2 <- crossbasis(data_rms_subreg %>% filter(Subreg_climat == "RMS2") %>% pull(aerosol),
                      lag = 13, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.4 Combine cross-basis matrices ----

#' Combine data with cross-basis variables

df_con_crossbasis_riesgos <- cbind(
  data_rms_subreg,
  
  #' Temperature
  setNames(
    as.data.frame(rbind(cb_t1, cb_t2)),
    make.names(paste0("T_", colnames(cb_t1)), unique = TRUE)
  ),
  
  #' Precipitation
  setNames(
    as.data.frame(rbind(cb_p1, cb_p2)),
    make.names(paste0("P_", colnames(cb_p1)), unique = TRUE)
  ),
  
  #' Relative humidity
  setNames(
    as.data.frame(rbind(cb_rh1, cb_rh2)),
    make.names(paste0("RH_", colnames(cb_rh1)), unique = TRUE)
  ),
  
  #' Aerosol index (AOD)
  setNames(
    as.data.frame(rbind(cb_aod1, cb_aod2)),
    make.names(paste0("AOD_", colnames(cb_aod1)), unique = TRUE)
  )
)

#' Remove initial observations by group according to maximum lag (lag = 14)

df_con_crossbasis_riesgos <- df_con_crossbasis_riesgos %>%
  group_by(Subreg_climat) %>%
  slice(-(1:14)) %>%
  ungroup()

#' Retrieve names of columns generated by crossbasis

columnas_exposicion_riesgos <- colnames(df_con_crossbasis_riesgos)[12:ncol(df_con_crossbasis_riesgos)]
columnas_exposicion_riesgos

df_con_crossbasis_riesgos$Subreg_climat <- as.factor(df_con_crossbasis_riesgos$Subreg_climat)

## Zero-inflation need for Gamma distribution models ----

#' Percentage of zeros in all subregions
summary(data_rms_subreg$Riesgo_relativ_contagio)

100 * sum(data_rms_subreg$Riesgo_relativ_contagio == 0) / (length(data_rms_subreg$Riesgo_relativ_contagio))

#' Percentage of zeros in RMS1 subregion
riesgo_rms1 <- data_rms_subreg %>%
  filter(Subreg_climat == "RMS1") %>%
  pull(Riesgo_relativ_contagio) 

100 * sum(riesgo_rms1 == 0, na.rm = TRUE) / (length(riesgo_rms1))

#' Percentage of zeros in RMS2 subregion
riesgo_rms2 <- data_rms_subreg %>%
  filter(Subreg_climat == "RMS2") %>%
  pull(Riesgo_relativ_contagio) 

100 * sum(riesgo_rms2 == 0, na.rm = TRUE) / (length(riesgo_rms2))
# 6. DLNM-GLMM and DLNM-GAMLSS Models ----

## 6.1 GLMM Normal Model (1): simple ----

modelo_glmm_normal_rms <- glmmTMB(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol 
  + as.factor(epi.week),
  data = df_con_crossbasis_riesgos ,
  family = gaussian(link = "identity"))

## 6.2 DLNM + GLMM Model (2): with normal distribution ----

modelo_dlnm_glmm_normal_rms <- glmmTMB(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +  
    as.factor(epi.week) + (1 | Subreg_climat), 
  data = df_con_crossbasis_riesgos,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

summary(modelo_dlnm_glmm_normal_rms)

## 6.3 DLNM + GAMLSS Model (1): zero-inflated inverse Gaussian distribution ----

# this model does not converge 
# modelo_dlnm_gamlss_inv_gauss_rms <- gamlss(...)

## 6.4 DLNM + GAMLSS Model (2): zero-inflated gamma distribution ----
# Model does not converge, low zero presence and highly homogeneous regions

# modelo_dlnm_gamlss_gamma_rms <- gamlss(...)

# 7. Optimal model selection ----

#' This block applies the `calcular_metricas()` function to the fitted models
#' (GLMM and GAMLSS, with different distributions), and builds a table
#' sorted according to the best fit score.

#' Append fitted values to dataset
df_con_crossbasis_riesgos_rms <- df_con_crossbasis_riesgos %>%
  mutate(
    #' GLMM Normal Model
    ajustado_modelo_glmm_normal_rms = fitted(modelo_glmm_normal_rms),
    
    #' DLNM + GLMM Normal Model
    ajustado_modelo_dlnm_glmm_normal_rms = fitted(modelo_dlnm_glmm_normal_rms))

## 7.1 Metrics for each model ----

#' ------------------------------------------------------------------------------
#' calcular_metricas()
#' ------------------------------------------------------------------------------
#' Function to compute performance metrics of a fitted model and its
#' associated predictions. Includes classic error metrics, information
#' criteria, and a composite score summarizing overall performance.
#'
#' Parameters:
#' - modelo: fitted model object
#' - predicciones: numeric vector of model predictions
#' - obs: numeric vector of observed values
#' - nombre_modelo: string identifier of the evaluated model
#'
#' Returns:
#' - Data frame with metrics MAE, MSE, RMSE, MAPE, AIC, BIC, logLik, and Score
#' ------------------------------------------------------------------------------

calcular_metricas <- function(modelo, predicciones, obs, nombre_modelo) {
  MAE <- mean(abs(obs - predicciones))
  MSE <- mean((obs - predicciones)^2)
  RMSE <- sqrt(MSE)
  
  epsilon <- 1e-10
  MAPE_val <- mean(abs((obs - predicciones) / (obs + epsilon))) * 100
  
  AIC_val <- tryCatch(AIC(modelo), error = function(e) NA)
  BIC_val <- tryCatch(BIC(modelo), error = function(e) NA)
  ll <- tryCatch(as.numeric(logLik(modelo)), error = function(e) NA)
  
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

resultados_modelos <- rbind(
  calcular_metricas(
    modelo_glmm_normal_rms,
    df_con_crossbasis_riesgos_rms$ajustado_modelo_glmm_normal_rms,
    df_con_crossbasis_riesgos_rms$Riesgo_relativ_contagio,
    "GLMM_Normal"
  ),
  calcular_metricas(
    modelo_dlnm_glmm_normal_rms,
    df_con_crossbasis_riesgos_rms$ajustado_modelo_dlnm_glmm_normal_rms,
    df_con_crossbasis_riesgos_rms$Riesgo_relativ_contagio,
    "DLNM_GLMM_Normal"
  ))

#' Sort models from best to worst according to score

resultados_modelos <- resultados_modelos %>%
  arrange(desc(Score))

#' Display results in an interactive table

datatable(resultados_modelos, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados_modelos),
    color = "black",
    backgroundColor = "lightgray"
  )
## 7.2 Best-performing model assignment -----

dlnm_glmm_model_rms <- modelo_dlnm_glmm_normal_rms

## 7.3 Metrics for filtered subregions ----

#' Construct time series by model for RMS1

df_rms1 <- df_con_crossbasis_riesgos_rms %>%
  filter(Subreg_climat == "RMS1") %>%
  mutate(
    riesgos_ts = ts(Riesgo_relativ_contagio, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_modelo_glmm_normal_rms, start = c(2000, 15), frequency = 52),
    dlnm_glmm_ts = ts(ajustado_modelo_dlnm_glmm_normal_rms, start = c(2000, 15), frequency = 52)
  )

#' Construct time series by model for RMS2

df_rms2 <- df_con_crossbasis_riesgos_rms %>%
  filter(Subreg_climat == "RMS2") %>%
  mutate(
    riesgos_ts = ts(Riesgo_relativ_contagio, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_modelo_glmm_normal_rms, start = c(2000, 15), frequency = 52),
    dlnm_glmm_ts = ts(ajustado_modelo_dlnm_glmm_normal_rms, start = c(2000, 15), frequency = 52)
  )

#' ------------------------------------------------------------------------------
#' Function: calcular_metricas_sub()
#' ------------------------------------------------------------------------------
#' Calculates error metrics for fitted models by subregion
#' Returns the top two predictions with the lowest composite score
#' ------------------------------------------------------------------------------

calcular_metricas_sub <- function(df, subregion, col_obs = "riesgos_ts") {
  
  #' Identify model columns (ending with _ts), excluding the observed variable
  columnas_modelo <- setdiff(
    names(df)[grepl("_ts$", names(df))],
    col_obs
  )
  
  obs <- df[[col_obs]]
  epsilon <- 1e-10  # Small value to avoid division by zero in MAPE
  
  #' Compute error metrics for each fitted model
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

#' Apply the function by subregion

resultados_modelos <- rbind(
  calcular_metricas_sub(df_rms1, "RMS1", "Riesgo_relativ_contagio"),
  calcular_metricas_sub(df_rms2, "RMS2", "Riesgo_relativ_contagio")
)

#' Sort models from best to worst according to the score

resultados_modelos <- resultados_modelos %>%
  arrange(desc(Score))

#' Display results in an interactive table

datatable(resultados_modelos, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados_modelos),
    color = "black",
    backgroundColor = "lightgray"
  )
# 8. Residuals of the optimal model ----

#' Pearson and deviance residuals
resid_pearson_rms <- residuals(
  dlnm_glmm_model_rms,
  type = "pearson"
)

resid_deviance_rms <- residuals(
  dlnm_glmm_model_rms,
  type = "deviance"
)

#' Append residuals to the dataset
df_con_crossbasis_riesgos_rms <- df_con_crossbasis_riesgos_rms %>%
  mutate(
    resid_pearson_rms = resid_pearson_rms,
    resid_deviance_rms = resid_deviance_rms
  )

## 8.1 Residual diagnostics ----

#' Q-Q plots to assess residual normality

qqnorm(resid_pearson_rms, main = "Pearson Residuals")
qqline(resid_pearson_rms, col = "red")

qqnorm(resid_deviance_rms, main = "Deviance Residuals")
qqline(resid_deviance_rms, col = "red")

shapiro_test_pearson <- shapiro.test(resid_pearson_rms)
shapiro_test_deviance <- shapiro.test(resid_deviance_rms)

# Display the test results
print(shapiro_test_pearson)
print(shapiro_test_deviance)

#' Extract Pearson residuals by climatic subregion

resid_pearson_rms1 <- df_con_crossbasis_riesgos_rms %>%
  filter(Subreg_climat == "RMS1") %>%
  pull(resid_pearson_rms)

resid_pearson_rms2 <- df_con_crossbasis_riesgos_rms %>%
  filter(Subreg_climat == "RMS2") %>%
  pull(resid_pearson_rms)

#' Extract Deviance residuals by climatic subregion

resid_deviance_rms1 <- df_con_crossbasis_riesgos_rms %>%
  filter(Subreg_climat == "RMS1") %>%
  pull(resid_deviance_rms)

resid_deviance_rms2 <- df_con_crossbasis_riesgos_rms %>%
  filter(Subreg_climat == "RMS2") %>%
  pull(resid_deviance_rms)

#' Time series diagnostic plots for Pearson residuals by subregion

ggtsdisplay(resid_pearson_rms1) # RMS1 subregion
ggtsdisplay(resid_pearson_rms2) # RMS2 subregion

#' Time series diagnostic plots for Deviance residuals by subregion

ggtsdisplay(resid_deviance_rms1) # RMS1 subregion
ggtsdisplay(resid_deviance_rms2) # RMS2 subregion

# 9. Fitted effects of the model ----

## 9.1 Comparative plots of model fits ----

#' Comparative plots of predicted values - RMS1 subregion
autoplot(df_rms1$riesgos_ts, series = "Observed") +
  autolayer(df_rms1$glmm_ts, series = "GLMM") +
  autolayer(df_rms1$dlnm_glmm_ts, series = "DLNM - GLMM") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharge risk"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM" = "blue",
    "DLNM - GLMM" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plots of predicted values - RMS2 subregion
autoplot(df_rms2$riesgos_ts, series = "Observed") +
  autolayer(df_rms2$glmm_ts, series = "GLMM") +
  autolayer(df_rms2$dlnm_glmm_ts, series = "DLNM - GLMM") +
  labs(
    x = "Time (weeks)",
    y = "Hospital discharge risk"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM" = "blue",
    "DLNM - GLMM" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

## 9.2 Predictions ----

coef_cond <- fixef(dlnm_glmm_model_rms)$cond
vcov_full <- vcov(dlnm_glmm_model_rms, full = TRUE)



# Save the models to a directory
save(
  modelo_glmm_normal_rms,
  modelo_dlnm_glmm_normal_rms,
  dlnm_glmm_model_rms,
  coef_cond,
  vcov_full,
  file = "random_effects_models/dlnm_models_for_risk_rms.RData"
)

# Load saved models if needed
# load("random_effects_models/dlnm_models_for_risk_rms.RData")

#' Extract coefficients and var-cov matrix for temperature (cb_t1 - RMS1 subregion)
coef_names_t <- grep("^T_", names(coef_cond), value = TRUE)
coef_t <- coef_cond[coef_names_t]
vcov_t <- as.matrix(vcov_full[coef_names_t, coef_names_t])

#' Extract coefficients and var-cov matrix for precipitation (cb_p1 - RMS1 subregion)
coef_names_p <- grep("^P_", names(coef_cond), value = TRUE)
coef_p <- coef_cond[coef_names_p]
vcov_p <- as.matrix(vcov_full[coef_names_p, coef_names_p])

#' Extract coefficients and var-cov matrix for relative humidity (cb_rh1 - RMS1 subregion)
coef_names_rh <- grep("^RH_", names(coef_cond), value = TRUE)
coef_rh <- coef_cond[coef_names_rh]
vcov_rh <- as.matrix(vcov_full[coef_names_rh, coef_names_rh])

#' Extract coefficients and var-cov matrix for aerosols (cb_aod1 - RMS1 subregion)
coef_names_aod <- grep("^AOD_", names(coef_cond), value = TRUE)
coef_aod <- coef_cond[coef_names_aod]
vcov_aod <- as.matrix(vcov_full[coef_names_aod, coef_names_aod])

#' RMS1 Subregion
#' Generate predictions for temperature using crosspred (DLNM-GLMM, RMS1 subregion)


pred_dlnm_glmm_rms1_temp <- crosspred(
  cb_t1,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_rms_subreg %>%
    filter(Subreg_climat == "RMS1") %>%
    pull(Temp_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for precipitation using crosspred (DLNM-GLMM, RMS1 subregion)
pred_dlnm_glmm_rms1_prec <- crosspred(
  cb_p1,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_rms_subreg %>%
    filter(Subreg_climat == "RMS1") %>%
    pull(precip_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for relative humidity using crosspred (DLNM-GLMM, RMS1 subregion)
pred_dlnm_glmm_rms1_rh <- crosspred(
  cb_rh1,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_rms_subreg %>%
    filter(Subreg_climat == "RMS1") %>%
    pull(RH) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Generate predictions for AOD using crosspred (DLNM-GLMM, RMS1 subregion)
pred_dlnm_glmm_rms1_aod <- crosspred(
  cb_aod1,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_rms_subreg %>%
    filter(Subreg_climat == "RMS1") %>%
    pull(aerosol) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)
### 9.2.1 Mean temperature ----

#' 3D lag-response plot for mean temperature
plot(
  pred_dlnm_glmm_rms1_temp,
  xlab = "Mean temperature",
  zlab = "RR",
  theta = 200,
  phi = 10,
  lphi = 100
)

#' Contour lag-response plot for mean temperature
plot(
  pred_dlnm_glmm_rms1_temp,
  "contour",
  xlab = "Mean temperature",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response plot for a specific temperature value (var = 27.4 °C)
plot(
  pred_dlnm_glmm_rms1_temp,
  "slices",
  var = 27.4,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific temperature value (var = 22.3 °C)
plot(
  pred_dlnm_glmm_rms1_temp,
  "slices",
  var = min(pred_dlnm_glmm_rms1_temp$predvar),
  col = 2,
  ylab = "RR"
)

#' Overall cumulative effect plot for temperature
plot(
  pred_dlnm_glmm_rms1_temp,
  "overall",
  xlab = "Mean temperature",
  ylab = "RR"
)

### 9.2.2 Mean precipitation ----

#' 3D lag-response plot for mean precipitation
plot(
  pred_dlnm_glmm_rms1_prec,
  xlab = "Mean precipitation",
  zlab = "RR",
  theta = 300,
  phi = 10,
  lphi = 100
)

#' Contour lag-response plot for mean precipitation
plot(
  pred_dlnm_glmm_rms1_prec,
  "contour",
  xlab = "Mean precipitation",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response plot for a specific precipitation value (var = 10 mm)
plot(
  pred_dlnm_glmm_rms1_prec,
  "slices",
  var = 10,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific precipitation value (var = 67 mm)
plot(
  pred_dlnm_glmm_rms1_prec,
  "slices",
  var = 67,
  col = 2,
  ylab = "RR"
)

#' Overall cumulative effect plot for precipitation
plot(
  pred_dlnm_glmm_rms1_prec,
  "overall",
  xlab = "Mean precipitation",
  ylab = "RR"
)

### 9.2.3 Mean relative humidity ----

#' 3D lag-response plot for relative humidity
plot(
  pred_dlnm_glmm_rms1_rh,
  xlab = "Relative humidity",
  zlab = "RR",
  theta = 300,
  phi = 10,
  lphi = 100
)

#' Contour lag-response plot for relative humidity
plot(
  pred_dlnm_glmm_rms1_rh,
  "contour",
  xlab = "Relative humidity",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response plot for a specific relative humidity value (var = 0.24)
plot(
  pred_dlnm_glmm_rms1_rh,
  "slices",
  var = 0.240,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific relative humidity value (var = 0.45)
plot(
  pred_dlnm_glmm_rms1_rh,
  "slices",
  var = max(pred_dlnm_glmm_rms1_rh$predvar),
  col = 2,
  ylab = "RR"
)

#' Overall cumulative effect plot for relative humidity
plot(
  pred_dlnm_glmm_rms1_rh,
  "overall",
  xlab = "Mean relative humidity",
  ylab = "RR"
)

### 9.2.4 Mean AOD index ----

#' 3D lag-response plot for AOD
plot(
  pred_dlnm_glmm_rms1_aod,
  xlab = "Mean AOD index",
  zlab = "RR",
  theta = 300,
  phi = 10,
  lphi = 100
)

#' Contour lag-response plot for AOD
plot(
  pred_dlnm_glmm_rms1_aod,
  "contour",
  xlab = "Mean AOD index",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response plot for a specific AOD value (var = 1e-05)
plot(
  pred_dlnm_glmm_rms1_aod,
  "slices",
  var = 0,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific AOD value (var = 4.8e-04)
plot(
  pred_dlnm_glmm_rms1_aod,
  "slices",
  var = max(pred_dlnm_glmm_rms1_aod$predvar),
  col = 2,
  ylab = "RR"
)

#' Overall cumulative effect plot for AOD
plot(
  pred_dlnm_glmm_rms1_aod,
  "overall",
  xlab = "Mean AOD index",
  ylab = "RR"
)

# RMS2 subregion

#' Create DLNM-GLMM predictions for temperature (RMS2)
pred_dlnm_glmm_rms2_temp <- crosspred(
  cb_t2,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_rms_subreg %>%
    filter(Subreg_climat == "RMS2") %>%
    pull(Temp_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create DLNM-GLMM predictions for precipitation (RMS2)
pred_dlnm_glmm_rms2_prec <- crosspred(
  cb_p2,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_rms_subreg %>%
    filter(Subreg_climat == "RMS2") %>%
    pull(precip_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create DLNM-GLMM predictions for relative humidity (RMS2)
pred_dlnm_glmm_rms2_rh <- crosspred(
  cb_rh2,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_rms_subreg %>%
    filter(Subreg_climat == "RMS2") %>%
    pull(RH) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create DLNM-GLMM predictions for AOD (RMS2)
pred_dlnm_glmm_rms2_aod <- crosspred(
  cb_aod2,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_rms_subreg %>%
    filter(Subreg_climat == "RMS2") %>%
    pull(aerosol) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)
# 10. Forecasting ----

## 10.1 Forecast for 2017 ----

# Forecast year
anio_pronostico <- 2017

# Training dataset: excludes the forecast year
df_train <- df_con_crossbasis_riesgos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# Testing dataset: only includes the forecast year
df_test <- df_con_crossbasis_riesgos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# Set common levels for 'year' in train and test
niveles_year <- sort(unique(df_con_crossbasis_riesgos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# Fit DLNM-GLMM Normal model
modelo_dlnm_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +  
    as.factor(epi.week) + (1 | Subreg_climat),
  data = df_train,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# Prediction and forecasting with DLNM-GLMM Normal
df_train <- df_train %>%
  mutate(adjust_dlnm_glmm = predict(modelo_dlnm_glmm_normal, type = "response"))

df_test <- df_test %>%
  mutate(pred_dlnm_glmm = predict(modelo_dlnm_glmm_normal, newdata = df_test, type = "response"))

# Fit GLMM model (glmmTMB)
modelo_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol 
  + as.factor(epi.week),
  data = df_train ,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# Prediction and forecasting with GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_normal, type = "response"))

df_test <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_normal, newdata = df_test, type = "response"))

### Error metrics ----

# Function to compute error metrics
calcular_errores <- function(obs, pred, modelo) {
  tibble(
    modelo = modelo,
    MAE = mae(obs, pred),
    MSE = mse(obs, pred),
    RMSE = rmse(obs, pred),
    SAMPE = mean(2 * abs(pred - obs) / (abs(obs) + abs(pred))) * 100
  )
}

# Summary of forecasting errors
errores_2017 <- bind_rows(
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$adjust_dlnm_glmm, "DLNM_GLMM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test$Riesgo_relativ_contagio, df_test$pred_dlnm_glmm, paste0("DLNM_GLMM_", anio_pronostico)),
  calcular_errores(df_test$Riesgo_relativ_contagio, df_test$pred_glmm, paste0("GLMM_", anio_pronostico))
)
## 10.2 Forecast for 2018 ----

# Forecast year
anio_pronostico <- 2018

# Training dataset: excludes the forecast year
df_train <- df_con_crossbasis_riesgos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# Testing dataset: includes only the forecast year
df_test <- df_con_crossbasis_riesgos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# Set common levels for 'year' in training and testing sets
niveles_year <- sort(unique(df_con_crossbasis_riesgos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# Fit DLNM-GLMM Normal model
modelo_dlnm_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +  
    as.factor(epi.week) + (1 | Subreg_climat),
  data = df_train,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# Predictions and forecasting with DLNM-GLMM Normal
df_train <- df_train %>%
  mutate(adjust_dlnm_glmm = predict(modelo_dlnm_glmm_normal, type = "response"))

df_test <- df_test %>%
  mutate(pred_dlnm_glmm = predict(modelo_dlnm_glmm_normal, newdata = df_test, type = "response"))

# Fit GLMM model (glmmTMB)
modelo_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol 
  + as.factor(epi.week),
  data = df_train,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# Predictions and forecasting with GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_normal, type = "response"))

df_test <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_normal, newdata = df_test, type = "response"))

### Error metrics ----

# Summary of prediction errors
errores_2018 <- bind_rows(
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$adjust_dlnm_glmm, "DLNM_GLMM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test$Riesgo_relativ_contagio, df_test$pred_dlnm_glmm, paste0("DLNM_GLMM_", anio_pronostico)),
  calcular_errores(df_test$Riesgo_relativ_contagio, df_test$pred_glmm, paste0("GLMM_", anio_pronostico))
)
## 10.3 Forecast for 2019 ----

# Forecast year
anio_pronostico <- 2019

# Training dataset: excludes the forecast year
df_train <- df_con_crossbasis_riesgos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# Testing dataset: includes only the forecast year
df_test <- df_con_crossbasis_riesgos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# Set common levels for 'year' in training and testing sets
niveles_year <- sort(unique(df_con_crossbasis_riesgos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# Fit DLNM-GLMM model with Gaussian distribution
modelo_dlnm_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +  
    as.factor(epi.week) + (1 | Subreg_climat),
  data = df_train,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# Predictions and forecast using DLNM-GLMM
df_train <- df_train %>%
  mutate(adjust_dlnm_glmm = predict(modelo_dlnm_glmm_normal, type = "response"))

df_test <- df_test %>%
  mutate(pred_dlnm_glmm = predict(modelo_dlnm_glmm_normal, newdata = df_test, type = "response"))

# Fit standard GLMM model (glmmTMB)
modelo_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol 
  + as.factor(epi.week),
  data = df_train,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# Predictions and forecast using GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_normal, type = "response"))

df_test <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_normal, newdata = df_test, type = "response"))

### Error metrics ----

# Function to compute error metrics
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
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$adjust_dlnm_glmm, "DLNM_GLMM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test$Riesgo_relativ_contagio, df_test$pred_dlnm_glmm, paste0("DLNM_GLMM_", anio_pronostico)),
  calcular_errores(df_test$Riesgo_relativ_contagio, df_test$pred_glmm, paste0("GLMM_", anio_pronostico))
)

# Combine results across all forecast years
errores_totales <- bind_rows(errores_2017,
                             errores_2018,
                             errores_2019)

# Display results in an interactive table
datatable(errores_totales, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(errores_totales),
    color = "black",
    backgroundColor = "lightgray"
  )
