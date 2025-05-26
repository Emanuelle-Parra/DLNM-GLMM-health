# 1. Libraries --------------------------------------------------------------

## 1.1 Data manipulation and transformation ----

library(dplyr) # Data manipulation
library(tidyr) # Data transformation
library(tidyverse) # Collection of packages for data analysis

## 1.2 Visualization and plotting packages ----

library(ggplot2) # ggplot style plotting
library(ggseas) # Visualization of seasonal components
library(scales) # Customization of plot scales
library(viridis) # Perceptual color palettes
library(corrplot) # Correlation plots
library(GGally) # Enhancements for ggplot2 (plot matrices)

## 1.3 Statistical analysis and modeling packages ----

library(car) # Linear model analysis and diagnostics
library(dlnm) # Nonlinear time series relationship models
library(forecast) # Time series forecasting
library(glmmTMB) # Generalized nonlinear mixed models
library(lme4) # Mixed effects models
library(mgcv) # Fitting GAM models
library(gamlss) # Fit GAMLSS models for zero inflation
library(MASS) # GLM model fitting
library(splines) # Generation of cubic splines
library(urca) # Unit root tests in time series
library(tseries) # Time series analysis

## 1.4 Information analysis and metrics packages ----

library(energy) # Distance Correlation (dCor)
library(infotheo) # Mutual Information (MI)
library(Metrics) # Model performance metrics

## 1.5 Spatial data packages ----

library(sf) # Handling spatial data

## 1.6 Table creation packages ----

library(kableExtra) # Table creation
library(DT) # Interactive tables


# 2. Data loading -----------------------------------------------------------

## 2.1 General database ----
load(file = "datos_final1_para_analizar.RData")

## 2.2 Spatial data ----
subregion_st <- st_read("Subegiones Climáticas.shp")


# 3. Data generation by region and subregion ---------------------------

# This block of code generates summary data by region and subregion,
# calculating key weekly statistics for each combination of year,
# epidemiological week, and subregion.
# The data includes summarized values for discharges, relative risk of contagion,
# population, precipitation, temperature, aerosol concentration, and relative humidity.

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

# A column is added to identify each region and all data sets are combined into a single set.

data_region1$region <- "PC"
data_region2$region <- "PS"
data_region3$region <- "PN"
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
# 4. Correlational analysis using permutation tests -----------------------

#' This block designs a function to analyze correlation between lagged predictor variables
#' and the response variable in region RA using different methods:
#' - Pearson correlation (linear)
#' - Distance correlation (dCor) (nonlinear)
#' - Mutual information (MI) (nonlinear)
#' Significance is assessed using permutation tests.


## 4.1 Functions to set crossbasis parametrization ----

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
#' - method: character, indicates the dependence method to use: "pearson" (default), "dcor", or "MI".
#' - n_perm: integer, number of permutations to perform (default 50).
#'
#' Value:
#' - A numeric value between 0 and 1 corresponding to the p-value from the permutation test.
#'
#' Details:
#' - For "pearson", |cor(x, y)| is computed with random permutations of x.
#' - For "dcor", the dcor() function from the energy package is used.
#' - For "MI", mutual information is calculated between discretized variables using mutinformation() from infotheo.

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
#' distributed lag nonlinear model (DLNM) structures, testing various
#' exposure and lag function types with a Normal model. The optimal model
#' is selected based on fit and prediction metrics.
#'
#' Parameters:
#' - data: data.frame with input data
#' - variable: name of the exposure variable (string)
#' - respuesta: name of the response variable (string)
#' - max_lag: maximum number of lags to evaluate
#' - max_df_var: maximum degrees of freedom for the exposure variable
#' - max_df_lag: maximum degrees of freedom for the lag structure
#'
#' Returns:
#' - A list with:
#'   - mejor_modelo: data.frame with the best model row
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
        # Create crossbasis with cubic splines
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
## 4.2 Correlational analysis in subregion RA1 ----

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
#' Calculate correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA1")
  
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


#' Calculate extreme values per metric
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
  labs(x = "Temperature lag (k)", y = "coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  # Maxima and minima annotations
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


#' Evaluation of univariate DLNM models for temperature in subregion RA1

data_ra1 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA1")

res <- buscar_mejor_modelo(
  data = data_ra1,
  variable = "Temp_media",
  respuesta = "Riesgo_relativ_contagio",
  max_lag = 10,
  max_df_var = 3,
  max_df_lag = 3
)

#' Display results in an interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.2.2 Mean precipitation ----

#' Initialize data frame to store lagged results

results_prec <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)

#' Calculate correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_ra1 <- data_ra_subreg %>% filter(Subreg_climat == "RA1")
  
  X_lagged <- head(data_ra1$precip_media, n = length(data_ra1$precip_media) - lag)
  Y_lagged <- tail(data_ra1$Riesgo_relativ_contagio,
                   n = length(data_ra1$Riesgo_relativ_contagio) - lag)
  
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

#' Identify extreme values for annotations

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
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Precipitation lag (k)", y = "Coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
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

#' Evaluation of univariate DLNM models for precipitation in subregion RA1

res <- buscar_mejor_modelo(
  data_ra_subreg %>% filter(Subreg_climat == "RA1"), # Filter subregion RA1
  variable = "precip_media", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 6, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by combined score

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

#' Initialize dataframe to store results for each lag

results_rh <- data.frame(
  Lag = integer(),
  Pearson = numeric(), Pearson_p = numeric(),
  dCor = numeric(), dCor_p = numeric(),
  MI = numeric(), MI_p = numeric()
)

#' Calculate correlations for each lag from 1 to `max_lag`
for (lag in 1:max_lag) {
  df_filtered <- data_ra_subreg %>% filter(Subreg_climat == "RA1")
  
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

#' Compute extreme values for graph annotations

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
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    x = "Relative Humidity lag (k)",
    y = "Coefficient"
  ) +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
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


#' Evaluation of univariate DLNM models for relative humidity in subregion RA1

res <- buscar_mejor_modelo(
  data_ra_subreg %>% filter(Subreg_climat == "RA1"), # filter subregion RA1
  variable = "RH", # explanatory variable
  respuesta = "Riesgo_relativ_contagio", # response variable
  max_lag = 12, # maximum lag
  max_df_var = 3, # max df for the variable
  max_df_lag = 3 # max df for the lags
)

#' Display all evaluated models sorted by the combined score

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

#' Initialize dataframe to store results for each lag

results_aod <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' Calculate correlations for each lag

for (lag in 1:max_lag) {
  datos <- data_ra_subreg %>% filter(Subreg_climat == "RA1")
  
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
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "AOD index lag (k)", y = "Coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (nonlinear)" = "red",
    "MI (nonlinear)" = "green"
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

#' Evaluation of univariate DLNM models for AOD in subregion RA1
res <- buscar_mejor_modelo(
  data_ra_subreg %>% filter(Subreg_climat == "RA1"), # Filter subregion RA1
  variable = "aerosol", # Explanatory variable (AOD)
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 13, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by the combined score
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

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA2")
  
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

#' Calculate extreme values by metric
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

#' Selection of the best DLNM model for mean temperature in subregion RA2

#' Filter climate subregion RA2 and fit DLNM models
res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA2"), # Filter subregion RA2
  variable = "Temp_media", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 11, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for lags
)

#' Display all evaluated models sorted by the combined score
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

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA2")
  
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

#' Calculate extreme values by metric

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
  labs(x = "Precipitation lag (k)", y = "Coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  #' Maxima and minima annotations
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

#' Evaluation of univariate DLNM models for precipitation in subregion RA2
res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA2"), # Filter subregion RA2
  variable = "precip_media", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by the combined score
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

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA2")
  
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

#' Compute maxima and minima for graph annotations

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
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Relative humidity lag (k)", y = "Coefficient") +
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

#' Fit DLNM models with different function and degrees of freedom combinations

res <- buscar_mejor_modelo(
  data = data_ra_subreg %>% filter(Subreg_climat == "RA2"), # Filter subregion RA2
  variable = "RH", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 7, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by the combined score
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

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>% filter(Subreg_climat == "RA2")
  
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

#' Calculate extreme values by metric

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
  data = data_ra_subreg %>% filter(Subreg_climat == "RA2"), # Filter subregion RA2
  variable = "aerosol", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 13, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by the combined score

datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
## 4.4 Correlational analysis in subregion RA3 ----

### 4.4.1 Mean temperature ----

#' Initialize results data frame
results_temp <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  X_lagged <- head(
    (data_ra_subreg %>% filter(Subreg_climat == "RA3"))$Temp_media,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA3"))$Temp_media) - lag
  )
  Y_lagged <- tail(
    (data_ra_subreg %>% filter(Subreg_climat == "RA3"))$Riesgo_relativ_contagio,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA3"))$Riesgo_relativ_contagio) - lag
  )
  
  #' Correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' Store results
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

#' Calculate extrema for annotation in plot

max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' Plot correlations by lag

ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature lag (k)", y = "Coefficient") +
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

#' Filter climate subregion RA3

data_ra3 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA3")

#' Search for best model using previously defined function

res <- buscar_mejor_modelo(
  data = data_ra3,
  variable = "Temp_media",
  respuesta = "Riesgo_relativ_contagio",
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
### 4.4.2 Mean precipitation ----

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

#' Calculate correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA3")
  
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

#' Calculate extreme values by metric

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
  labs(x = "Precipitation lag (k)", y = "Coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  #' Maxima and minima annotations
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

#' Filter climate subregion RA3

data_ra3 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA3")

#' Search for best model using previously defined function

res <- buscar_mejor_modelo(
  data = data_ra3,
  variable = "precip_media", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 11, # Maximum lag to evaluate
  max_df_var = 3, # Degrees of freedom for the variable
  max_df_lag = 3 # Degrees of freedom for lags
)

#' Display results in interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.4.3 Mean relative humidity ----

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

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA3")
  
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

#' Calculate extreme values by metric

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' Visualization of correlations by lag

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
  labs(x = "Relative humidity lag (k)", y = "Coefficient") +
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
  data = data_ra_subreg %>% filter(Subreg_climat == "RA3"), # Filter subregion RA3
  variable = "RH", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 13, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by the combined score

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

#' Initialize results data frame

results_aod <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA3")
  
  X_lagged <- head(data_lag$aerosol, length(data_lag$aerosol) - lag)
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
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

#' Calculate extreme values by metric
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
  data = data_ra_subreg %>% filter(Subreg_climat == "RA3"), # Filter subregion RA3
  variable = "aerosol", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 14, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by the combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
## 4.5 Correlational analysis in subregion RA4 ----

### 4.5.1 Mean temperature ----

#' Initialize results data frame
results_temp <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  X_lagged <- head(
    (data_ra_subreg %>% filter(Subreg_climat == "RA4"))$Temp_media,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA4"))$Temp_media) - lag
  )
  Y_lagged <- tail(
    (data_ra_subreg %>% filter(Subreg_climat == "RA4"))$Riesgo_relativ_contagio,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA4"))$Riesgo_relativ_contagio) - lag
  )
  
  #' Correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' Store results
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

#' Calculate extrema for annotation in plot

max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' Plot correlations by lag

ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (nonlinear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (nonlinear)")) +
  geom_point(aes(y = MI, color = "MI (nonlinear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (nonlinear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature lag (k)", y = "Coefficient") +
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

#' Filter climate subregion RA4

data_ra4 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA4")

#' Search for best model using previously defined function

res <- buscar_mejor_modelo(
  data = data_ra4,
  variable = "Temp_media",
  respuesta = "Riesgo_relativ_contagio",
  max_lag = 12,
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

### 4.5.2 Mean precipitation ----

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

#' Calculate correlations and p-values for each lag
for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA4")
  
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

#' Calculate extreme values for annotation

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
  labs(x = "Precipitation lag (k)", y = "Coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  #' Annotations for maxima and minima
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

#' Filter climate subregion RA4

data_ra4 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA4")

#' Search for the best model using the previously defined function

res <- buscar_mejor_modelo(
  data = data_ra4,
  variable = "precip_media", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 5, # Maximum lag to evaluate
  max_df_var = 3, # Degrees of freedom for the variable
  max_df_lag = 3 # Degrees of freedom for the lags
)

#' Display results in interactive table

datatable(res$resultado_general, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
### 4.5.3 Mean relative humidity ----

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

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA4")
  
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

#' Calculate extreme values by metric

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' Visualization of correlations by lag

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
  labs(x = "Relative humidity lag (k)", y = "Coefficient") +
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
  data = data_ra_subreg %>% filter(Subreg_climat == "RA4"), # Filter subregion RA4
  variable = "RH", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 12, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by the combined score

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

#' Initialize results data frame

results_aod <- data.frame(
  Lag = integer(),
  Pearson = numeric(),
  Pearson_p = numeric(),
  dCor = numeric(),
  dCor_p = numeric(),
  MI = numeric(),
  MI_p = numeric()
)

#' Calculate correlations and p-values for each lag

for (lag in 1:max_lag) {
  data_lag <- data_ra_subreg %>%
    filter(Subreg_climat == "RA4")
  
  X_lagged <- head(data_lag$aerosol, length(data_lag$aerosol) - lag)
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
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

#' Calculate extreme values by metric

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
  data = data_ra_subreg %>% filter(Subreg_climat == "RA4"), # Filter subregion RA4
  variable = "aerosol", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 10, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' Display all evaluated models sorted by the combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )

## 4.6 Correlation analysis in the RA5 subregion ----

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
    (data_ra_subreg %>% filter(Subreg_climat == "RA5"))$Riesgo_relativ_contagio,
    length((data_ra_subreg %>% filter(Subreg_climat == "RA5"))$Riesgo_relativ_contagio) - lag
  )
  
  #' correlation metrics
  pearson_corr <- cor(X_lagged, Y_lagged)
  pearson_p <- perm_test(X_lagged, Y_lagged, method = "pearson")
  
  dcor_val <- dcor(X_lagged, Y_lagged)
  dcor_p <- perm_test(X_lagged, Y_lagged, method = "dcor")
  
  MI_val <- mutinformation(discretize(X_lagged), discretize(Y_lagged))
  MI_p <- perm_test(X_lagged, Y_lagged, method = "MI")
  
  #' save results
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

#' compute extremes for annotation in the plot

max_pearson <- max(results_temp$Pearson)
min_pearson <- min(results_temp$Pearson)
max_dcor <- max(results_temp$dCor)
min_dcor <- min(results_temp$dCor)
max_mi <- max(results_temp$MI)
min_mi <- min(results_temp$MI)

#' plot correlations by lag

ggplot(results_temp, aes(x = Lag)) +
  geom_point(aes(y = Pearson, color = "Pearson (linear)"), shape = 18) +
  geom_line(aes(y = Pearson, color = "Pearson (linear)")) +
  geom_point(aes(y = dCor, color = "dCor (non-linear)"), shape = 18) +
  geom_line(aes(y = dCor, color = "dCor (non-linear)")) +
  geom_point(aes(y = MI, color = "MI (non-linear)"), shape = 17) +
  geom_line(aes(y = MI, color = "MI (non-linear)")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature lag (k)", y = "coefficient") +
  scale_color_manual(values = c(
    "Pearson (linear)" = "blue",
    "dCor (non-linear)" = "red",
    "MI (non-linear)" = "green"
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

#' filter the RA5 climatic subregion

data_ra5 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA5")

#' find the best model using the previously defined function

res <- buscar_mejor_modelo(
  data = data_ra5,
  variable = "Temp_media",
  respuesta = "Riesgo_relativ_contagio",
  max_lag = 11,
  max_df_var = 3,
  max_df_lag = 3
)

#' display results in an interactive table

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
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
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

#' compute extreme values for each metric

max_pearson <- max(results_prec$Pearson)
min_pearson <- min(results_prec$Pearson)
max_dcor <- max(results_prec$dCor)
min_dcor <- min(results_prec$dCor)
max_mi <- max(results_prec$MI)
min_mi <- min(results_prec$MI)

#' visualize correlations by lag

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
  labs(x = "Precipitation lag (k)", y = "coefficient") +
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

#' filter the RA5 climatic subregion

data_ra5 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA5")

#' find the best model using the previously defined function

res <- buscar_mejor_modelo(
  data = data_ra5,
  variable = "precip_media", # explanatory variable
  respuesta = "Riesgo_relativ_contagio", # response variable
  max_lag = 14, # maximum lag to evaluate
  max_df_var = 3, # degrees of freedom for the variable
  max_df_lag = 3 # degrees of freedom for the lags
)

#' display the results in an interactive table

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

#' compute extreme values for each metric

max_pearson <- max(results_rh$Pearson)
min_pearson <- min(results_rh$Pearson)
max_dcor <- max(results_rh$dCor)
min_dcor <- min(results_rh$dCor)
max_mi <- max(results_rh$MI)
min_mi <- min(results_rh$MI)

#' visualization of correlations by lag

ggplot(results_rh, aes(x = Lag)) +
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
  labs(x = "Relative humidity lag (k)", y = "coefficient") +
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
  data = data_ra_subreg %>% filter(Subreg_climat == "RA5"), # Filter RA5 subregion
  variable = "RH", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 13, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models sorted by combined score

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
  Y_lagged <- tail(data_lag$Riesgo_relativ_contagio,
                   length(data_lag$Riesgo_relativ_contagio) - lag)
  
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

#' compute extreme values for each metric
max_pearson <- max(results_aod$Pearson)
min_pearson <- min(results_aod$Pearson)
max_dcor <- max(results_aod$dCor)
min_dcor <- min(results_aod$dCor)
max_mi <- max(results_aod$MI)
min_mi <- min(results_aod$MI)

#' visualization of correlations by lag
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
  labs(x = "AOD index lag (k)", y = "coefficient") +
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
  data = data_ra_subreg %>% filter(Subreg_climat == "RA5"), # Filter RA5 subregion
  variable = "aerosol", # Explanatory variable
  respuesta = "Riesgo_relativ_contagio", # Response variable
  max_lag = 14, # Maximum lag
  max_df_var = 3, # Maximum df for the variable
  max_df_lag = 3 # Maximum df for the lags
)

#' visualize all evaluated models sorted by combined score
datatable(
  res$resultado_general,
  options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatStyle(
    columns = names(res$resultado_general),
    color = "black",
    backgroundColor = "lightgray"
  )
## 4.7 Function for Combined Correlational Analysis -----

#' ------------------------------------------------------------------------------
#' Function: continuous_modeling_parameters()
#' ------------------------------------------------------------------------------
#' Evaluates multiple combinations of non-linear base functions for climate 
#' variables (temperature, precipitation, relative humidity, and AOD), using 
#' normal models with distributed lags. This function was implemented at the 
#' subregional level using all exposure variables to adequately parameterize 
#' the non-linear base functions.
#' 
#' Inputs:
#' - data: complete dataset.
#' - respuesta: name of the response variable (count).
#' - temp_var, prec_var, rh_var, aod_var: names of the predictor variables.
#' - max_lag_*: maximum lag for each predictor.
#' - max_df_var_*: maximum df for the variable function.
#' - max_df_lag_*: maximum df for the lag function.
#'
#' Output:
#' - List containing a data.frame called 'resultados_completos' with 
#'   fit metrics (AIC, BIC, MAE, MSE, RMSE, R2, MAPE, logLik, score) 
#'   for each combination of parameters evaluated.
#'
#' Details:
#' - The fitted model is of type glm
#' - Includes seasonal effects via as.factor(epi.week).
#' - Uses crossbases generated with the crossbasis function (dlnm package).
#' ------------------------------------------------------------------------------

continuous_modeling_parameters <- function(
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
  
  # Function to evaluate the model and compute metrics
  # The R2 term is removed from the score.
  evaluar_modelo <- function(modelo, data_filtrada, respuesta, tipo, params) {
    predicciones <- predict(modelo, type = "response")
    obs <- data_filtrada[[respuesta]]
    
    MAE    <- mean(abs(obs - predicciones))
    MSE    <- mean((obs - predicciones)^2)
    RMSE   <- sqrt(MSE)
    R2     <- cor(obs, predicciones)^2
    AIC_val <- AIC(modelo)
    BIC_val <- BIC(modelo)
    MAPE_val <- mean(abs((obs - predicciones) / obs)) * 100  # Mean Absolute Percentage Error
    ll <- as.numeric(logLik(modelo))
    
    # Condition for the logLik term in the score
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
  
  # Get the maximum lag across all crossbases to filter the dataset
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
  
  # Nested loops to combine specifications for each variable
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
          
          # MODEL (another continuous function could be used to validate; the behavior is the same)
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
  
  return(list(resultados_completos = resultados
  ))
}
# 5. Construction of Crossbasis by Climatic Subregion ----

#' This block defines the crossbasis functions required for the 
#' DLNM-GLMM modeling of risks associated with hospital discharges in the 
#' subregions RA1, RA2, and RA3, based on climatic and air pollution variables.

#' sort dataset by climatic subregion

data_ra_subreg <- data_ra_subreg %>%
  arrange(Subreg_climat)

## 5.1 Crossbasis for Subregion RA1 ----

cb_t1 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA1") %>% pull(Temp_media),
                    lag = 10, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
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
                    lag = 11, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p2 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA2") %>% pull(precip_media),
                    lag = 12, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh2 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA2") %>% pull(RH),
                     lag = 6, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod2 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA2") %>% pull(aerosol),
                      lag = 3, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.4 Crossbasis for Subregion RA3 ----

cb_t3 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA3") %>% pull(Temp_media),
                    lag = 7, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p3 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA3") %>% pull(precip_media),
                    lag = 10, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh3 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA3") %>% pull(RH),
                     lag = 12, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod3 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA3") %>% pull(aerosol),
                      lag = 14, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.5 Crossbasis for Subregion RA4 ----

cb_t4 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA4") %>% pull(Temp_media),
                    lag = 3, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p4 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA4") %>% pull(precip_media),
                    lag = 4, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh4 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA4") %>% pull(RH),
                     lag = 12, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod4 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA4") %>% pull(aerosol),
                      lag = 9, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)

## 5.6 Crossbasis for Subregion RA5 ----

cb_t5 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA5") %>% pull(Temp_media),
                    lag = 11, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_p5 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA5") %>% pull(precip_media),
                    lag = 12, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_rh5 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA5") %>% pull(RH),
                     lag = 13, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
cb_aod5 <- crossbasis(data_ra_subreg %>% filter(Subreg_climat == "RA5") %>% pull(aerosol),
                      lag = 14, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3)
)
## 5.7 Merging Crossbasis -----

#' combine data with crossbasis matrices

df_con_crossbasis_riesgos <- cbind(
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

  #' aerosol index (AOD)
  setNames(
    as.data.frame(rbind(cb_aod1, cb_aod2, cb_aod3, cb_aod4, cb_aod5)),
    make.names(paste0("AOD_", colnames(cb_aod1)), unique = TRUE)
  )
)

#' Remove initial observations per group according to the maximum lag (lag = 14)

df_con_crossbasis_riesgos <- df_con_crossbasis_riesgos %>%
  group_by(Subreg_climat) %>%
  slice(-(1:14)) %>%
  ungroup()

#' Retrieve the names of columns generated in crossbasis

columnas_exposicion_riesgos <- colnames(df_con_crossbasis_riesgos)[12:ncol(df_con_crossbasis_riesgos)]
columnas_exposicion_riesgos

df_con_crossbasis_riesgos$Subreg_climat <- as.factor(df_con_crossbasis_riesgos$Subreg_climat)

## Need for Zero-Inflation in Gamma Distribution Models ----

#' Percentage of zeros in subregions, overall
summary(data_ra_subreg$Riesgo_relativ_contagio)

100*sum(data_ra_subreg$Riesgo_relativ_contagio == 0)/(length(data_ra_subreg$Riesgo_relativ_contagio))

#' Percentage of zeros in subregion RA1
riesgo_ra1 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA1") %>%
  pull(Riesgo_relativ_contagio) 

100*sum(riesgo_ra1 == 0, na.rm = TRUE)/(length(riesgo_ra1))

#' Percentage of zeros in subregion RA2
riesgo_ra2 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA2") %>%
  pull(Riesgo_relativ_contagio) 

100*sum(riesgo_ra2 == 0, na.rm = TRUE)/(length(riesgo_ra2))

#' Percentage of zeros in subregion RA3
riesgo_ra3 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA3") %>%
  pull(Riesgo_relativ_contagio) 

100*sum(riesgo_ra3 == 0, na.rm = TRUE)/(length(riesgo_ra3))

#' Percentage of zeros in subregion RA4
riesgo_ra4 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA4") %>%
  pull(Riesgo_relativ_contagio) 

100*sum(riesgo_ra4 == 0, na.rm = TRUE)/(length(riesgo_ra4))

#' Percentage of zeros in subregion RA5
riesgo_ra5 <- data_ra_subreg %>%
  filter(Subreg_climat == "RA5") %>%
  pull(Riesgo_relativ_contagio) 

100*sum(riesgo_ra5 == 0, na.rm = TRUE)/(length(riesgo_ra5))
# 6. DLNM-GLMM and DLNM-GAMLSS Models ----

## 6.1 GLMM Normal Model (1): simple ----

modelo_glmm_normal_ra <- glmmTMB(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol 
  + as.factor(epi.week),
  data = df_con_crossbasis_riesgos,
  family = gaussian(link = "identity")
)


## 6.2 DLNM + GLMM Model (2): with normal distribution ----

modelo_dlnm_glmm_normal_ra <- glmmTMB(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +  
    as.factor(epi.week) + 
    (1 | Subreg_climat), # Random effects
  data = df_con_crossbasis_riesgos,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)


## 6.3 DLNM + GAMLSS Model (1): with zero-altered inverse Gaussian distribution ----

modelo_dlnm_gamlss_inv_gauss_ra <- gamlss(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +  
    as.factor(epi.week) + random(Subreg_climat),
  data = df_con_crossbasis_riesgos,
  family = ZAIG()
)


## 6.4 DLNM + GAMLSS Model (2): with zero-altered gamma distribution ----

modelo_dlnm_gamlss_gamma_ra <- gamlss(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 +  
    as.factor(epi.week) + random(Subreg_climat), 
  data = df_con_crossbasis_riesgos,
  family = ZAGA()
)
# 7. Optimal Model Selection ----

#' This block applies the `calcular_metricas()` function to the fitted models
#' (GLMM and GAMLSS with different distributions), and builds a table
#' ordered by the best fit score.

#' Add fitted values to the dataset
df_con_crossbasis_riesgos_ra <- df_con_crossbasis_riesgos %>%
  mutate(
    #' GLMM Normal Model
    ajustado_modelo_glmm_normal_ra = fitted(modelo_glmm_normal_ra),
    
    #' DLNM + GLMM Normal Model
    ajustado_modelo_dlnm_glmm_normal_ra = fitted(modelo_dlnm_glmm_normal_ra),
    
    #' DLNM + GAMLSS Models
    ajustado_modelo_dlnm_gamlss_inv_gauss_ra = fitted(modelo_dlnm_gamlss_inv_gauss_ra),
    ajustado_modelo_dlnm_gamlss_gamma_ra = fitted(modelo_dlnm_gamlss_gamma_ra)
  )


## 7.1 Metrics for Each Model ----

#' ------------------------------------------------------------------------------
#' calcular_metricas()
#' ------------------------------------------------------------------------------
#' Function to calculate performance metrics of a fitted model and its
#' associated predictions. Includes classical error metrics, information
#' criteria, and a composite score summarizing overall performance.
#'
#' Parameters:
#' - modelo: fitted model object
#' - predicciones: numeric vector of model predictions
#' - obs: numeric vector of observed values
#' - nombre_modelo: identifier string for the evaluated model
#'
#' Returns:
#' - Data frame with MAE, MSE, RMSE, MAPE, AIC, BIC, logLik, and Score metrics
#' ------------------------------------------------------------------------------

calcular_metricas <- function(modelo, predicciones, obs, nombre_modelo) {
  #' --------------------------------------------------------------------------
  #' Error metric calculations
  #' --------------------------------------------------------------------------
  MAE <- mean(abs(obs - predicciones))
  MSE <- mean((obs - predicciones)^2)
  RMSE <- sqrt(MSE)
  
  #' MAPE - includes correction for division by zero
  erailon <- 1e-10
  MAPE_val <- mean(abs((obs - predicciones) / (obs + erailon))) * 100
  
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
  #' Assemble result as data.frame
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


resultados_modelos <- rbind(
  calcular_metricas(
    modelo_glmm_normal_ra,
    df_con_crossbasis_riesgos_ra$ajustado_modelo_glmm_normal_ra,
    df_con_crossbasis_riesgos_ra$Riesgo_relativ_contagio,
    "GLMM_Normal"
  ),
  calcular_metricas(
    modelo_dlnm_glmm_normal_ra,
    df_con_crossbasis_riesgos_ra$ajustado_modelo_dlnm_glmm_normal_ra,
    df_con_crossbasis_riesgos_ra$Riesgo_relativ_contagio,
    "DLNM_GLMM_Normal"
  ),
  calcular_metricas(
    modelo_dlnm_gamlss_inv_gauss_ra,
    df_con_crossbasis_riesgos_ra$ajustado_modelo_dlnm_gamlss_inv_gauss_ra,
    df_con_crossbasis_riesgos_ra$Riesgo_relativ_contagio,
    "DLNM_GAMLSS_INV_GAUSS"
  ),
  calcular_metricas(
    modelo_dlnm_gamlss_gamma_ra,
    df_con_crossbasis_riesgos_ra$ajustado_modelo_dlnm_gamlss_gamma_ra,
    df_con_crossbasis_riesgos_ra$Riesgo_relativ_contagio,
    "DLNM_GAMLSS_GAMMA"
  )
)

#' Order models from best to worst according to the obtained score

resultados_modelos <- resultados_modelos %>%
  arrange(desc(Score))

#' Display results in an interactive table

datatable(resultados_modelos, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados_modelos),
    color = "black",
    backgroundColor = "lightgray"
  )
## 7.2 Best Performing Model -----

dlnm_glmm_model_ra <- modelo_dlnm_gamlss_gamma_ra
summary(dlnm_glmm_model_ra)


## 7.3 Metrics for Filtered Subregions ----

#' Time series construction by model for RA1

df_ra1 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA1") %>%
  mutate(
    riesgos_ts = ts(Riesgo_relativ_contagio, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_modelo_glmm_normal_ra, start = c(2000, 15), frequency = 52),
    dlnm_gamlss_ts = ts(ajustado_modelo_dlnm_gamlss_gamma_ra, start = c(2000, 15), frequency = 52)
  )

#' Time series construction by model for RA2
df_ra2 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA2") %>%
  mutate(
    riesgos_ts = ts(Riesgo_relativ_contagio, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_modelo_glmm_normal_ra, start = c(2000, 15), frequency = 52),
    dlnm_gamlss_ts = ts(ajustado_modelo_dlnm_gamlss_gamma_ra, start = c(2000, 15), frequency = 52)
  )

#' Time series construction by model for RA3
df_ra3 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA3") %>%
  mutate(
    riesgos_ts = ts(Riesgo_relativ_contagio, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_modelo_glmm_normal_ra, start = c(2000, 15), frequency = 52),
    dlnm_gamlss_ts = ts(ajustado_modelo_dlnm_gamlss_gamma_ra, start = c(2000, 15), frequency = 52)
  )

#' Time series construction by model for RA4
df_ra4 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA4") %>%
  mutate(
    riesgos_ts = ts(Riesgo_relativ_contagio, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_modelo_glmm_normal_ra, start = c(2000, 15), frequency = 52),
    dlnm_gamlss_ts = ts(ajustado_modelo_dlnm_gamlss_gamma_ra, start = c(2000, 15), frequency = 52)
  )

#' Time series construction by model for RA5
df_ra5 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA5") %>%
  mutate(
    riesgos_ts = ts(Riesgo_relativ_contagio, start = c(2000, 15), frequency = 52),
    glmm_ts = ts(ajustado_modelo_glmm_normal_ra, start = c(2000, 15), frequency = 52),
    dlnm_gamlss_ts = ts(ajustado_modelo_dlnm_gamlss_gamma_ra, start = c(2000, 15), frequency = 52)
  )


calcular_metricas_sub <- function(df, subregion, col_obs = "riesgos_ts") {
  #' --------------------------------------------------------------------------
  #' Error metrics for models fitted by subregion
  #' Returns the top two predictions based on lowest Score
  #' --------------------------------------------------------------------------
  
  #' Identify prediction columns (ending in _ts), excluding the observed one
  columnas_modelo <- setdiff(
    names(df)[grepl("_ts$", names(df))],
    col_obs
  )
  
  #' Real observation vector
  obs <- df[[col_obs]]
  
  #' Small value to avoid division by zero in MAPE
  erailon <- 1e-10
  
  #' --------------------------------------------------------------------------
  #' Compute error metrics for each fitted model
  #' --------------------------------------------------------------------------
  resultados <- lapply(columnas_modelo, function(nombre_modelo) {
    pred <- df[[nombre_modelo]]
    
    MAE <- mean(abs(obs - pred))
    MSE <- mean((obs - pred)^2)
    RMSE <- sqrt(MSE)
    MAPE_val <- mean(abs((obs - pred) / (obs + erailon))) * 100
    
    #' Composite score as the sum of inverses of the error metrics
    score <- (1 / MAE) + (1 / MSE) + (1 / RMSE) + (1 / MAPE_val)
    
    #' Assemble result into a data.frame
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
  calcular_metricas_sub(df_ra1, "RA1", "Riesgo_relativ_contagio"),
  calcular_metricas_sub(df_ra2, "RA2", "Riesgo_relativ_contagio"),
  calcular_metricas_sub(df_ra3, "RA3", "Riesgo_relativ_contagio"),
  calcular_metricas_sub(df_ra4, "RA4", "Riesgo_relativ_contagio"),
  calcular_metricas_sub(df_ra5, "RA5", "Riesgo_relativ_contagio")
)

#' Order models from best to worst according to the obtained score
resultados_modelos <- resultados_modelos %>%
  arrange(desc(Score))

#' Display results in an interactive table
datatable(resultados_modelos, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(resultados_modelos),
    color = "black",
    backgroundColor = "lightgray"
  )
# 8. Residuals of the Optimal Model ----

#' Simple residuals (similar to response or deviance residuals)
resid_simple_ra <- residuals(dlnm_glmm_model_ra, type = "simple")

#' Weighted residuals (similar to Pearson residuals)
resid_weighted_ra <- residuals(dlnm_glmm_model_ra, type = "weighted")

#' Partial residuals (used to evaluate partial effects)
resid_partial_ra <- residuals(dlnm_glmm_model_ra, type = "partial")

#' dataset with residuals from the model fit
df_con_crossbasis_riesgos_ra <- df_con_crossbasis_riesgos_ra %>%
  mutate(
    resid_simple_ra = resid_simple_ra,
    resid_weighted_ra = resid_weighted_ra,
    resid_partial_ra = resid_partial_ra
  )

## 9.1 Residual Diagnostics ----

#' Q-Q plots to assess normality of global residuals

qqnorm(resid_simple_ra, main = "Simple residuals")
qqline(resid_simple_ra, col = "red")

qqnorm(resid_weighted_ra, main = "Weighted residuals")
qqline(resid_weighted_ra, col = "red")

shapiro_test_simple <- shapiro.test(resid_simple_ra)
shapiro_test_weighted <- shapiro.test(resid_weighted_ra)

# Display test results
print(shapiro_test_simple)
print(shapiro_test_weighted)

#' Extract simple residuals by climatic subregion

resid_simple_ra1 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA1") %>%
  pull(resid_simple_ra)

resid_simple_ra2 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA2") %>%
  pull(resid_simple_ra)

resid_simple_ra3 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA3") %>%
  pull(resid_simple_ra)

resid_simple_ra4 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA4") %>%
  pull(resid_simple_ra)

resid_simple_ra5 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA5") %>%
  pull(resid_simple_ra)

#' Extract weighted residuals by climatic subregion

resid_weighted_ra1 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA1") %>%
  pull(resid_weighted_ra)

resid_weighted_ra2 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA2") %>%
  pull(resid_weighted_ra)

resid_weighted_ra3 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA3") %>%
  pull(resid_weighted_ra)

resid_weighted_ra4 <- df_con_crossbasis_riesgos_ra %>%
  filter(Subreg_climat == "RA4") %>%
  pull(resid_weighted_ra)

resid_weighted_ra5 <- df_con_crossbasis_riesgos_ra %>%
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
# 9. Fitted Effects of the Model ----

## 9.1 Comparative Plots of Model Fits ----

#' Comparative plots of predicted values - Subregion RA1
autoplot(df_ra1$riesgos_ts, series = "Observed") +
  autolayer(df_ra1$glmm_ts, series = "GLMM") +
  autolayer(df_ra1$dlnm_gamlss_ts, series = "DLNM - GAMLSS") +
  labs(
    x = "Time (weeks)",
    y = "Risk of hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM" = "blue",
    "DLNM - GAMLSS" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plots of predicted values - Subregion RA2
autoplot(df_ra2$riesgos_ts, series = "Observed") +
  autolayer(df_ra2$glmm_ts, series = "GLMM") +
  autolayer(df_ra2$dlnm_gamlss_ts, series = "DLNM - GAMLSS") +
  labs(
    x = "Time (weeks)",
    y = "Risk of hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM" = "blue",
    "DLNM - GAMLSS" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plots of predicted values - Subregion RA3
autoplot(df_ra3$riesgos_ts, series = "Observed") +
  autolayer(df_ra3$glmm_ts, series = "GLMM") +
  autolayer(df_ra3$dlnm_gamlss_ts, series = "DLNM - GAMLSS") +
  labs(
    x = "Time (weeks)",
    y = "Risk of hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM" = "blue",
    "DLNM - GAMLSS" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plots of predicted values - Subregion RA4
autoplot(df_ra4$riesgos_ts, series = "Observed") +
  autolayer(df_ra4$glmm_ts, series = "GLMM") +
  autolayer(df_ra4$dlnm_gamlss_ts, series = "DLNM - GAMLSS") +
  labs(
    x = "Time (weeks)",
    y = "Risk of hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM" = "blue",
    "DLNM - GAMLSS" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#' Comparative plots of predicted values - Subregion RA5
autoplot(df_ra5$riesgos_ts, series = "Observed") +
  autolayer(df_ra5$glmm_ts, series = "GLMM") +
  autolayer(df_ra5$dlnm_gamlss_ts, series = "DLNM - GAMLSS") +
  labs(
    x = "Time (weeks)",
    y = "Risk of hospital discharges"
  ) +
  theme_bw() +
  scale_color_manual(values = c(
    "Observed" = "green",
    "GLMM" = "blue",
    "DLNM - GAMLSS" = "red"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
## 9.2 Predictions ----

coef_cond <- coef(dlnm_glmm_model_ra)
vcov_full <- vcov(dlnm_glmm_model_ra, full = TRUE)

# Save the models to the folder
save(
  modelo_glmm_normal_ra,
  modelo_dlnm_glmm_normal_ra,
  modelo_dlnm_gamlss_inv_gauss_ra,
  modelo_dlnm_gamlss_gamma_ra,
  dlnm_glmm_model_ra,
  coef_cond,
  vcov_full,
  file = "random_effects_models/dlnm_models_for_risk_ra.RData"
)

# Load saved models
load("random_effects_models/dlnm_models_for_risk_ra.RData")

# Extract coefficients and var-cov matrix for temperature (cb_t1 - subregion RA1)
coef_names_t <- grep("^T_", names(coef_cond), value = TRUE)
coef_t <- coef_cond[coef_names_t]
vcov_t <- as.matrix(vcov_full[coef_names_t, coef_names_t])

# Extract coefficients and var-cov matrix for precipitation (cb_p1 - subregion RA1)
coef_names_p <- grep("^P_", names(coef_cond), value = TRUE)
coef_p <- coef_cond[coef_names_p]
vcov_p <- as.matrix(vcov_full[coef_names_p, coef_names_p])

# Extract coefficients and var-cov matrix for relative humidity (cb_rh1 - subregion RA1)
coef_names_rh <- grep("^RH_", names(coef_cond), value = TRUE)
coef_rh <- coef_cond[coef_names_rh]
vcov_rh <- as.matrix(vcov_full[coef_names_rh, coef_names_rh])

# Extract coefficients and var-cov matrix for AOD (cb_aod1 - subregion RA1)
coef_names_aod <- grep("^AOD_", names(coef_cond), value = TRUE)
coef_aod <- coef_cond[coef_names_aod]
vcov_aod <- as.matrix(vcov_full[coef_names_aod, coef_names_aod])

# Subregion RA1
# Create predictions for temperature using crosspred (DLNM-GLMM, RA1)
pred_dlnm_glmm_ra1_temp <- crosspred(
  cb_t1,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA1") %>%
    pull(Temp_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

# Create predictions for precipitation
pred_dlnm_glmm_ra1_prec <- crosspred(
  cb_p1,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA1") %>%
    pull(precip_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

# Create predictions for relative humidity
pred_dlnm_glmm_ra1_rh <- crosspred(
  cb_rh1,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA1") %>%
    pull(RH) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

# Create predictions for AOD
pred_dlnm_glmm_ra1_aod <- crosspred(
  cb_aod1,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA1") %>%
    pull(aerosol) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

### 9.2.1 Mean Temperature ----

#' 3D plot of lag-response for mean temperature
plot(
  pred_dlnm_glmm_ra1_temp,
  xlab = "Mean temperature",
  zlab = "RR",
  theta = 300,
  phi = 10,
  lphi = 100
)

#' Contour lag-response plot for mean temperature
plot(
  pred_dlnm_glmm_ra1_temp,
  "contour",
  xlab = "Mean temperature",
  ylab = "Lag",
  key.title = title("RR")
)

#' Lag-response plot for specific temperature value (var = 21.1 °C)
plot(
  pred_dlnm_glmm_ra1_temp,
  "slices",
  var = 21.1,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific temperature value (var = 16 °C)
plot(
  pred_dlnm_glmm_ra1_temp,
  "slices",
  var = 16,
  col = 2,
  ylab = "RR"
)

#' Overall cumulative effect plot for temperature
plot(
  pred_dlnm_glmm_ra1_temp,
  "overall",
  xlab = "Mean temperature",
  ylab = "RR"
)
### 9.2.2 Mean Precipitation ----

#' 3D lag-response plot for mean precipitation
plot(
  pred_dlnm_glmm_ra1_prec,
  xlab = "Mean precipitation",
  zlab = "RR",
  theta = 300,
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

#' Overall cumulative effect plot for precipitation
plot(
  pred_dlnm_glmm_ra1_prec,
  "overall",
  xlab = "Mean precipitation",
  ylab = "RR"
)


### 9.2.3 Mean Relative Humidity ----

#' 3D lag-response plot for relative humidity
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

#' Lag-response plot for a specific relative humidity value (var = 0.225)
plot(
  pred_dlnm_glmm_ra1_rh,
  "slices",
  var = 0.225,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific relative humidity value (var = 0.35)
plot(
  pred_dlnm_glmm_ra1_rh,
  "slices",
  var = 0.350,
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


### 9.2.4 Mean AOD Index ----

#' 3D lag-response plot for AOD
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

#' Lag-response plot for a specific AOD value (var = 0)
plot(
  pred_dlnm_glmm_ra1_aod,
  "slices",
  var = 0,
  col = 2,
  ylab = "RR"
)

#' Lag-response plot for another specific AOD value (var = 0.00057)
plot(
  pred_dlnm_glmm_ra1_aod,
  "slices",
  var = max(pred_dlnm_glmm_ra1_aod$predvar),
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
#' Subregion RA2 

#' Create predictions for temperature with crosspred (DLNM-GLMM, subregion RA2)
pred_dlnm_glmm_ra2_temp <- crosspred(
  cb_t2,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA2") %>%
    pull(Temp_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for precipitation with crosspred (DLNM-GLMM, subregion RA2)
pred_dlnm_glmm_ra2_prec <- crosspred(
  cb_p2,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA2") %>%
    pull(precip_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for relative humidity with crosspred (DLNM-GLMM, subregion RA2)
pred_dlnm_glmm_ra2_rh <- crosspred(
  cb_rh2,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA2") %>%
    pull(RH) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for AOD with crosspred (DLNM-GLMM, subregion RA2)
pred_dlnm_glmm_ra2_aod <- crosspred(
  cb_aod2,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA2") %>%
    pull(aerosol) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)


#' Subregion RA3 
#' Create predictions for temperature with crosspred (DLNM-GLMM, subregion RA3)
pred_dlnm_glmm_ra3_temp <- crosspred(
  cb_t3,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA3") %>%
    pull(Temp_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for precipitation with crosspred (DLNM-GLMM, subregion RA3)
pred_dlnm_glmm_ra3_prec <- crosspred(
  cb_p3,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA3") %>%
    pull(precip_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for relative humidity with crosspred (DLNM-GLMM, subregion RA3)
pred_dlnm_glmm_ra3_rh <- crosspred(
  cb_rh3,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA3") %>%
    pull(RH) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for AOD with crosspred (DLNM-GLMM, subregion RA3)
pred_dlnm_glmm_ra3_aod <- crosspred(
  cb_aod3,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA3") %>%
    pull(aerosol) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)


#' Subregion RA4 
#' Create predictions for temperature with crosspred (DLNM-GLMM, subregion RA4)
pred_dlnm_glmm_ra4_temp <- crosspred(
  cb_t3,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA4") %>%
    pull(Temp_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for precipitation with crosspred (DLNM-GLMM, subregion RA4)
pred_dlnm_glmm_ra4_prec <- crosspred(
  cb_p3,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA4") %>%
    pull(precip_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for relative humidity with crosspred (DLNM-GLMM, subregion RA4)
pred_dlnm_glmm_ra4_rh <- crosspred(
  cb_rh3,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA4") %>%
    pull(RH) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for AOD with crosspred (DLNM-GLMM, subregion RA4)
pred_dlnm_glmm_ra4_aod <- crosspred(
  cb_aod3,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA4") %>%
    pull(aerosol) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)


#' Subregion RA5 
#' Create predictions for temperature with crosspred (DLNM-GLMM, subregion RA5)
pred_dlnm_glmm_ra5_temp <- crosspred(
  cb_t3,
  coef = coef_t,
  vcov = vcov_t,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA5") %>%
    pull(Temp_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for precipitation with crosspred (DLNM-GLMM, subregion RA5)
pred_dlnm_glmm_ra5_prec <- crosspred(
  cb_p3,
  coef = coef_p,
  vcov = vcov_p,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA5") %>%
    pull(precip_media) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for relative humidity with crosspred (DLNM-GLMM, subregion RA5)
pred_dlnm_glmm_ra5_rh <- crosspred(
  cb_rh3,
  coef = coef_rh,
  vcov = vcov_rh,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA5") %>%
    pull(RH) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)

#' Create predictions for AOD with crosspred (DLNM-GLMM, subregion RA5)
pred_dlnm_glmm_ra5_aod <- crosspred(
  cb_aod3,
  coef = coef_aod,
  vcov = vcov_aod,
  model.link = "log",
  cen = data_ra_subreg %>%
    filter(Subreg_climat == "RA5") %>%
    pull(aerosol) %>%
    median(na.rm = TRUE),
  bylag = 0.1
)
# 10. Forecasts ----

## 10.1 Forecast for 2017 ----

# year to forecast
anio_pronostico <- 2017

# training dataset: excludes the forecast year
df_train <- df_con_crossbasis_riesgos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# test dataset: only includes the forecast year
df_test <- df_con_crossbasis_riesgos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# set common levels for year in train and test
niveles_year <- sort(unique(df_con_crossbasis_riesgos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# fit DLNM-GLMM model with Gamma distribution
modelo_dlnm_gamlss_gamma_ra <- gamlss(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3  +  as.factor(epi.week) + 
    random(Subreg_climat), 
  data = df_train,
  family = ZAGA()
)

# prediction and forecasting with DLNM-GLMM
df_train <- df_train %>%
  mutate(adjust_dlnm_glmm = predict(modelo_dlnm_gamlss_gamma_ra, type = "response"))

df_test <- df_test %>%
  mutate(pred_dlnm_glmm = predict(modelo_dlnm_gamlss_gamma_ra, newdata = df_test, type = "response"))

# fit GLMM model (glmmTMB)
modelo_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol 
  + as.factor(epi.week),
  data = df_train,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# prediction and forecasting with GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_normal, type = "response"))

df_test_2017 <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_normal, newdata = df_test, type = "response"))

### Error Metrics ----

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

# Error summary
errores_2017 <- bind_rows(
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$adjust_dlnm_glmm, "DLNM_GLMM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test_2017$Riesgo_relativ_contagio, df_test_2017$pred_dlnm_glmm, paste0("DLNM_GLMM_", anio_pronostico)),
  calcular_errores(df_test_2017$Riesgo_relativ_contagio, df_test_2017$pred_glmm, paste0("GLMM_", anio_pronostico))
)
## 10.2 Forecast for 2018 ----

# year to forecast
anio_pronostico <- 2018

# training dataset: excludes the forecast year
df_train <- df_con_crossbasis_riesgos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# test dataset: includes only the forecast year
df_test <- df_con_crossbasis_riesgos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# set common levels for 'year' in train and test
niveles_year <- sort(unique(df_con_crossbasis_riesgos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# fit DLNM-GLMM model with Gamma distribution
modelo_dlnm_gamlss_gamma_ra <- gamlss(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3  +  as.factor(epi.week) + 
    random(Subreg_climat), 
  data = df_train,
  family = ZAGA()
)

# prediction and forecasting with DLNM-GLMM
df_train <- df_train %>%
  mutate(adjust_dlnm_glmm = predict(modelo_dlnm_gamlss_gamma_ra, type = "response"))

df_test <- df_test %>%
  mutate(pred_dlnm_glmm = predict(modelo_dlnm_gamlss_gamma_ra, newdata = df_test, type = "response"))

# fit GLMM model (glmmTMB)
modelo_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol 
  + as.factor(epi.week),
  data = df_train,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# prediction and forecasting with GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_normal, type = "response"))

df_test_2018 <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_normal, newdata = df_test, type = "response"))

### Error Metrics ----

# Error summary
errores_2018 <- bind_rows(
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$adjust_dlnm_glmm, "DLNM_GLMM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test_2018$Riesgo_relativ_contagio, df_test_2018$pred_dlnm_glmm, paste0("DLNM_GLMM_", anio_pronostico)),
  calcular_errores(df_test_2018$Riesgo_relativ_contagio, df_test_2018$pred_glmm, paste0("GLMM_", anio_pronostico))
)
## 10.3 Forecast for 2019 ----

# year to forecast
anio_pronostico <- 2019

# training dataset: excludes the forecast year
df_train <- df_con_crossbasis_riesgos %>%
  filter(year != anio_pronostico)
df_train$Subreg_climat <- as.factor(df_train$Subreg_climat)

# test dataset: only the forecast year
df_test <- df_con_crossbasis_riesgos %>%
  filter(year == anio_pronostico)
df_test$Subreg_climat <- as.factor(df_test$Subreg_climat)

# set common levels for year in train and test
niveles_year <- sort(unique(df_con_crossbasis_riesgos$year))

df_train <- df_train %>%
  mutate(year = factor(year, levels = niveles_year))

df_test <- df_test %>%
  mutate(year = factor(year, levels = niveles_year))

# fit DLNM-GLMM model with Gamma distribution
modelo_dlnm_gamlss_gamma_ra <- gamlss(
  Riesgo_relativ_contagio ~ T_v1.l1 + T_v1.l2 + T_v1.l3 + 
    T_v2.l1 + T_v2.l2 + T_v2.l3 + T_v3.l1 + T_v3.l2 + T_v3.l3 + 
    P_v1.l1 + P_v1.l2 + P_v1.l3 + P_v2.l1 + P_v2.l2 + P_v2.l3 + 
    P_v3.l1 + P_v3.l2 + P_v3.l3 + RH_v1.l1 + RH_v1.l2 + RH_v1.l3 + 
    RH_v2.l1 + RH_v2.l2 + RH_v2.l3 + RH_v3.l1 + RH_v3.l2 + RH_v3.l3 + 
    AOD_v1.l1 + AOD_v1.l2 + AOD_v1.l3 + AOD_v2.l1 + AOD_v2.l2 + 
    AOD_v2.l3 + AOD_v3.l1 + AOD_v3.l2 + AOD_v3.l3 + as.factor(epi.week) + 
    random(Subreg_climat), 
  data = df_train,
  family = ZAGA()
)

# prediction and forecasting with DLNM-GLMM
df_train <- df_train %>%
  mutate(adjust_dlnm_glmm = predict(modelo_dlnm_gamlss_gamma_ra, type = "response"))

df_test <- df_test %>%
  mutate(pred_dlnm_glmm = predict(modelo_dlnm_gamlss_gamma_ra, newdata = df_test, type = "response"))

# fit GLMM model (glmmTMB)
modelo_glmm_normal <- glmmTMB(
  Riesgo_relativ_contagio ~ Temp_media + precip_media + RH + aerosol + as.factor(epi.week),
  data = df_train,
  family = gaussian(link = "identity"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

# prediction and forecasting with GLMM
df_train <- df_train %>%
  mutate(ajust_glmm = predict(modelo_glmm_normal, type = "response"))

df_test_2019 <- df_test %>%
  mutate(pred_glmm = predict(modelo_glmm_normal, newdata = df_test, type = "response"))

### Error Metrics ----

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

# Error summary
errores_2019 <- bind_rows(
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$adjust_dlnm_glmm, "DLNM_GLMM_train"),
  calcular_errores(df_train$Riesgo_relativ_contagio, df_train$ajust_glmm, "GLMM_train"),
  calcular_errores(df_test_2019$Riesgo_relativ_contagio, df_test_2019$pred_dlnm_glmm, paste0("DLNM_GLMM_", anio_pronostico)),
  calcular_errores(df_test_2019$Riesgo_relativ_contagio, df_test_2019$pred_glmm, paste0("GLMM_", anio_pronostico))
)

# print results
errores_totales <- bind_rows(errores_2017, errores_2018, errores_2019)

datatable(errores_totales, options = list(pageLength = 5, scrollX = TRUE)) %>%
  formatStyle(
    columns = names(errores_totales),
    color = "black",
    backgroundColor = "lightgray"
  )
