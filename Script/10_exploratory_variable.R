# 1. Load Required Libraries --------------------------------------------------

library(car)         #' Linear model diagnostics
library(corrplot)    #' Correlation plots and tests
library(dlnm)        #' Nonlinear distributed lag models
library(dplyr)       #' Data manipulation
library(energy)      #' Distance correlation
library(forecast)    #' Time series forecasting
library(GGally)      #' ggplot2 correlation matrices
library(ggplot2)     #' Grammar of graphics
library(ggseas)      #' Seasonal decomposition for ggplot
library(glmmTMB)     #' Generalized linear mixed models
library(infotheo)    #' Mutual information
library(kableExtra)  #' HTML and LaTeX tables
library(lme4)        #' Mixed effects models
library(MASS)        #' GLMs and distributions
library(Metrics)     #' Model evaluation metrics
library(mgcv)        #' Generalized additive models
library(scales)      #' Custom axis scales
library(sf)          #' Spatial feature handling
library(splines)     #' Spline basis generation
library(tidyverse)   #' Tidy data tools
library(tseries)     #' Time series analysis
library(urca)        #' Unit root tests
library(viridis)     #' Color palettes
library(DT)          #' Interactive tables


# 2. Load Data ----------------------------------------------------------------

load("datos_final1_para_analizar.RData")             #' Final weekly dataset
subregion_st <- st_read("Subegiones Climáticas.shp") #' Subregional shapefile


# 3. Aggregate Annual Indicators ----------------------------------------------

annual_data <- datos_finales_subregion %>%
  group_by(year, reg_climat) %>%
  summarise(
    egreso_anual = sum(egreso_semana),
    precip_media_anual = mean(precip_media),
    Temp_media_anual = mean(Temp_media),
    Tasa_egresos = sum(egreso_semana) / sum(pobl_subreg_),
    aerosol_anual = mean(aerosol),
    RH_anual = mean(RH),
    riesgo_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    .groups = "drop"
  )

annual_data2 <- datos_finales_subregion %>%
  group_by(year, reg_climat, Subreg_climat) %>%
  summarise(
    egreso_anual = sum(egreso_semana),
    precip_media_anual = mean(precip_media),
    Temp_media_anual = mean(Temp_media),
    aerosol_anual = mean(aerosol),
    RH_anual = mean(RH),
    Pop = sum(pobl_subreg_),
    tasa_egreso = sum(egreso_semana) / sum(pobl_subreg_),
    riesgo_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    .groups = "drop"
  )


# 4. Plot Annual Hospital Discharges and Risk ---------------------------------

ggplot(annual_data) +
  geom_line(aes(x = year, y = egreso_anual, color = reg_climat)) +
  labs(x = "Year", y = "Annual hospital discharges") +
  scale_color_discrete(name = "Climatic region") +
  theme(legend.position = "none") +
  theme_bw()

ggplot(annual_data) +
  geom_line(aes(x = year, y = riesgo_contagio, color = reg_climat)) +
  labs(x = "Year", y = "Relative risk of hospital discharges") +
  scale_color_discrete(name = "Climatic region") +
  theme_bw()

ggplot(annual_data) +
  geom_line(aes(x = year, y = Tasa_egresos, color = reg_climat)) +
  labs(x = "Year", y = "Hospital discharge rate") +
  scale_color_discrete(name = "Climatic region") +
  theme_bw()


# 5. Compute Subregion Label Coordinates --------------------------------------

projected_crs <- 32616 #' UTM Zone 16N

subregion_st_projected <- st_transform(subregion_st, crs = projected_crs)

subregion_labels_projected <- subregion_st_projected %>%
  st_point_on_surface() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(x = X, y = Y) %>%
  mutate(NOMBRE = subregion_st_projected$NOMBRE)

subregion_labels <- st_as_sf(subregion_labels_projected, coords = c("x", "y"), crs = projected_crs) %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(x = X, y = Y) %>%
  mutate(NOMBRE = subregion_labels_projected$NOMBRE)

# 6. Spatial Maps by Subregion ----------------------------------------

#' Plot climatic subregions with subregion labels and geographic axes
ggplot(data = subregion_st) +
  geom_sf(aes(fill = as.factor(SUBREG))) +
  geom_text(
    data = subregion_labels,
    aes(x = x, y = y, label = NOMBRE),
    size = 3,
    color = "black",
    check_overlap = TRUE
  ) +
  labs(
    fill = "Climatic subregions",
    x = "Longitude (degrees)",
    y = "Latitude (degrees)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray85"),
    panel.background = element_rect(fill = "white")
  )

#' Annual Spatial Maps by Subregion 
map_data_egre_reg_anual <- subregion_st %>%
  left_join(
    annual_data2 %>%
      group_by(Subreg_climat) %>%
      summarise(
        discharges = sum(egreso_anual),
        relative_risk = sum(riesgo_contagio * Pop) / sum(Pop),
        discharge_rate = sum(egreso_anual) / sum(Pop),
        precip_mean = mean(precip_media_anual),
        temp_mean = mean(Temp_media_anual),
        aerosol_mean = mean(aerosol_anual),
        rh_mean = mean(RH_anual),
        .groups = "drop"
      ),
    by = c("NOMBRE" = "Subreg_climat")
  )

#' Hospital discharges per subregion
ggplot(map_data_egre_reg_anual) +
  geom_sf(aes(fill = discharges), color = "white") +
  scale_fill_viridis_c(name = "Total discharges") +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()

#' Discharge rate per subregion
ggplot(map_data_egre_reg_anual) +
  geom_sf(aes(fill = discharge_rate), color = "white") +
  scale_fill_viridis_c(name = "Discharge rate") +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()

#' Relative risk of hospital discharges
ggplot(map_data_egre_reg_anual) +
  geom_sf(aes(fill = relative_risk), color = "white") +
  scale_fill_viridis_c(
    limits = c(0, max(map_data_egre_reg_anual$relative_risk)),
    breaks = c(0, 1, max(map_data_egre_reg_anual$relative_risk)),
    name = "Relative risk"
  ) +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()


# 7. Correlation Analysis by Year ---------------------------------------------

calcular_correlaciones_por_año <- function(data, year_especifico) {
  data %>%
    filter(year == year_especifico) %>%
    dplyr::select(egreso_semana, Riesgo_relativ_contagio, precip_media, Temp_media, aerosol, RH) %>%
    cor(use = "complete.obs")
}

mayor_correlacion_con_egreso_y_riesgo <- function(data) {
  years <- unique(data$year)
  correlation_sums <- numeric(length(years))
  
  for (i in seq_along(years)) {
    corr <- calcular_correlaciones_por_año(data, years[i])
    correlation_sums[i] <- sum(abs(corr["egreso_semana", -c(1, 2)])) +
      sum(abs(corr["Riesgo_relativ_contagio", -c(1, 2)]))
  }
  
  best_year <- years[which.max(correlation_sums)]
  best_corr <- calcular_correlaciones_por_año(data, best_year)
  
  print(paste("Year with highest correlation:", best_year))
  print("Correlation matrix:")
  print(best_corr)
  
  return(list(best_year = best_year, correlation_matrix = best_corr))
}

resultado <- mayor_correlacion_con_egreso_y_riesgo(datos_finales_subregion)

# 8. Generate Maps for Best Year ----------------------------------------------

datos_2014 <- datos_finales_subregion %>% filter(year == 2014)

subregion_st_projected <- st_transform(subregion_st, crs = 32616)

subregion_labels_projected <- subregion_st_projected %>%
  st_point_on_surface() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(x = X, y = Y) %>%
  mutate(NOMBRE = subregion_st_projected$NOMBRE)

subregion_labels <- st_as_sf(subregion_labels_projected, coords = c("x", "y"), crs = 32616) %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(x = X, y = Y) %>%
  mutate(NOMBRE = subregion_labels_projected$NOMBRE)

map_data_egre_reg <- subregion_st %>%
  left_join(
    datos_2014 %>%
      group_by(Subreg_climat) %>%
      summarise(
        discharges = sum(egreso_semana),
        relative_risk = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
        precip_mean = mean(precip_media),
        temp_mean = mean(Temp_media),
        aerosol = mean(aerosol),
        rh = mean(RH),
        .groups = "drop"
      ),
    by = c("NOMBRE" = "Subreg_climat")
  )

#' Hospital discharges (2014)
ggplot(map_data_egre_reg) +
  geom_sf(aes(fill = discharges), color = "white") +
  scale_fill_viridis_c(name = "Discharges (2014)") +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()

#' Relative risk (2014)
ggplot(map_data_egre_reg) +
  geom_sf(aes(fill = relative_risk), color = "white") +
  scale_fill_viridis_c(
    limits = c(0, max(map_data_egre_reg$relative_risk)),
    breaks = c(0, 1, max(map_data_egre_reg$relative_risk)),
    name = "Relative risk (2014)"
  ) +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()

#' Mean temperature (2014)
ggplot(map_data_egre_reg) +
  geom_sf(aes(fill = temp_mean), color = "white") +
  scale_fill_viridis_c(name = "Mean Temp. (°C)") +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()

#' Mean precipitation (2014)
ggplot(map_data_egre_reg) +
  geom_sf(aes(fill = precip_mean), color = "white") +
  scale_fill_viridis_c(name = "Mean Precip. (mm)") +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()

#' Relative humidity (2014)
ggplot(map_data_egre_reg) +
  geom_sf(aes(fill = rh), color = "white") +
  scale_fill_viridis_c(name = "Relative Humidity (%)") +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()

#' Aerosol concentration (2014)
ggplot(map_data_egre_reg) +
  geom_sf(aes(fill = aerosol), color = "white") +
  scale_fill_viridis_c(name = "AOD (Aerosol Optical Depth)") +
  geom_text(data = subregion_labels, aes(x = x, y = y, label = NOMBRE),
            size = 3, color = "black", check_overlap = TRUE) +
  theme_void() +
  theme_bw()

# 9. Prepare Weekly Aggregated Data by Region and Subregion -------------------

#' Pacific Central (PC) - Subregional
data_PC_subreg <- datos_finales_subregion %>%
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

#' Pacific Central (PC) - Regional
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

#' Pacific North (PN)
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

data_PN_subreg <- datos_finales_subregion %>%
  filter(reg_climat == "PN", epi.week != 53) %>%
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

#' Pacific South (PS)
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

data_PS_subreg <- datos_finales_subregion %>%
  filter(reg_climat == "PS", epi.week != 53) %>%
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

#' Mountain South (RMS)
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

data_RMS_subreg <- datos_finales_subregion %>%
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

#' Central Valley (VC)
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

data_VC_subreg <- datos_finales_subregion %>%
  filter(reg_climat == "VC", epi.week != 53) %>%
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

#' North (RN)
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

data_RN_subreg <- datos_finales_subregion %>%
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

#' Atlantic (RA)
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

data_RA_subreg <- datos_finales_subregion %>%
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

# 10. Combine Regional Data and Create Boxplots -------------------------------

#' Add region labels
data_region1$region <- "PC"
data_region2$region <- "PS"
data_region3$region <- "PN"
data_region4$region <- "RMS"
data_region5$region <- "VC"
data_region6$region <- "RN"
data_region7$region <- "RA"

#' Combine all regions
data_combined <- bind_rows(
  data_region1, data_region2, data_region3, data_region4,
  data_region5, data_region6, data_region7
)

#' Save individual region data if needed
Data_PC <- data_region1
Data_PS <- data_region2
Data_PN <- data_region3
Data_RMS <- data_region4
Data_VC <- data_region5
Data_RN <- data_region6
Data_RA <- data_region7


# Boxplots for hospital discharges and exposure variables ---------------------

#' Weekly hospital discharges by region
ggplot(data_combined, aes(x = region, y = egreso_semana, fill = region)) +
  geom_boxplot(fill = "#B3C6E7") +
  labs(x = "Region", y = "Weekly hospital discharges") +
  theme(legend.position = "none") +
  theme_bw()

#' Relative risk of hospital discharges by region
ggplot(data_combined, aes(x = region, y = Riesgo_relativ_contagio, fill = region)) +
  geom_boxplot(fill = "#B3C6E7") +
  labs(x = "Region", y = "Relative risk") +
  theme(legend.position = "none") +
  theme_bw()

#' Mean temperature by region
ggplot(data_combined, aes(x = region, y = Temp_media, fill = region)) +
  geom_boxplot(fill = "#B3C6E7") +
  labs(x = "Region", y = "Mean temperature (°C)") +
  theme(legend.position = "none") +
  theme_bw()

#' Mean precipitation by region
ggplot(data_combined, aes(x = region, y = precip_media, fill = region)) +
  geom_boxplot(fill = "#B3C6E7") +
  labs(x = "Region", y = "Mean precipitation (mm)") +
  theme(legend.position = "none") +
  theme_bw()

#' Aerosol optical depth index by region
ggplot(data_combined, aes(x = region, y = aerosol, fill = region)) +
  geom_boxplot(fill = "#B3C6E7") +
  labs(x = "Region", y = "AOD Index") +
  theme(legend.position = "none") +
  theme_bw()

#' Relative humidity by region
ggplot(data_combined, aes(x = region, y = RH, fill = region)) +
  geom_boxplot(fill = "#B3C6E7") +
  labs(x = "Region", y = "Relative humidity (%)") +
  theme(legend.position = "none") +
  theme_bw()


# 11. Create Weekly Time Series and Decomposition -----------------------------

#' National time series
egresos_ord <- datos_finales_subregion %>%
  group_by(year, epi.week) %>%
  summarise(
    egreso_semana = sum(egreso_semana),
    Riesgo_relativ_contagio = sum(Riesgo_relativ_contagio * pobl_subreg_) / sum(pobl_subreg_),
    precip_media = mean(precip_media),
    Temp_media = mean(Temp_media),
    aerosol = mean(aerosol),
    RH = mean(RH),
    .groups = "drop"
  )

start_year <- min(egresos_ord$year)
start_week <- min(egresos_ord$epi.week)

egresos_ts <- ts(egresos_ord$egreso_semana, start = c(start_year, start_week), frequency = 52)
riesgo_ts <- ts(egresos_ord$Riesgo_relativ_contagio, start = c(start_year, start_week), frequency = 52)

#' Plot time series
autoplot(egresos_ts) +
  labs(x = "Time", y = "Weekly hospital discharges")

#' Decomposition
decomposed_data <- decompose(egresos_ts)
autoplot(decomposed_data)

#' Seasonal pattern
ggtsdisplay(egresos_ts)

#' ADF test for stationarity
adf_test <- adf.test(egresos_ts)
cat("ADF test result:\n")
print(adf_test)

#' KPSS test
kpss_test <- ur.kpss(egresos_ts)
cat("\nKPSS test result:\n")
summary(kpss_test)


# 12. Create Regional Time Series ---------------------------------------------

egresos_sf_PC <- ts(Data_PC$egreso_semana, start = c(min(Data_PC$year), min(Data_PC$epi.week)), frequency = 52)
egresos_sf_PS <- ts(Data_PS$egreso_semana, start = c(min(Data_PS$year), min(Data_PS$epi.week)), frequency = 52)
egresos_sf_PN <- ts(Data_PN$egreso_semana, start = c(min(Data_PN$year), min(Data_PN$epi.week)), frequency = 52)
egresos_sf_RMS <- ts(Data_RMS$egreso_semana, start = c(min(Data_RMS$year), min(Data_RMS$epi.week)), frequency = 52)
egresos_sf_VC <- ts(Data_VC$egreso_semana, start = c(min(Data_VC$year), min(Data_VC$epi.week)), frequency = 52)
egresos_sf_RN <- ts(Data_RN$egreso_semana, start = c(min(Data_RN$year), min(Data_RN$epi.week)), frequency = 52)
egresos_sf_RA <- ts(Data_RA$egreso_semana, start = c(min(Data_RA$year), min(Data_RA$epi.week)), frequency = 52)
# 13. Regional Time Series: Hospital Discharges -------------------------------

#' Plot regional hospital discharges
autoplot(egresos_sf_PC) +
  labs(x = "Time", y = "Hospital discharges")

autoplot(egresos_sf_PS) +
  labs(x = "Time", y = "Hospital discharges")

autoplot(egresos_sf_PN) +
  labs(x = "Time", y = "Hospital discharges")

autoplot(egresos_sf_RMS) +
  labs(x = "Time", y = "Hospital discharges")

autoplot(egresos_sf_VC) +
  labs(x = "Time", y = "Hospital discharges")

autoplot(egresos_sf_RN) +
  labs(x = "Time", y = "Hospital discharges")

autoplot(egresos_sf_RA) +
  labs(x = "Time", y = "Hospital discharges")

#' Autocorrelation (ACF) for each region
ggAcf(egresos_sf_PC, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(egresos_sf_PS, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(egresos_sf_PN, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(egresos_sf_RMS, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(egresos_sf_VC, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(egresos_sf_RN, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(egresos_sf_RA, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")


# 14. National Relative Risk Series -------------------------------------------

#' Plot total weekly relative risk for all regions
autoplot(riesgo_ts) +
  labs(x = "Time", y = "Relative risk of hospital discharges")

#' Decomposition of risk time series
decomposed_data_riesg <- decompose(riesgo_ts)
autoplot(decomposed_data_riesg)

#' Display seasonal patterns
ggtsdisplay(riesgo_ts)

#' Augmented Dickey-Fuller (ADF) test
adf_test <- adf.test(riesgo_ts)
cat("ADF test result:\n")
print(adf_test)

#' KPSS test for stationarity
kpss_test <- ur.kpss(riesgo_ts)
cat("\nKPSS test result:\n")
summary(kpss_test)


# 15. Relative Risk Series by Region ------------------------------------------

#' Create relative risk time series by region
riesgos_sf_PC <- ts(Data_PC$Riesgo_relativ_contagio, start = c(min(Data_PC$year), min(Data_PC$epi.week)), frequency = 52)
riesgos_sf_PS <- ts(Data_PS$Riesgo_relativ_contagio, start = c(min(Data_PS$year), min(Data_PS$epi.week)), frequency = 52)
riesgos_sf_PN <- ts(Data_PN$Riesgo_relativ_contagio, start = c(min(Data_PN$year), min(Data_PN$epi.week)), frequency = 52)
riesgos_sf_RMS <- ts(Data_RMS$Riesgo_relativ_contagio, start = c(min(Data_RMS$year), min(Data_RMS$epi.week)), frequency = 52)
riesgos_sf_VC <- ts(Data_VC$Riesgo_relativ_contagio, start = c(min(Data_VC$year), min(Data_VC$epi.week)), frequency = 52)
riesgos_sf_RN <- ts(Data_RN$Riesgo_relativ_contagio, start = c(min(Data_RN$year), min(Data_RN$epi.week)), frequency = 52)
riesgos_sf_RA <- ts(Data_RA$Riesgo_relativ_contagio, start = c(min(Data_RA$year), min(Data_RA$epi.week)), frequency = 52)

#' Plot risk series by region
autoplot(riesgos_sf_PC) +
  labs(x = "Time", y = "Relative risk") +
  scale_y_continuous(breaks = seq(0, max(riesgos_sf_PC), by = 0.5))

autoplot(riesgos_sf_PS) +
  labs(x = "Time", y = "Relative risk") +
  scale_y_continuous(breaks = seq(0, max(riesgos_sf_PS), by = 0.5))

autoplot(riesgos_sf_PN) +
  labs(x = "Time", y = "Relative risk") +
  scale_y_continuous(breaks = seq(0, max(riesgos_sf_PN), by = 0.5))

autoplot(riesgos_sf_RMS) +
  labs(x = "Time", y = "Relative risk") +
  scale_y_continuous(breaks = seq(0, max(riesgos_sf_RMS), by = 0.5))

autoplot(riesgos_sf_VC) +
  labs(x = "Time", y = "Relative risk") +
  scale_y_continuous(breaks = seq(0, max(riesgos_sf_VC), by = 0.5))

autoplot(riesgos_sf_RN) +
  labs(x = "Time", y = "Relative risk") +
  scale_y_continuous(breaks = seq(0, max(riesgos_sf_RN), by = 0.5))

autoplot(riesgos_sf_RA) +
  labs(x = "Time", y = "Relative risk") +
  scale_y_continuous(breaks = seq(0, max(riesgos_sf_RA), by = 0.5))

#' Autocorrelation (ACF) of relative risk series
ggAcf(riesgos_sf_PC, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(riesgos_sf_PS, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(riesgos_sf_PN, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(riesgos_sf_RMS, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(riesgos_sf_VC, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(riesgos_sf_RN, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")
ggAcf(riesgos_sf_RA, lag.max = 156) + labs(x = "Lag", y = "Autocorrelation")

