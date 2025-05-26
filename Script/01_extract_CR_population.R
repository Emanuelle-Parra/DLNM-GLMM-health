# 1. Load Required Libraries --------------------------------------------------

library(stringr)    #' String manipulation
library(ncdf4)      #' Read/write NetCDF files
library(lubridate)  #' Date/time parsing
library(reshape2)   #' Data reshaping (legacy)
library(dplyr)      #' Data manipulation
library(tidyr)      #' Data structure transformation
library(readxl)     #' Read Excel files
library(ggplot2)    #' Data visualization
library(sf)         #' Spatial data manipulation


# 2. Read Population Data from Excel ------------------------------------------

population_datos <- read_xlsx(
  "./Censo_2011_C.xlsx",
  range = "A1:C486",
  col_names = TRUE,
  .name_repair = "minimal"
)

length(population_datos$Código) #' Number of districts


# 3. Define Population Projection Parameters -----------------------------------

start_year <- 2000
end_year <- 2020
ajuste_poblacion_inicial <- 3810000
ajuste_poblacion_final <- 5128000

tasa_crecimiento_promedio <- (ajuste_poblacion_final / ajuste_poblacion_inicial)^(1 / (end_year - start_year + 1)) - 1


# 4. Create Weekly Date Sequence ----------------------------------------------

dates <- seq.Date(
  from = as.Date(paste0(start_year, "/01/03")),
  to = as.Date(paste0(end_year, "/12/31")),
  by = "week"
)


# 5. Project Weekly Population per District -----------------------------------

projection_df <- data.frame(Date = dates)

for (i in seq_len(nrow(population_datos))) {
  codigo <- population_datos$Código[i]
  poblacion_2011 <- population_datos$`Población total`[i]
  
  if (is.na(poblacion_2011)) {
    poblacion_2011 <- 0
  }
  
  years <- seq(start_year, end_year)
  tasa_crecimiento <- (1 + tasa_crecimiento_promedio)^(years - start_year) - 0.13
  poblacion_anual <- poblacion_2011 * tasa_crecimiento
  
  dates_yearly <- seq.Date(
    from = as.Date(paste0(start_year, "-01-03")),
    to = as.Date(paste0(end_year, "-12-31")),
    by = "year"
  )
  
  poblacion_semanal <- approx(
    x = dates_yearly,
    y = poblacion_anual,
    xout = dates,
    method = "linear",
    rule = 2
  )$y
  
  projection_df[[paste0("Poblacion_", codigo)]] <- poblacion_semanal
}


# 6. Finalize and Reorder Columns ---------------------------------------------

projection_df$Year <- year(projection_df$Date)

columnas <- colnames(projection_df)
nuevo_orden <- c("Date", "Year", columnas[!columnas %in% c("Date", "Year")])
projection_df <- projection_df[, nuevo_orden]


# 7. Rename Columns and Join with Subregions ----------------------------------

load(file = "./.RData") #' Load `list_distritos`

colnames(projection_df)[-c(1, 2)] <- list_distritos$cod_distrito

population_subregion_semana <- projection_df %>%
  pivot_longer(cols = "Year":"7065", names_to = "cod_distrito", values_to = "población") %>%
  left_join(list_distritos, by = "cod_distrito") %>%
  group_by(Date, Subreg_climat) %>%
  summarise(population_subregion = sum(población), .groups = "drop") %>%
  na.omit()

population_subregion_semana <- population_subregion_semana %>%
  mutate(
    year = epiyear(Date),
    epi.week = epiweek(Date),
    month = month(Date)
  ) %>%
  group_by(Subreg_climat, year, epi.week) %>%
  summarise(population = sum(population_subregion), .groups = "drop") %>%
  mutate(date = aweek::get_date(week = epi.week, year = year))


# 8. Aggregate Total Population -----------------------------------------------

population_totals <- population_subregion_semana %>%
  group_by(year, epi.week) %>%
  summarise(population_total = sum(population), .groups = "drop")

population_subregion_semana <- population_subregion_semana %>%
  left_join(population_totals, by = c("year", "epi.week")) %>%
  mutate(population = round(population))


# 9. Load Hospitalization Data ------------------------------------------------

CR_datos <- read_xlsx(
  "CCSS Serie de egresos hospitalarios debido a enfermedades respiratorias por distrito.xlsx",
  range = "A3:RS9134",
  col_names = TRUE,
  .name_repair = "minimal"
)

head(CR_datos)

egresos_ts <- rowSums(CR_datos[, -1])
egresos_ts_sin_desconocidos <- rowSums(CR_datos[, -c(1, 2)])

fechas <- CR_datos$Fecha
distritos <- names(CR_datos)
fechas_true <- seq(as.Date("1997/1/1"), as.Date("2021/12/31"), by = "day")

length(fechas_true)
length(fechas)

data <- data.frame(fechas, egresos_ts)

data %>%
  ggplot(aes(x = fechas, y = egresos_ts)) +
  geom_line() +
  geom_point()


# 10. Read Subregion Classifications ------------------------------------------

list_distritos <- read_xlsx("Distritos_Clasificados_final.xlsx", col_names = TRUE)

all.equal(distritos[-c(1, 2)], list_distritos$distrito)

colnames(CR_datos)[-c(1, 2)] <- list_distritos$cod_distrito


# 11. Convert Hospitalization Data to Long Format -----------------------------

egresos_subregion_dia <- CR_datos %>%
  pivot_longer(cols = "Desconocido":"7065", names_to = "cod_distrito", values_to = "egreso") %>%
  left_join(list_distritos, by = "cod_distrito") %>%
  group_by(Fecha, Subreg_climat) %>%
  summarise(egreso_subregion = sum(egreso), .groups = "drop") %>%
  na.omit()


# 12. Weekly Aggregation of Hospitalizations ----------------------------------

egresos_subregion_dia <- egresos_subregion_dia %>%
  mutate(
    year = epiyear(Fecha),
    epi.week = epiweek(Fecha),
    month = month(Fecha)
  )

egresos_subregion_semana <- egresos_subregion_dia %>%
  group_by(Subreg_climat, year, epi.week) %>%
  summarise(egreso = sum(egreso_subregion), .groups = "drop") %>%
  mutate(date = aweek::get_date(week = epi.week, year = year))

egre_totals <- egresos_subregion_semana %>%
  group_by(year, epi.week) %>%
  summarise(egreso_CR_semana_t = sum(egreso), .groups = "drop")

egresos_subregion_semana <- egresos_subregion_semana %>%
  left_join(egre_totals, by = c("year", "epi.week")) %>%
  arrange(date)

egresos_subregion_semana %>%
  ggplot(aes(x = date, y = egreso_CR_semana_t)) +
  geom_line() +
  geom_point()


# 13. Join Population and Compute Risk Metrics --------------------------------

load(file = "./population.RData")

egresos_filtrados <- egresos_subregion_semana %>%
  filter(date %in% population_subregion_semana$date)

Hospitalizaciones_final <- egresos_filtrados %>%
  left_join(
    population_subregion_semana,
    by = c("Subreg_climat", "date", "year", "epi.week")
  ) %>%
  mutate(
    Riesgo_relativ_contagio = (egreso / population) /
      (egreso_CR_semana_t / population_total)
  )


# 14. Save Final Dataset ------------------------------------------------------

save(
  egresos_subregion_semana,
  Hospitalizaciones_final,
  file = "Hospitalizaciones_final_CR.RData"
)
