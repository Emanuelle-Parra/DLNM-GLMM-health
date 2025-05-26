# 1. Load Required Libraries --------------------------------------------------

library(dplyr) #' Data manipulation


# 2. Load Processed Datasets --------------------------------------------------

load("datos_CHIRPS_subreg.RData")          #' Weekly precipitation by subregion
rm(datos)                                  #' Remove redundant variable if exists

load("Hospitalizaciones_final_CR.RData")   #' Hospital discharges + population
load("CHIRTS_CIGEFI_week_Subregion.RData") #' Weekly temperature by subregion
load("Aereosol_subregion_final.RData")     #' Weekly aerosol levels by subregion
load("RH_Subreg_final.RData")              #' Weekly relative humidity by subregion


# 3. Adjust Relative Humidity Format ------------------------------------------

datos_HR_finales <- datos_HR_finales %>%
  mutate(year = as.numeric(year)) #' Ensure year is numeric


# 4. Merge All Variables by Subregion and Week --------------------------------

datos_finales_subregion <- Hospitalizaciones_final %>%
  filter(year >= 2000 & year <= 2019) %>%
  left_join(
    datos_CHIRPS_subreg,
    by = c(
      "Subreg_climat" = "Subreg_climat",
      "date" = "date",
      "year" = "year",
      "epi.week" = "week"
    )
  ) %>%
  left_join(
    datos_CHIRTS_subreg,
    by = c(
      "Subreg_climat" = "Subreg_climat",
      "date" = "date",
      "year" = "year",
      "epi.week" = "week"
    )
  ) %>%
  left_join(
    datos_aerosol,
    by = c(
      "Subreg_climat" = "Subregion_climatica",
      "year" = "year",
      "epi.week" = "week"
    )
  ) %>%
  left_join(
    datos_HR_finales,
    by = c(
      "Subreg_climat" = "Subregion_climatica",
      "year" = "year",
      "epi.week" = "week"
    )
  ) %>%
  mutate(
    reg_climat = gsub("\\d", "", Subreg_climat) #' Extract region code without digits
  ) %>%
  select(reg_climat, Subreg_climat, year, epi.week, date, everything()) %>%
  rename(
    egreso_semana = egreso,
    pobl_subreg_ = population,
    pobl_CR = population_total
  )


# 5. Save Final Analysis Dataset ----------------------------------------------

save(datos_finales_subregion, file = "datos_final1_para_analizar.RData")

