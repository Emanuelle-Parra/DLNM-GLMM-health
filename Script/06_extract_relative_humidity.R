# 1. Install and Load Required Libraries --------------------------------------

install.packages(c("ncdf4", "dplyr", "sf", "ggplot2"))

library(ncdf4)      #' Read NetCDF files
library(tidyverse)  #' Core data manipulation
library(sf)         #' Spatial data handling
library(geodata)    #' Access to GADM boundaries
library(readxl)     #' Read Excel files
library(dplyr)      #' Data wrangling
library(sp)         #' Legacy spatial classes
library(raster)     #' Raster data access


# 2. Load GADM Level 1 and Create Spatial Grid --------------------------------

aut <- gadm("GADM", country = "CR", level = 1) %>%
  st_as_sf() #' Load and convert admin boundaries of Costa Rica

ggplot() + geom_sf(data = aut) #' Quick plot of regions

grid <- aut %>%
  st_make_grid(cellsize = 0.1, what = "centers") %>%
  st_intersection(aut) #' Spatial grid within country borders

ubicaciones <- unlist(grid)
lon_CR <- ubicaciones[ubicaciones < 0]
lat_CR <- ubicaciones[ubicaciones > 0]

lonlat <- data.frame(lon = lon_CR, lat = lat_CR)

dates <- c("2000-01-03", "2005-05-31")

save(dates, lonlat, file = "localizacionesHR.RData") #' Save coordinates and dates


# 3. Read and Process NetCDF RH Files -----------------------------------------

#' ------------------------------------------------------------------------------
#' Relative humidity observations were derived from the MERRA-2 reanalysis by the
#' National Aeronautics and Space Administration (NASA), provided by the Global 
#' Modeling and Assimilation Office (GMAO) (2020). 
#'
#' Data were extracted from the variable `RH` (relative humidity), available in the
#' MERRA-2 product M2T3NVCLD, with hourly resolution. The files are downloadable at:
#' https://disc.gsfc.nasa.gov/datasets/M2T3NVCLD_5.12.4/summary
#'
#' This script processes daily NetCDF files, computes vertical and temporal 
#' averages, crops the data to Costa Rica’s spatial domain, and generates a weekly 
#' average dataset by subregion.
#' ------------------------------------------------------------------------------


carpeta <- "./Humedad Relativa/"
archivos_nc <- list.files(path = carpeta, pattern = "\\.nc$", full.names = TRUE) %>%
  sort()

lista_datos <- list()

for (ruta_archivo in archivos_nc) {
  nc <- nc_open(ruta_archivo)
  RH <- ncvar_get(nc, "RH")
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  nc_close(nc)
  
  indice_fecha <- which(archivos_nc == ruta_archivo)
  fecha <- format(as.Date("2000-01-03") + (indice_fecha - 1), "%Y-%m-%d")
  year <- format(as.Date(fecha), "%Y")
  day <- format(as.Date(fecha), "%e")
  
  promedio_RH <- apply(RH, c(1, 2), mean)
  
  lon_lat_df <- expand.grid(lon = lon, lat = lat)
  lon_lat_df$RH <- as.vector(promedio_RH)
  lon_lat_df$date <- fecha
  lon_lat_df$year <- year
  lon_lat_df$day <- day
  
  lista_datos[[length(lista_datos) + 1]] <- lon_lat_df
}

datosRH <- bind_rows(lista_datos)

save(datosRH, file = "datosRH.RData") #' Save merged RH data


# 4. Filter Points Inside Costa Rica Polygon ----------------------------------

datosRH_sf <- st_as_sf(datosRH, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
lonlat_sf <- st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)

poligono_CR <- st_convex_hull(st_union(lonlat_sf))

datosRH_CR_sf <- st_intersection(datosRH_sf, poligono_CR)
datosRH_CR <- as.data.frame(st_drop_geometry(datosRH_CR_sf))

write.csv(datosRH_CR, "datosRH_CR.csv", row.names = FALSE)


# 5. Compute Weighted Subregional RH ------------------------------------------

load("regiones_geom.RData") #' Load subregion geometries

points_RH_CR <- st_as_sf(data.frame(unique(datosRH_CR[, c("lon", "lat")])),
                         coords = c("lon", "lat"), crs = 4326
)

distancias_HR <- st_distance(centroides_sf, points_RH_CR)

coordenadas_puntos <- data.frame(st_coordinates(points_RH_CR)) %>%
  mutate(codigo_grilla = paste(X, Y, sep = ","))

distancias_HR <- data.frame(distancias_HR)
colnames(distancias_HR) <- coordenadas_puntos$codigo_grilla
rownames(distancias_HR) <- centroides_sf$Subreg_climat

distancias_HR <- data.frame(t(distancias_HR)) %>%
  mutate(codigo_grilla = coordenadas_puntos$codigo_grilla)

colnames(distancias_HR)[-ncol(distancias_HR)] <- centroides_sf$Subreg_climat


# 6. Transform Distances to Long Format ---------------------------------------

distancias_HR_long <- distancias_HR %>%
  pivot_longer(
    cols = PC1:VC3,
    names_to = "Subregion_climatica",
    values_to = "distancia_HR"
  )


# 7. Prepare RH Data for Spatial Aggregation ----------------------------------

datos_HR_CR <- datosRH_CR %>%
  mutate(week = epiweek(date)) %>%
  group_by(year, week, lat, lon) %>%
  summarise(RH = mean(RH, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    date = aweek::get_date(week = week, year = year),
    codigo_grilla = paste(lon, lat, sep = ",")
  )

Subregion_climatica <- unique(distancias_HR_long$Subregion_climatica)


# 8. Define Aggregation Function by Subregion ---------------------------------

cambia_puntos_subregiones <- function(bloque) {
  bloque_p <- NULL
  for (i in seq_along(Subregion_climatica)) {
    distancias_long_pba <- distancias_HR_long %>%
      filter(Subregion_climatica == Subregion_climatica[i])
    
    bloque_c <- bloque %>%
      left_join(distancias_long_pba, by = "codigo_grilla") %>%
      mutate(distancia2 = RH * distancia_HR / sum(distancia_HR, na.rm = TRUE)) %>%
      select(year, week, Subregion_climatica, distancia2) %>%
      group_by(Subregion_climatica) %>%
      summarise(distancia2 = sum(distancia2, na.rm = TRUE), .groups = "drop") %>%
      distinct()
    
    bloque_p <- bind_rows(bloque_p, bloque_c)
  }
  return(bloque_p)
}


# 9. Apply Function Week by Week ----------------------------------------------

fechas_unicas <- datos_HR_CR %>%
  select(year, week) %>%
  distinct()

bloques_subregiones <- NULL

for (i in seq_len(nrow(fechas_unicas))) {
  datos_HR_t <- datos_HR_CR %>%
    filter(
      year == fechas_unicas$year[i],
      week == fechas_unicas$week[i]
    )
  
  bloques_subregiones_t <- cambia_puntos_subregiones(datos_HR_t)
  bloques_subregiones <- bind_rows(bloques_subregiones, bloques_subregiones_t)
}


# 10. Finalize and Save RH Output ---------------------------------------------

datos_HR_finales <- bloques_subregiones %>%
  rename(RH = distancia2)

datos_HR_dist <- datos_HR_finales

save(datos_HR_finales, file = "RH_Subreg_final.RData")


