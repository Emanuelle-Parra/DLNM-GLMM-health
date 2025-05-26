# 1. Load Required Libraries --------------------------------------------------

install.packages("geodata") # (optional: run only once)

library(chirps)      #' CHIRPS climate data access
library(sf)          #' Spatial feature manipulation
library(raster)      #' Raster file handling
library(ggplot2)     #' Data visualization
library(tidyverse)   #' Core data science packages
library(lubridate)   #' Date and time handling
library(dplyr)       #' Data manipulation
library(terra)       #' Alternative to raster/sp library for modern spatial data


# 2. Download and Load Administrative Boundaries ------------------------------

aut <- geodata::gadm(
  country = "CRI",
  level = 1,
  path = tempdir()
) #' Download GADM level 1 shapefile for Costa Rica

aut <- st_as_sf(aut) #' Convert to sf object for spatial operations


# 3. Visualize Administrative Regions -----------------------------------------

ggplot() +
  geom_sf(data = aut) #' Plot administrative boundaries of Costa Rica


# 4. Generate Regular Grid over Costa Rica ------------------------------------

grid <- aut %>%
  st_make_grid(cellsize = 0.1, what = "centers") %>%
  st_intersection(aut) #' Generate grid points within Costa Rica


# 5. Extract Grid Point Coordinates -------------------------------------------

ubicaciones <- unlist(grid)                 #' Flatten coordinates
lon_CR <- ubicaciones[ubicaciones < 0]     #' Longitudes (negative)
lat_CR <- ubicaciones[ubicaciones > 0]     #' Latitudes (positive)

lonlat <- data.frame(lon = lon_CR, lat = lat_CR) #' Build coordinate data frame


# 6. Define Date Range and Save Coordinates -----------------------------------

dates <- c("1981-01-02", "2022-02-28") #' Start and end date

save(dates, lonlat, file = "localizacionesCHIRPS.RData") #' Save metadata


# 7. Load Example COG File and Extract Raster ---------------------------------

#' ------------------------------------------------------------------------------
#' Precipitation data were obtained from the Climate Hazards Group InfraRed Precipitation 
#' with Station data (CHIRPS) database of the University of California, Santa Barbara.
#'
#' These data provide daily rainfall estimates at a spatial resolution of ~0.05° from 1981 onward, 
#' and are particularly suited for climate analysis in data-scarce regions.
#'
#' The dataset is publicly available and can be downloaded from:
#' https://developers.google.com/earth-engine/datasets/catalog/UCSB-CHG_CHIRPS_DAILY?hl=es-419
#'
#' This script reads .cog files (Cloud Optimized GeoTIFF) corresponding to daily precipitation, 
#' extracts values over Costa Rica, and computes weekly or monthly summaries by climatic subregion.
#' ------------------------------------------------------------------------------

file_path <- "chirps-v2.0.1993.08.12.cog"
raster_data <- raster(file_path) #' Load CHIRPS COG raster file


# 8. Download CHIRPS Precipitation Data ---------------------------------------

dat_max <- get_chirps(lonlat, dates, server = "CHC", operation = 0) #' Max precipitation
dat_min <- get_chirps(lonlat, dates, server = "CHC", operation = 1) #' Min precipitation

head(dat_max) #' Preview first rows of maximum precipitation data


# 9. Rename Columns and Join Precipitation Data -------------------------------

dat_max <- dat_max %>% rename(prep_max = chirps)
dat_min <- dat_min %>% rename(prep_min = chirps)

datos <- dat_max %>%
  left_join(dat_min, by = c("id", "lon", "lat", "date")) #' Merge datasets


# 10. Load Additional Geometries ----------------------------------------------

load(file = "datosPrecCHIRPS.RData")
load(file = "./subregiones_geom.RData")


# 11. Preprocess CHIRPS Data --------------------------------------------------

datos$prep_min[datos$prep_min == -9999] <- 0 #' Replace missing values
datos$prep_max[datos$prep_max == -9999] <- 0

datos_CHIRPS <- datos %>%
  mutate(
    year = year(date),
    week = epiweek(date)
  ) %>%
  group_by(lon, lat, year, week) %>%
  summarise(
    prep_max = max(prep_max),
    prep_min = min(prep_min),
    .groups = "drop"
  )


# 12. Filter a Single Week and Create Spatial Points --------------------------

datos_space <- datos_CHIRPS %>%
  filter(
    year == datos_CHIRPS$year[1],
    week == datos_CHIRPS$week[1]
  ) %>%
  select(lon, lat)

puntos <- st_as_sf(
  datos_space,
  remove = FALSE,
  coords = c("lon", "lat"),
  crs = 4326,
  agr = "constant"
)


# 13. Spatial Join with Regional Geometry -------------------------------------

puntos_sf_joined <- st_join(puntos, union_sf)

datos_space_joined <- as.data.frame(puntos_sf_joined) %>%
  select(lon, lat, Subreg_climat)


# 14. Join Region Labels to CHIRPS Data ---------------------------------------

datos_CHIRPS <- datos_CHIRPS %>%
  left_join(datos_space_joined, by = c("lon", "lat")) %>%
  na.omit()


# 15. Recreate Date and Aggregate by Subregion --------------------------------

datos_CHIRPS <- datos_CHIRPS %>%
  mutate(date = aweek::get_date(week = week, year = year))

datos_CHIRPS_subreg <- datos_CHIRPS %>%
  group_by(year, week, date, Subreg_climat) %>%
  summarise(precip_media = mean((prep_max + prep_min) / 2), .groups = "drop") %>%
  arrange(date)


# 16. Save Final Processed Dataset --------------------------------------------

save(datos_CHIRPS_subreg, file = "datos_CHIRPS_subreg.RData")
