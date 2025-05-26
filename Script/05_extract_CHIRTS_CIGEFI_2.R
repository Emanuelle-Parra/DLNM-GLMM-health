# 1. Load Required Libraries --------------------------------------------------

library(dplyr)       #' Data manipulation
library(tidync)      #' Work with NetCDF files as tidy data
library(lubridate)   #' Date and time manipulation
library(sf)          #' Spatial feature handling
library(raster)      #' Raster file handling
library(geodata)     #' Access GADM and other global datasets
library(sp)          #' Spatial data (legacy)

# 2. Load CHIRTS Daily Temperature Data ---------------------------------------

#' ------------------------------------------------------------------------------
#' Temperature data were obtained from the Climate Hazards Group InfraRed Temperature 
#' with Stations (CHIRTS) database of the University of California, Santa Barbara.
#'
#' These data provide long-term temperature records combining satellite and station 
#' information, and are available as monthly and daily gridded datasets.
#'
#' The CHIRTS dataset used here corresponds to maximum temperature estimates from 1950 
#' to 2020, and can be downloaded from:
#' https://www.chc.ucsb.edu/data/chirtsmonthly
#' ------------------------------------------------------------------------------


TMAX <- tidync("./tmax_estimated_1950-2020.nc") %>%
  hyper_tibble() #' Load and flatten Tmax data

TMIN <- tidync("./tmin_estimated_1950-2020.nc") %>%
  hyper_tibble() #' Load and flatten Tmin data

# 3. Join and Format Dataset --------------------------------------------------

datos <- TMAX %>%
  left_join(TMIN, by = c("longitude", "latitude", "time")) #' Join by coordinates and time
rm(TMAX, TMIN)

datos <- datos %>%
  group_by(longitude, latitude) %>%
  mutate(tt = seq_len(n())) %>%
  ungroup() %>%
  mutate(
    year = floor(1950 + (tt - 1) / 365),
    day = ave(tt, longitude, latitude, year, FUN = seq_along)
  )

datos <- datos %>%
  mutate(
    date = parse_date_time(paste(year, day), orders = "yj"),
    week = epiweek(date),
    amplitude = Tmax - Tmin
  ) %>%
  filter(year >= 2000)


# 4. Compute Weekly Metrics ---------------------------------------------------

datos <- datos %>%
  group_by(longitude, latitude, year, week) %>%
  summarise(
    Tmax_max = max(Tmax),
    Tmax_mean = mean(Tmax),
    Tmax_min = min(Tmax),
    n_Tmax_Q3 = sum(Tmax > 32.82),
    Tmin_min = min(Tmin),
    Tmin_mean = mean(Tmin),
    Tmin_max = max(Tmin),
    n_Tmin_Q1 = sum(Tmin < 22.67),
    amplitude_max = max(amplitude),
    amplitude_mean = mean(amplitude),
    amplitude_min = min(amplitude),
    n_amplitude_Q3 = sum(amplitude > 7.16),
    n_amplitude_P90 = sum(amplitude > 8.496),
    .groups = "drop"
  ) %>%
  mutate(date = aweek::get_date(week = week, year = year))


# 5. Save CHIRTS Weekly Data --------------------------------------------------

save(datos, file = "./Data/CHIRTS_CIGEFI_week.RData")


# 6. Regional Summary ---------------------------------------------------------

load("CHIRTS_CIGEFI_week.RData")
load("subregiones_geom.RData")

#' Load Costa Rica level-1 administrative boundaries
aut <- gadm("GADM", country = "CR", level = 1) %>%
  st_as_sf()

#' Plot administrative boundaries
ggplot() + geom_sf(data = aut)

#' Create spatial grid within Costa Rica
grid <- aut %>%
  st_make_grid(cellsize = 0.1, what = "centers") %>%
  st_intersection(aut)

#' Extract coordinates
ubicaciones <- unlist(grid)
lon_CR <- ubicaciones[ubicaciones < 0]
lat_CR <- ubicaciones[ubicaciones > 0]
lonlat <- data.frame(lon = lon_CR, lat = lat_CR)

#' Convert to sf object
lonlat_sf <- st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)


# 7. Join Spatial Coordinates with Climate Data -------------------------------

datos_space <- datos %>%
  filter(date == datos$date[1]) %>%
  select(longitude, latitude)

datos_CHIRTS_sf <- st_as_sf(
  datos_space,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

#' Create polygon around Costa Rica points
poligono_CR <- st_convex_hull(st_union(lonlat_sf))

#' Filter points inside Costa Rica
datos_CHIRTS_CR_sf <- st_intersection(datos_CHIRTS_sf, poligono_CR)

#' Load regional geometry (subregions)
load("regiones_geom.RData")

#' Join spatial points to subregions
puntos_sf_joined <- st_join(datos_CHIRTS_CR_sf, union_sf)

datos_space_joined <- as.data.frame(puntos_sf_joined) %>%
  select(longitude, latitude, Subreg_climat)

#' Merge subregion info into CHIRTS dataset
datos <- datos %>%
  left_join(datos_space_joined, by = c("longitude", "latitude")) %>%
  na.omit()


# 8. Aggregate Weekly Regional Temperature -------------------------------------

datos_CHIRTS_subreg <- datos %>%
  group_by(year, week, date, Subreg_climat) %>%
  summarise(Temp_media = mean((Tmax_max + Tmin_min) / 2), .groups = "drop") %>%
  arrange(date)


# 9. Save Final Regional Dataset ----------------------------------------------

save(
  datos_CHIRTS_subreg,
  datos,
  file = "CHIRTS_CIGEFI_week_Subregion.RData"
)
