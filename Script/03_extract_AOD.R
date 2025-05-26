# 1. Load Required Libraries --------------------------------------------------


library(fields)     #' Tools for spatial data, interpolation, and plotting
library(maps)       #' Base map data for spatial visualization
library(sp)         #' Classes and methods for spatial data
library(mapproj)    #' Map projections for base R maps
library(RNetCDF)    #' Interface to read and write NetCDF files
library(ggmap)      #' Visualization of spatial data with Google Maps and OpenStreetMap
library(reshape)    #' Reshaping data between wide and long formats (older than reshape2)
library(dplyr)      #' Data manipulation with the grammar of data transformation
library(stringr)    #' Consistent string operations
library(rlist)      #' Tools for manipulating list objects
library(terra)      #' Handling raster and vector spatial data
library(lubridate)  #' Simplified date-time manipulation
library(ncdf4)      #' Reading and writing NetCDF files (newer API)
library(reshape2)   #' Improved version of reshape for data transformation
library(tidyr)      #' Tidy data tools: pivoting, nesting, separating
library(readxl)     #' Import Excel spreadsheets into R
library(ggplot2)    #' Data visualization based on the Grammar of Graphics
library(sf)         #' Simple feature objects for spatial vector data

# 2. Database -----------------------------------------------------------------------

#' ------------------------------------------------------------------------------
#' The aerosol optical depth (AOD) index was obtained from the MOD08_D3 product 
#' of the MODIS sensor aboard NASA’s Terra and Aqua satellites (NASA LAADS-DAAC, 2020).
#' Data are publicly available at:
#' https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MOD08_D3#product-information
#'
#' This script processes HDF files containing daily mean AOD data, extracts the
#' layer Aerosol_Optical_Depth_Land_Ocean_Mean, crops the data to Costa Rica’s
#' spatial extent, applies the appropriate scale factor, and generates a final
#' dataset with latitude, longitude, AOD value, and date.
#' ------------------------------------------------------------------------------

# 3. Set Base Directory and Configuration (example for 2001) --------------------

dirbase <- "./Data/MOD08_D3/2001/"      #' Path to directory containing HDF files
year <- 2001                            #' Year of data being processed

#' List all HDF files in directory (including subfolders)
listfilesg <- list.files(
  path = dirbase,
  pattern = "*.hdf",
  recursive = TRUE
)

#' Define spatial extent for Costa Rica (longitude and latitude limits)
lon_lim <- c(-87, -82)
lat_lim <- c(8, 11)
CR_ext <- ext(-87, -82, 8, 11)          #' terra::ext object for cropping

# 4. Initialize Data Container ------------------------------------------------

data_list <- list()                    #' Empty list to store data frames

# 5. Loop Through Each File ---------------------------------------------------

for (i in seq_along(listfilesg)) {
  show(paste0("Processing file ", i))
  
  #' Read SDS layers from HDF file
  data_nc <- sds(paste0(dirbase, listfilesg[i]))
  
  #' Extract Aerosol Optical Depth (AOD) layer
  var <- data_nc$Aerosol_Optical_Depth_Land_Ocean_Mean
  
  #' Crop to Costa Rica extent
  var_CR <- crop(var, CR_ext)
  
  #' Extract 2D array for the first time slice (daily average)
  var_array <- as.array(var_CR)[, , 1]
  
  #' Extract Julian day from filename (first 3 characters)
  day <- str_sub(listfilesg[i], 1, 3)
  
  #' Get longitude and latitude coordinates from raster
  lon <- xFromCol(var_CR)
  lat <- yFromRow(var_CR)
  
  rm(data_nc, var)
  
  #' Set dimnames and melt array into long-format data frame
  dimnames(var_array) <- list(lat, lon)
  var_prep <- melt(var_array)
  colnames(var_prep) <- c("lat", "lon", "aerosol")
  
  #' Add year and Julian day
  data <- var_prep %>%
    mutate(
      year = as.numeric(year),
      day = as.numeric(day)
    )
  
  #' Append to list
  data_list[[i]] <- data
}

# 6. Combine and Format Aerosol Dataset ---------------------------------------

datos_aerosol <- list.rbind(data_list)  #' Combine all days

#' Apply scale factor (0.001) according to MOD08_D3 documentation
scale <- 0.001
datos_aerosol$aerosol <- datos_aerosol$aerosol * scale

#' Convert (year + Julian day) to proper Date format
datos_aerosol <- datos_aerosol %>%
  mutate(date = parse_date_time(paste(year, day), orders = "yj"))

# 7. Save Final Output --------------------------------------------------------

save(datos_aerosol, file = paste0("./Data/aerosol_MOD08_D3_", year, ".RData"))


# 8. Load and Merge Aerosol Data from RData Files -----------------------------

dirbase <- "./MOD08_D3/" #' Base directory for MOD08_D3 RData files

listfilesg <- list.files(
  path = dirbase,
  pattern = "\\.RData$",
  recursive = TRUE
) #' List all .RData files recursively

aerosol <- numeric() #' Initialize empty object to store all aerosol data

for (i in seq_along(listfilesg)) {
  load(file = paste0(dirbase, listfilesg[i])) #' Load `datos_aerosol` from file
  aerosol <- rbind(aerosol, datos_aerosol)    #' Append to the cumulative dataset
}

head(aerosol) #' Preview the first few rows of the merged aerosol dataset

names(aerosol) <- c("lat", "lon", "aerosol", "year", "day", "date") #' Rename columns


# 9. Create Spatial Points from Aerosol Coordinates ---------------------------

points_aerosol <- st_as_sf(
  data.frame(unique(aerosol[, c("lon", "lat")])),
  coords = c("lon", "lat"),
  crs = 4326
) #' Create spatial object from unique coordinates


# 10. Calculate Distances from Subregional Centroids ---------------------------

distancias <- st_distance(centroides_sf, points_aerosol) #' Compute distance matrix


# 11. Create and Join Distance Data --------------------------------------------

coordenadas_puntos <- data.frame(st_coordinates(points_aerosol)) %>%
  mutate(codigo_grilla = paste(X, Y, sep = ",")) #' Create unique grid ID per point

distancias <- data.frame(distancias) #' Convert to data frame
colnames(distancias) <- coordenadas_puntos$codigo_grilla #' Set column names
rownames(distancias) <- centroides_sf$Subreg_climat      #' Set row names

distancias <- data.frame(t(distancias)) %>%
  mutate(codigo_grilla = coordenadas_puntos$codigo_grilla) #' Transpose and attach grid codes

colnames(distancias)[-ncol(distancias)] <- centroides_sf$Subreg_climat #' Rename columns with subregion names


# 12. Debugging Output ---------------------------------------------------------

colnames(distancias)   #' Show final column names
centroides_sf          #' Print subregional centroids
names(distancias)      #' Inspect structure of final distance matrix

# 13. Convert Distance Data to Long Format --------------------------------------

distancias_long <- distancias %>%
  pivot_longer(
    cols = PC1:VC3,
    names_to = "Subregion_climatica",
    values_to = "distancia"
  ) #' Convert wide distance matrix to long format


# 14. Process Aerosol Data by Week and Coordinates ------------------------------

datos_aerosol <- aerosol %>%
  mutate(week = epiweek(date)) %>%                              #' Extract epidemiological week
  group_by(year, week, lat, lon) %>%
  summarise(aerosol = mean(aerosol, na.rm = TRUE), .groups = "drop") #' Weekly average aerosol by location


# 15. Reconstruct Date from Year and Week ---------------------------------------

datos_aerosol <- datos_aerosol %>%
  mutate(date = aweek::get_date(week = week, year = year)) #' Convert week/year to ISO date


# 16. Create Grid Code from Coordinates ----------------------------------------

datos_aerosol <- datos_aerosol %>%
  mutate(codigo_grilla = paste(lon, lat, sep = ",")) #' Unique ID per grid cell


# 17. Define Function to Aggregate Aerosol by Subregion ------------------------

Subregion_climatica <- unique(distancias_long$Subregion_climatica) #' Extract subregion codes

cambia_puntos_subregiones <- function(bloque) {
  #' Function to weight aerosol data by inverse distance and aggregate by subregion
  #' @param bloque Data frame for a given week-year
  #' @return Data frame with aerosol values assigned to subregions
  
  bloque_p <- NULL
  for (i in seq_along(Subregion_climatica)) {
    distancias_long_pba <- distancias_long %>%
      filter(Subregion_climatica == Subregion_climatica[i])
    
    bloque_c <- bloque %>%
      left_join(distancias_long_pba, by = "codigo_grilla") %>%
      mutate(distancia2 = aerosol * distancia / sum(distancia, na.rm = TRUE)) %>%
      select(year, week, Subregion_climatica, distancia2) %>%
      group_by(Subregion_climatica) %>%
      summarise(distancia2 = sum(distancia2, na.rm = TRUE), .groups = "drop") %>%
      distinct()
    
    bloque_p <- bind_rows(bloque_p, bloque_c)
  }
  return(bloque_p)
}


# 18. Extract Unique Year-Week Combinations ------------------------------------

fechas_unicas <- datos_aerosol %>%
  select(year, week) %>%
  distinct() #' Unique combinations of year and week


# 19. Apply Function by Week-Year ----------------------------------------------

bloques_subregiones <- NULL

for (i in seq_len(nrow(fechas_unicas))) {
  datos_aerosol_t <- datos_aerosol %>%
    filter(
      year == fechas_unicas$year[i],
      week == fechas_unicas$week[i]
    )
  
  bloques_subregiones_t <- cambia_puntos_subregiones(datos_aerosol_t)
  bloques_subregiones <- bind_rows(bloques_subregiones, bloques_subregiones_t)
}


# 20. Finalize and Save Aerosol Data -------------------------------------------

datos_aerosol <- bloques_subregiones
datos_aerosol_dist <- datos_aerosol

datos_aerosol <- datos_aerosol %>%
  rename(aerosol = distancia2) #' Rename weighted variable as final aerosol

save(datos_aerosol, file = "Aereosol_subregion_final.RData") #' Save to file


