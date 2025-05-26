# 1. Load Required Libraries --------------------------------------------------

library(tidyverse)  #' Tidyverse data science tools
library(sf)         #' Simple feature spatial data
library(geodata)    #' GADM access for administrative boundaries
library(readxl)     #' Read Excel files


# 2. Define Utility Function --------------------------------------------------

#' Remove accents, non-alphanumeric characters, and convert to lowercase
remove_accents_and_lowercase <- function(input_string) {
  cleaned_string <- iconv(input_string, to = "ASCII//TRANSLIT")
  cleaned_string <- str_remove_all(cleaned_string, "[^[:alnum:]]")
  cleaned_string <- tolower(cleaned_string)
  return(cleaned_string)
}


# 3. Load GADM Level 3 Boundaries for Costa Rica ------------------------------

CRC <- gadm(country = "Costa Rica", level = 3, path = ".") #' Load district-level shapefile


# 4. Exclude Specific Districts -----------------------------------------------

nombres_excluir <- c("Isla del Coco") #' Exclude this outlier district
CRC_sC <- CRC[!(CRC$NAME_3 %in% nombres_excluir), ]
CRC_sf <- st_as_sf(CRC_sC) #' Convert to sf object


# 5. Normalize District and Canton Names --------------------------------------

CRC_sf <- CRC_sf %>%
  mutate(
    distrito = remove_accents_and_lowercase(NAME_3),
    canton = remove_accents_and_lowercase(NAME_2)
  )


# 6. Load Area Classification Data from Excel ---------------------------------

tabla_areas <- read_xlsx("Distritos_Clasificados_final_2.xlsx") %>%
  mutate(
    distrito = remove_accents_and_lowercase(distrito),
    canton = remove_accents_and_lowercase(canton)
  )


# 7. Join Area Classification to Spatial Data ---------------------------------

CRC_sf_areas <- CRC_sf %>%
  left_join(tabla_areas, by = c("distrito", "canton"))


# 8. Merge Geometries by Subregion -------------------------------------------

union_sf <- CRC_sf_areas %>%
  group_by(Subreg_climat) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")


# 9. Visualize Merged Geometries ----------------------------------------------

ggplot() +
  geom_sf(data = union_sf, aes(fill = Subreg_climat)) +
  theme_minimal() +
  labs(fill = "Region")


# 10. Compute Centroids of Subregions -----------------------------------------

centroides_sf <- st_centroid(union_sf)


# 11. Save Output as RData ----------------------------------------------------

save(union_sf, centroides_sf, file = "./subregiones_geom.RData")
