# River data 

#https://blog.benthies.de/blog/mapping-streams-and-rivers-with-ggplot-sf/

pacman::p_load(here, sf)
library(tidyverse)

import_iu_shapefile <- function(iu_shp) {
  iu_shp <-
    read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>%
    filter(ADMIN0 == 'Angola') %>%
    select(ADMIN0, ADMIN1, ADMIN2, IU_ID, geometry)
  
  return(iu_shp)
}
iu_shp <- import_iu_shapefile()

collapse_country_shapefile <- function(ang_shp) {
  ang_shp <- iu_shp %>%
    group_by(ADMIN0) %>%
    summarise()
  
  return(ang_shp)
}
  
ang_shp <- collapse_country_shapefile()

import_river_shapefile <- function(river_shp, angola_river) {

# river_shp <- list.files(here('data', 'input', 'GloRiC_v10_shapefile'), 
#                         pattern='.shp',
#                         recursive = T, full.names = T)
# 
# river_shp <- read_sf(river_shp, query = 'SELECT * from GloRiC_v10 WHERE Reach_ID < 12000000')
  
#saveRDS(data, here('data', 'input', 'africa_rivers.Rds'))

#angola_river <- sf::st_crop(river_shp, ang_shp)
  
#saveRDS(angola_river, here('data', 'input', 'angola_rivers.Rds'))
  
angola_river <- readRDS(here('data', 'input', 'angola_rivers.Rds'))

return(angola_river)

}

angola_river <- import_river_shapefile()

river_processing <- function(data, ADMIN2) {
  
  province_river <- sf::st_crop(angola_river, data)
  
  rivers_small <- province_river %>%
    rename(geometry = '_ogr_geometry_') %>%
    filter(Reach_type != 0) %>%
    mutate(
      class_hydr_discharge = str_sub(Class_hydr, 1, 1),
      variability = str_sub(Class_hydr, 2, 2),
      CMI = str_sub(Class_phys, 2, 2)
    ) %>%
    rowwise() %>%
    mutate(red_hydr_class = ifelse(
      Reach_type < 1000,
      str_sub(Reach_type, 2, 2),
      str_sub(Reach_type, 3, 3)
    )) %>%
    ungroup() %>%
    mutate(
      width = as.numeric(class_hydr_discharge),
      width = case_when(
        width == 5 ~ 1,
        width == 4 ~ 0.8,
        width == 3 ~ 0.6,
        width == 2 ~ 0.4,
        width == 1 ~ 0.2,
        TRUE ~ 0
      )
    ) %>%
    rowwise() %>%
    mutate(stream_power = ifelse(
      Reach_type < 1000,
      str_sub(Reach_type, 3, 3),
      str_sub(Reach_type, 4, 4)
    )) %>%
    ungroup()
  
  rivers_plot <- rivers_small %>%
    filter(CMI != 1 |
             Class_geom == 12 |
             variability <= 1 |
             class_hydr_discharge > 1 | stream_power >= 2) %>%
    select(width, Reach_type, geometry , red_hydr_class) %>%
    st_as_sf()
  
  province_river_map <- ggplot(data = data) +
    geom_sf(fill = "#FFFFFF", color = "#AAAAAA") +
    geom_sf(data = rivers_plot, aes(
      color = factor(Reach_type),
      size = width,
      alpha = factor(red_hydr_class)
    )) +
    #ggtitle(glue("Number of Cylinder: {.y}")) +
    scale_size_continuous(range = c(0, 0.3)) +
    scale_alpha_manual(values = c(
      "1" = 0.1,
      "2" = 0.4,
      "3" = 0.7,
      "4" = 1,
      "5" = 1
    )) +
    theme_minimal() + theme(legend.position = "none", panel.grid = element_blank())
  
  return(province_river_map)

}

library(furrr)
library(glue)
plan(multisession)
x <- iu_shp %>% 
  filter(ADMIN1 %in% c('Bengo', "Huambo")) %>% 
  group_nest(ADMIN2) %>% 
  mutate(river_map=future_map(data, river_processing))


