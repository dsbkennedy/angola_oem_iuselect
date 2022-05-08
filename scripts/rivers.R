# River data 

pacman::p_load(here, sf)
library(tidyverse)

river_shp <- list.files(here('data', 'input', 'GloRiC_v10_shapefile'), 
                        pattern='.shp',
                        recursive = T, full.names = T)
river_shp <- read_sf(river_shp)

rgdal::ogrInfo(system.file(river_shp))

sf::st_layers(river_shp)

cshapes_row <- st_read(river_shp, 
                       query = "SELECT * FROM GloRiC_v10 WHERE Reach_ID LIKE '1%' LIMIT 10")

data = read_sf(river_shp, query = 'SELECT * from GloRiC_v10 WHERE Reach_ID < 12000000')
#saveRDS(data, here('data', 'input', 'africa_rivers.Rds'))

rand_select <- data
#%>% dplyr::sample_frac(0.4)


angola_river <- sf::st_crop(rand_select, ang_shp)
#saveRDS(angola_river, here('data', 'input', 'angola_rivers.Rds'))



rivers_small <- angola_river %>%
  rename(geometry='_ogr_geometry_') %>% 
  filter(Reach_type != 0) %>% 
  mutate(class_hydr_discharge = str_sub(Class_hydr, 1, 1),
         variability = str_sub(Class_hydr, 2, 2),
         CMI = str_sub(Class_phys, 2, 2)) %>% 
  rowwise() %>%
  mutate(red_hydr_class = ifelse(Reach_type < 1000,
                                 str_sub(Reach_type, 2, 2),
                                 str_sub(Reach_type, 3, 3))) %>%
  ungroup() %>%
  mutate(width = as.numeric(class_hydr_discharge),
         width = case_when(width == 5 ~ 1,
                           width == 4 ~ 0.8,
                           width == 3 ~ 0.6,
                           width == 2 ~ 0.4,
                           width == 1 ~ 0.2,
                           TRUE ~ 0)) %>% 
  rowwise() %>%
  mutate(stream_power = ifelse(Reach_type < 1000,
                               str_sub(Reach_type, 3, 3),
                               str_sub(Reach_type, 4, 4))) %>%
  ungroup()

rivers_plot <- rivers_small %>%
  filter(CMI != 1 | Class_geom == 12 | variability <= 1 | class_hydr_discharge > 1 | stream_power >= 2) %>% 
  select(width, Reach_type, geometry , red_hydr_class) %>% 
  st_as_sf()

nrow(rivers_small)

ggplot(data = ang_shp) +
  geom_sf(fill="#FFFFFF", color="#AAAAAA") +
  geom_sf(data=rivers_plot, aes(color=factor(Reach_type), size=width, 
                                         alpha=factor(red_hydr_class))) +
  scale_size_continuous(range = c(0,0.3)) +
  scale_alpha_manual(values=c("1" = 0.1, "2" = 0.4, "3" = 0.7, "4" = 1, "5" = 1)) +
  theme_minimal() + theme(legend.position="none", panel.grid = element_blank())


#https://blog.benthies.de/blog/mapping-streams-and-rivers-with-ggplot-sf/
  
import_rivers <- function(river_shp) {
  
river_shp <- list.files(here('data', 'input', 'GloRiC_v10_shapefile'), 
                        pattern='.shp',
                        recursive = T, full.names = T)

river_shp <- st_read(river_shp)

return(river_shp)

}

# Import shapefiles 
import_iu_shapefile <- function(iu_shp) {
  
  iu_shp <-
  read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>%
  filter(ADMIN0 == 'Angola') %>% 
  select(ADMIN0,ADMIN1, ADMIN2,IU_ID, geometry)
  
  ang_shp <- iu_shp %>% 
    group_by(ADMIN0) %>% 
    summarise()
  
  return(iu_shp)

}

bbox = st_as_sfc(st_bbox(iu_shp))

angola_river <- sf::st_crop(river_shp, ang_shp)


angola_river <- ggplot() +
  geom_sf(data=river_shp, aes(color=factor(Reach_type), size=Class_hydr)) +
  coord_sf(crs = 4087,
           xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"]))
