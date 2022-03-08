library(sf)
library(tidyverse)
library(here)
library(leaflet)
library(janitor)
library(linelist)

ang_shp <- read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>% 
    filter(ADMIN0=='Angola')

ang_espen_data <- read.csv(here('data', 'input', 'espen_platform_oncho.csv'))

joined <- ang_shp %>% 
  left_join(ang_espen_data, by=c('ADMIN1ID', 'ADMIN2ID', 'ADMIN3ID', 'IU_ID'))

joined %>% 
  filter(!Endemicity=='Not reported') %>% 
  group_by(IU_ID) %>%
  arrange(Year.y) %>% 
  filter(row_number()==1) %>% 
  ggplot() +
  geom_sf(aes(fill=Endemicity))
