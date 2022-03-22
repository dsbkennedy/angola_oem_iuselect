library(tidyverse)
library(here)
library(janitor)
library(sf)

ang_shp <- read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>% 
  filter(ADMIN0=='Angola')

ang_crs <- st_crs(ang_shp)


angola_oncho_dados_raw <- readxl::read_excel(here('data', 
                                                  'input', 
                                                  'Oncho DADOS GERAL DE TODOS OS MAPEAMENTOS DA ONCHO Angola SL.xlsx'), sheet='ONCHO - DESDE 2002 ate 2020') 

angola_oncho_dados_clean <- angola_oncho_dados_raw %>% clean_names() %>% 
  filter(!is.na(latitude_decimal_degrees)) %>% 
  filter(!is.na(longitude_decimal_degrees)) %>% 
  mutate(latitude_clean=as.numeric(gsub("[^0-9.-]", "", latitude_decimal_degrees))) %>% 
  mutate(longitude_clean=as.numeric(gsub("[^0-9.-]", "", longitude_decimal_degrees))) %>% 
  mutate(prevalence=number_of_people_positive_16/number_of_people_examined_14) %>% 
  #mutate(prevalence=as.numeric(percent_positive_17)) 
  mutate(latitude_clean=ifelse(latitude_clean<0,latitude_clean*-1,latitude_clean)) %>% 
  filter(latitude_clean <18 & latitude_clean>4) %>% 
  mutate(latitude_clean=latitude_clean*-1) %>% 
  st_as_sf(., coords = c("longitude_clean", "latitude_clean"),
          crs= ang_crs, agr = "constant")

joined <- st_join(angola_oncho_dados_clean, ang_shp)

st_is_longlat(angola_oncho_dados_clean)

angola_geo = st_set_crs(angola_oncho_dados_clean, 4326)
st_is_longlat(angola_geo)

st_distance(ang_shp, angola_geo)

angola_oncho_dados_clean %>% 
  #filter(name_of_administrative_level_2_implementation_unit=="Huambo") %>% 
ggplot() + 
  #geom_point(aes(x=longitude_clean, y=latitude_clean))
  geom_sf(aes(color = prevalence), show.legend = "point") +
  geom_sf(fill = "transparent", color = "black", size = .1,
          data = ang_shp) 


ggplot() + 
  geom_sf(fill = "transparent", color = "black", size = .1,
          data = ang_shp)
  #geom_sf(data = angola_oncho_dados_clean, aes(size = prevalence), color = "black", show.legend = "point")


  
library(sp)
coordinates(angola_oncho_dados_clean) <- c('longitude_clean', 'latitude_clean') #this is the line which answers the question.
proj4string(angola_oncho_dados_clean) <- CRS("+init=epsg:4326")
str(angola_oncho_dados_clean)
