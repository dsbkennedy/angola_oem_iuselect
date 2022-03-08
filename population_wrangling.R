

library(readxl)
library(here)
library(janitor)
library(tidyverse)
library(tidyxl)
library(unpivotr)

#https://medium.com/@samukweku/reshaping-an-excel-table-in-r-71d7d9020124

#sourcefile
filename <- here('data/input/Estimativas_Comunais_Geral_Final.xlsx')
#get list of sheet_names
#which also represents the company names
sheet_names <- xlsx_sheet_names(filename)#read in file, using the tidyxl package
#filter for only character and numeric
#and pick specific rows and columns
def <- function(sheetname){xlsx_cells(filename,sheetname)%>%
    filter(data_type %in% c("character","numeric"), 
           row >=5, 
           col >=1)}
#stage1
#apply function to sheetnames
#using the purrr library
stage1 <- map(sheet_names, def)

def_main <- function(datum){datum%>%
    select(sheet,row,col,data_type,character,numeric)%>%
    #behead("NNE",years)%>%
    behead("N",commune)%>%
    fill(character, .direction = "down") %>% 
    filter(!is.na(commune))
  }#apply function to every tibble in stage1
stage2 <- map(stage1,def_main)

dirty_labels <- c("controlo","estimativ")
commune_pop_append <- bind_rows(stage2) %>% 
  select(province=sheet,commune, age_group=character, population=numeric)  %>% 
  dplyr::filter(!is.na(population))  %>% 
  clean_data() %>% 
  filter(!grepl("estimativ", commune, ignore.case=TRUE)) %>% 
  filter(!grepl("controlo", commune, ignore.case=TRUE)) %>% 
  filter(age_group!='total') 
  

province_commune_match <- commune_pop_append %>%
  filter(province==commune | commune=="chitato" | commune=="bengo" | commune=="caiongo") %>%
  group_by(province,commune,age_group) %>%
  slice(which.min(population))


province_commune_population <- commune_pop_append %>%
  filter(!commune %in% c("chitato","bengo","caiongo")) %>% 
  filter(province!=commune) %>% 
  bind_rows(province_commune_match) %>% 
  #mutate(row = row_number()) %>%
  pivot_wider(id_cols=c('province','commune'), names_from=age_group, values_from=population) 
  #pivot_wider(names_from=age_group, values_from=population) %>%  
  #select(-row)

province_commune_population %>% filter(commune=="Huambo")

#Chitato 

# 
# 
# # Compare with raster WorldPop data
# 
world_pop_raster <- raster::stack(here('data', 'input', 'ago_ppp_2020_1km_Aggregated_UNadj.tif'))
commune_shp <- sf::read_sf(here('data', 'input', 'shp', 'ago_admbnda_adm3_gadm_ine_ocha_20180904.shp'))
  select(ADM1_EN, ADM2_EN, ADM3_EN, geometry) %>% 
  filter(ADM3_EN=="Huambo")
  
ago_admin_codes <- commune_shp %>% 
  as_tibble() %>% 
  select(ADM1_EN, ADM1_PCODE, ADM2_EN, ADM2_PCODE,ADM3_EN, ADM3_PCODE) %>% 
  clean_data()
#   
#Crop raster to Huambo shapefile
angola_pop_combined <-
  exact_extract(world_pop_raster,
                commune_shp,
                progress = TRUE)  %>%
  bind_rows(., .id = "id") %>%
  as_tibble()

huambo_pop <- raster::mask(world_pop_raster, commune_shp)
huambo_pop <- crop(world_pop_raster, commune_shp)

huambo_pop <- stack(raster::disaggregate(huambo_pop, fact = 5, method = "bilinear"))

plot(huambo_pop)

huambo_pop_sum <- raster::extract(x = world_pop_raster, 
                                      y = commune_shp,
                                      fun=sum, 
                                      df=TRUE)

#--- weighted mean ---#
angola_pop_by_id <- angola_pop_combined %>%
  #--- convert from character to numeric  ---#
  mutate(id = as.numeric(id)) %>% 
  #--- group summary ---#
  group_by(id) %>%
  filter(!is.na(value)) %>%
  summarise(total_pop = sum(value, na.rm=TRUE))

#--- merge ---#
angola_pop <- commune_shp %>%
  mutate(id := seq_len(nrow(.))) %>%
  left_join(., angola_pop_by_id, by = "id")
  dplyr::select(id, total_pop, 'ADMIN1ID', 'ADMIN2ID', 'ADMIN3ID', 'IU_ID')
  
huambo_commune_population <- province_commune_population %>% 
  filter(commune=="huambo") %>% 
  select(-contains('estima')) %>% 
  pivot_longer(-c('province', 'commune'))

huambo_commune_population %>% summarise(total_pop=sum(value))
#   
#   angola_pop %>% as_tibble() %>% 
#     summarise(total_pop=sum(total_pop))
# 
# 
# oncho_angola_mean_env_iu <- oncho_angola %>%
#   as.data.frame() %>%
#   select('ADMIN1ID', 'ADMIN2ID', 'ADMIN3ID', 'IU_ID', 'oncho_mean_aw')
# 
# remotes::install_github("afrimapr/afrilearndata")
# 
# library(terra)
# 
world_pop_raster <- terra::rast(here('data', 'input', 'ago_ppp_2020_constrained.tif'))
commune_shp <- sf::read_sf(here('data', 'input', 'shp', 'ago_admbnda_adm3_gadm_ine_ocha_20180904.shp')) %>%
  select(ADM1_EN, ADM2_EN, ADM3_EN, geometry) %>% 
  filter(ADM2_EN=="Huambo")

pop_crop <- terra::extract(commune_shp,world_pop_raster)
pop_sv <- terra::vect(commune_shp)
pop_by_commune <- terra::extract(world_pop_raster, pop_sv, fun=sum)
# 
# plot(pop_crop)
