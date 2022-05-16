require(pacman)
pacman::p_load(raster, sf, exactextractr, here, leaflet, leaflet.extras, 
               janitor, patchwork,RColorBrewer,ggsn,gghighlight,ggsflabel,conflicted)
library(tidyverse)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("scalebar", "ggsn")
conflict_prefer("geom_sf_label", "ggsflabel")

# Import raster data in raster format
africa_oncho_raster <- raster::raster(here('data', 'IHME_AFRICA_ONCHO_ENV_SUIT_MEAN_Y2020M08D26.TIF'))

angola_oncho_raster_crop <- crop(africa_oncho_raster, ang_shp)

hist(angola_oncho_raster_crop,
     breaks = 50,
     main = "Distribution of environmental suitability scores",
     xlab = "Environmental suitability (0=lowest, 100=highest)", ylab = "Frequency",
     col = "springgreen")

ang_shp <- read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>% 
  filter(ADMIN0=='Angola')

africa_oncho_rs <- stack(here('data', 'IHME_AFRICA_ONCHO_ENV_SUIT_MEAN_Y2020M08D26.TIF'))

#Crop raster to Angola shapefile
angola_oncho_raster_df <- 
  exact_extract(
    africa_oncho_rs, 
    ang_shp,
    progress = FALSE
  )  %>% 
  bind_rows(., .id = "id") %>% 
  as_tibble() %>% 
  angola_oncho_combined %>% 
  #--- convert from character to numeric  ---#
  mutate(id = as.numeric(id)) %>% 
  #--- group summary ---#
  group_by(id) %>% 
  filter(!is.na(value)) 

#--- merge ---#
oncho_angola_env_threshold <- ang_shp %>% 
  mutate(id := seq_len(nrow(.))) %>% 
  left_join(., angola_oncho_raster_df, by = "id") %>% 
  dplyr::select(id, 'ADMIN1ID', 'ADMIN2ID', 'ADMIN3ID', 'IU_ID', 'value') %>% 
  left_join(ang_espen_data, by=c('ADMIN1ID', 'ADMIN2ID', 'ADMIN3ID', 'IU_ID'))  
  

ggplot(oncho_angola, aes(x=value)) + 
    geom_histogram(binwidth=4) +
  facet_wrap(~ ADMIN1, scales='free_y')

ggplot(oncho_angola, aes(x=factor(ADMIN1), y=value)) +
  geom_boxplot(notch=TRUE) +
  coord_flip()
  
remotes::install_github("R-CoderDotCom/ridgeline@main")
library(ridgeline)

library(ridgeline)

oncho_angola %>% ridgeline(ADMIN1, value)

ridgeline(oncho_angola$value, oncho_angola$ADMIN1,mode=TRUE)


pacman::p_load(ggbeeswarm)

oncho_angola_env_threshold %>% 
  #filter(IU_ID %in% unknown_iu) %>% 
ggplot(.,aes((ADMIN2), value, color=Endemicity)) + geom_quasirandom() +
  geom_hline(yintercept = 71,
             color = "black", size=1.5) +
  labs(y = "Environmental suitability (0=lowest, 100=highest)", x = "") +
  coord_flip() +
  theme(legend.position='top') +
  facet_wrap(~ ADMIN1, scales='free_y', ncol=5)
