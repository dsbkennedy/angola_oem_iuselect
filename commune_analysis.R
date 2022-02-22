require(pacman)
pacman::p_load(raster, sf, exactextractr, here, 
               leaflet, leaflet.extras, 
               janitor, patchwork,RColorBrewer,
               ggsn,gghighlight,ggsflabel,ggbeeswarm,
               linelist,tidylog)
library(tidyverse)

commune_palette <- colorRampPalette(rev(brewer.pal(5, "Spectral")))
commune_fill <- scale_fill_stepsn(n.breaks = 5, colours = viridis::viridis(5), limits=c(1, 100))

# Import shapefiles 
iu_shp <-
  read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>%
  filter(ADMIN0 == 'Angola') %>% 
  select(IU_ID, ADMIN2, geometry)

commune_shp <- read_sf(here('data', 'input', 'shp', 'ago_admbnda_adm3_gadm_ine_ocha_20180904.shp')) %>% 
  select(ADM1_EN, ADM2_EN, ADM3_EN, geometry)

# Import oncho data from ESPEN
ang_espen_data <-
  read.csv(here('data', 'input', 'espen_platform_oncho.csv')) %>%
  filter(Year == 2020) %>% 
  select(IU_ID,ADMIN2, Endemicity)

# Filter IU shapefile to only include unknown endemicity
iu_unknown_endemicity <- iu_shp %>% 
  left_join(ang_espen_data, by='IU_ID') %>% 
  filter(grepl('Unknown', Endemicity)) %>% 
  select(-Endemicity)
  
# Melt commune shapefile to ADM2 level
iu_combine_melt <- commune_shp %>% 
  group_by(ADM2_EN) %>% 
  dplyr::summarise()

# Join IU & melted ADM2 shapefiles
iu_commune <- sf::st_join(iu_unknown_endemicity, iu_combine_melt,
                          left = FALSE, largest = TRUE, join = st_within)

# Extract ADM2 name from joined datasets
adm2_unknown <- iu_commune %>% as_tibble() %>% 
  pull(ADM2_EN)

#Filter commune dataset to only include IUs identified as "Unknown" endemicity
unknown_commune_shp <- commune_shp %>% filter(ADM2_EN %in% adm2_unknown)

# Map to check data
(iu_commune_map <- unknown_commune_shp %>%
  ggplot() +
  geom_sf(aes(fill = ADM3_EN)) +
  #geom_sf_label(aes(label = ADM3_EN)) +
  geom_sf(
    fill = "transparent",
    color = "black",
    size = 1.5,
    data = iu_unknown_endemicity
  ) +
  theme(legend.position = "none"))

#Import environmental suitability as raster data
africa_oncho_rs <-
  stack(here('data', 'IHME_AFRICA_ONCHO_ENV_SUIT_MEAN_Y2020M08D26.TIF'))

#Crop raster to shapefile of communes within IUs with unknown endemicity
unknown_iu_oncho_combined <-
  exact_extract(africa_oncho_rs,
                unknown_commune_shp,
                progress = FALSE)  %>%
  bind_rows(., .id = "id") %>%
  as_tibble() %>% 
  mutate(id = as.numeric(id)) %>%
  #--- group summary ---#
  filter(!is.na(value))

# Merge raster values with shapefile
angola_commune_oncho <- unknown_commune_shp %>%
  mutate(id := seq_len(nrow(.))) %>%
  left_join(., unknown_iu_oncho_combined, by = "id") 

#Calculate weighted mean
angola_commune_oncho_collapse <- angola_commune_oncho %>%
  group_by(ADM1_EN,ADM2_EN,ADM3_EN) %>%
  summarise(oncho_mean_aw = sum(value * coverage_fraction) / sum(coverage_fraction))  

# Map weighted mean by ADM3  
(angola_commune_oncho_map <- angola_commune_oncho_collapse %>% 
  ggplot() +
  geom_sf(aes(fill = oncho_mean_aw)) +
    commune_fill)

# Map weighted mean for Uige province only
(uige_oncho_map <- angola_commune_oncho_collapse %>%
  filter(ADM1_EN=="Uíge") %>% 
  ggplot() +
  geom_sf(aes(fill = oncho_mean_aw)) +
  geom_sf_label(aes(label = ADM3_EN)) +
  commune_fill)

uige <- angola_commune_oncho %>% 
  as_tibble() %>% 
  select(ADM1_EN, ADM2_EN, ADM3_EN, value) %>% 
  filter(ADM1_EN=="Uíge") 
  
library(ggridges)
uige_beeswarm_gph <- uige %>% 
  ggplot(., aes(x = value, y = ADM3_EN)) + 
  geom_density_ridges(scale = 0.9) +
  geom_hline(yintercept = 71,
             color = "red", size=1, linetype="dashed") +
  labs(y = "Environmental suitability (0=lowest, 100=highest)", 
       x = "", 
       title = "Onchocerciasis environmental suitability score for communes in implementation units with unknown onchocerciasis endemicity", 
       subtitle = "Red dashed line at threshold value (>=71) for environmental suitability.") +
  #coord_flip() +
  theme(legend.position='top') 
  #facet_wrap(~ ADM2_EN, scales='free_y')

library(patchwork)
uige_oncho_map / uige_beeswarm_gph

ggplot(uige, aes(x = value, y = ADM3_EN, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )



######################################
st_crs(iu_unknown_endemicity) == st_crs(commune_shp)
# Join shapefiles

huambo_iu_shp <- iu_unknown_endemicity %>% filter(ADMIN2=="Huambo") %>% clean_data()
huambo_commune_shp <- commune_shp %>% filter(ADM2_EN=="Huambo") %>% clean_data()

cal_grid_neat <- st_intersection(iu_unknown_endemicity, commune_shp)

overlapping_pol_names <- cal_grid_neat %>% 
  as_tibble() %>% 
  mutate(key=paste0(ADM1_EN,ADM1_EN,ADM3_EN)) %>% 
  clean_data() %>% 
  pull(key)


overlapping_commune <- commune_shp %>% 
  as_tibble() %>% 
  mutate(key=paste0(ADM1_EN,ADM1_EN,ADM3_EN)) %>% 
  clean_data() %>% 
  filter(key %in% overlapping_pol_names)
  

iu_commune_grid <- st_intersects(bengo_commune_shp, bengo_iu_shp, sparse = F) %>%
  cbind(bengo_commune_shp, 'keep' = .) 
  filter(keep == TRUE)


iu_commune_intersection <- st_intersection(bengo_commune_shp, bengo_iu_shp) %>% filter(n.overlaps < 2)

inter <- st_intersection(poly) %>% filter(n.overlaps < 2)

commune_shp$indicator <- st_within(commune_shp, iu_unknown_endemicity) %>% lengths > 0

iu_commune <- sf::st_join(iu_unknown_endemicity, iu_combine_melt,
                          left = FALSE, largest = TRUE, join = st_within)

geo_join <- st_join(shared_management, sectors, left = FALSE, largest = TRUE)

iu_commune_map <- iu_combine_melt %>%
  ggplot() +
  geom_sf(aes(fill = ADM2_EN)) +
  #geom_sf_label(aes(label = ADM3_EN)) +
  geom_sf(
    fill = "transparent",
    color = "black",
    size = 1.5,
    data = iu_unknown_endemicity
  ) +
  theme(legend.position = "none")

  


y <- iu_unknown_endemicity %>% 
  filter(st_filter(commune_shp, .predicate = st_within))

filtered = st_crop(commune_shp, iu_unknown_endemicity) %>% filter(indicator==T)

joined = st_join(commune_shp, iu_unknown_endemicity, join = st_intersects) %>% 
  filter(!is.na(IU_ID)) %>% as_tibble()

st_filter(balt_metro, .predicate = st_within)

nc %>%
  filter(st_intersects(., ash_point, sparse = FALSE)[1,])

x <- iu_commune %>%  as_tibble()

x <- iu_commune %>% as_tibble() %>% 
  count(ADMIN2.x, ADMIN2.y)

plot(iu_commune$geometry)
iu_df <- iu_commune %>% as_tibble()

# Drop known endemicity



# Make vector of unknown endemicity
unknown_iu <-
  ang_espen_data %>% 
  filter(grepl('Unknown', Endemicity)) %>% 
  pull(IU_ID)

commune_unknown_endemicity <- commune_shp %>% 

cachiungo_adm3_oncho_crop <- raster::crop(africa_oncho_raster, cachiungo_adm3_shp)

cachiungo_oncho_point  <-  rasterToPoints(cachiungo_adm3_oncho_crop)
cachiungo_oncho_point_df <-  data.frame(cachiungo_oncho_point)
colnames(cachiungo_oncho_point_df) = c("lon", "lat", "suitability")

cachiungo_map_label <- ggplot() +
  geom_raster(data = cachiungo_oncho_point_df, aes(lon, lat, fill = suitability), alpha = .7) +
  geom_sf_label(data = cachiungo_adm3_shp,aes(label = ADM3_EN)) +
  #scale_fill_fermenter(n.breaks = 5, palette = "YlOrRd", direction = +1) +
  sc + 
  geom_sf(data = cachiungo_adm3_shp, fill = NA) +
  theme_minimal() +
  labs(fill='Environmental suitability') +
  theme(legend.position='top') +
  #blank() +
  north(cachiungo_adm3_shp) +
  scalebar(cachiungo_adm3_shp, dist = 25, dist_unit = "km",
           transform = TRUE, model = "WGS84")

cachiungo_map_label