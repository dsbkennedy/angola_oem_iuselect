

 ggplot() +
  geom_raster(data = cachiungo_oncho_point_df, aes(lon, lat, fill = suitability/100), alpha = .7) +
  geom_sf_label(data = cachiungo_adm3_shp,aes(label = ADM3_EN)) +
  #scale_fill_fermenter(n.breaks = 5, palette = "YlOrRd", direction = +1) +
  sc +
  geom_sf(data = cachiungo_adm3_shp, fill = NA) +
  theme_minimal() +
  labs(fill='Environmental suitability') +
   theme_minimal() +
   theme_minimal(legend.position='top', panel.background = element_blank()) +
  #blank() +
  north(cachiungo_adm3_shp) +
  scalebar(cachiungo_adm3_shp, dist = 25, dist_unit = "km",
           transform = TRUE, model = "WGS84") +
   ggpubr::theme_transparent()

 ggsave('images/bailundo_map.tif',  width = 20, height = 20, units = "cm",
        device = "tiff")
 
province_interest <- c('Bengo', 'Bié')
 
iu_interest <- c('Mavinga', 'Bailundo',  'Mungo')
#  
# iu_select_shp <- read_sf(here('data', 'input', 'shp', 'ago_admbnda_adm3_gadm_ine_ocha_20180904.shp')) %>% 
#    filter(ADM2_EN %in% iu_interest | ADM1_EN %in% province_interest) %>% 
#   filter(ADM2_EN %in% c('Bailundo', 'Mungo'))

main_map +
  geom_rect(
    xmin = 1869227,
    ymin = 5086142,
    xmax = 1887557,
    ymax = 5104660,
    fill = NA, 
    colour = "black",
    size = 0.6
  )

iu_sf <- read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>%
  filter(ADMIN0 == 'Angola') %>% 
  select(IU_ID,ADMIN1, ADMIN2, geometry) 

iu_select_shp <- iu_sf %>% 
  filter(ADMIN2 %in% c('Bailundo', 'Mungo'))

bounding_box = st_as_sfc(st_bbox(iu_select_shp))

# ggm1 <- ggplot() + 
#   geom_sf(data = iu_sf, fill = "white") + 
#   geom_sf(data = bounding_box, fill = NA, color = "red", size = 1.2) +
#   theme_void()

ggm1 <- ggplot() + 
  geom_sf(data = adm1_shp, fill = "white") + 
  geom_sf(data = bounding_box, fill = NA, color = "red", size = 1.2) +
  geom_sf_label(data = adm1_shp %>% mutate(label_clean=gsub('_', ' ', adm1_en)),aes(label=label_clean)) +
  labs(title='Map of provinces in Angola', 
       subtitle='Red box indicates proposed study site: 2 municipalities (Bailundo & Mungo) in Huambo province') +
  theme_void() +
  north(adm1_shp) +
  scalebar(adm1_shp, dist = 250, dist_unit = "km",
           transform = TRUE, model = "WGS84")


 #cachiungo_adm3_oncho_crop <- mask(africa_oncho_raster, cachiungo_adm3_shp)
 iu_select_adm3_oncho_crop <- raster::crop(africa_oncho_raster, iu_select_shp)
 
 iu_select_oncho_point  <-  rasterToPoints(iu_select_adm3_oncho_crop)
 iu_select_oncho_point_df <-  data.frame(iu_select_oncho_point)
 colnames(iu_select_oncho_point_df) = c("lon", "lat", "suitability")
 
 iu_envsuitability_map <- ggplot() +
   geom_raster(data = iu_select_oncho_point_df , aes(lon, lat, fill = suitability/100), alpha = .7) +
   geom_sf_label(data = iu_select_shp,aes(label = ADMIN2)) +
   #scale_fill_fermenter(n.breaks = 5, palette = "YlOrRd", direction = +1) +
   sc +
   geom_sf(data = iu_select_shp, fill = NA) +
   theme_void() +
   theme(legend.position = 'right',
         legend.direction = "vertical",
         legend.key.width = unit(25, "mm")) +
   labs(fill='0=low suitability\n1=high suitability', 
        title='Onchocerciasis environemental suitability estimates for two municipalities (Bailundo & Mungo) in Huambo province',
        subtitle = 'Estimates from Cromwell EA, Osborne JCP, Unnasch TR, Basáñez MG, Gass KM, et al. (2021)\nPredicting the environmental suitability for onchocerciasis in Africa as an aid to elimination planning.\nPLOS Neglected Tropical Diseases 15(7): e0008824. https://doi.org/10.1371/journal.pntd.0008824') +
   north(iu_select_shp) +
   scalebar(iu_select_shp, dist = 25, dist_unit = "km",
            transform = TRUE, model = "WGS84")
 

 library(cowplot)
cow_gph <- ggdraw() +
  draw_plot(ggm1,x = 0, y = 0.15, width = 0.46, height = 0.6) +
  draw_plot(iu_envsuitability_map,x = 0.5, y = 0.15, width = 0.46, height = 0.6) 

save_plot(here('images', 'angola_oncho_studyside.png'), cow_gph, ncol = 2, base_asp = 1.1)


ggsave(here('images', 'angola_oncho_studyside.tif'), device = "tiff", height=10, width=10, units="cm")




 