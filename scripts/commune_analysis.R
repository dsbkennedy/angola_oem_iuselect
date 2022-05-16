require(pacman)
library(tidyverse)
pacman::p_load(raster, sf, exactextractr, here, 
               leaflet, leaflet.extras, 
               janitor, patchwork,RColorBrewer,
               ggsn,gghighlight,ggsflabel,ggbeeswarm,
               linelist,tidylog, patchwork)

commune_palette <- colorRampPalette(rev(brewer.pal(6, "Spectral")))
commune_fill <- scale_fill_stepsn(n.breaks = 6, colours = viridis::viridis(6), limits=c(1, 100))

# Import shapefiles 
iu_shp <-
  read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>%
  filter(ADMIN0 == 'Angola') %>% 
  select(IU_ID, geometry)

commune_shp <- read_sf(here('data', 'input', 'shp', 'ago_admbnda_adm3_gadm_ine_ocha_20180904.shp')) %>% 
  select(ADM1_EN, ADM1_PCODE, ADM2_EN,ADM2_PCODE, ADM3_EN,ADM3_PCODE, geometry) %>% 
    clean_data()

adm1_shp <- commune_shp %>%
  group_by(adm1_en) %>%
  summarise()


source(here('admin_db.R'))

commune_shp_linked <- commune_shp %>% 
  left_join(linking_db %>% 
              select('adm1_pcode', 'adm2_pcode', 'adm3_pcode','iu_id'),
            by=c('adm1_pcode', 'adm2_pcode', 'adm3_pcode'))

# source(here('population_wrangling.r'))
# 
# commune_shp_clean <- commune_shp %>% clean_data()
# 
# commune_pop_clean <- province_commune_population %>% clean_data()
# 
# commune_shp_pop_merge <- commune_shp_clean %>%
#   inner_join(commune_pop_clean, by=c('adm1_en'='province', 'adm3_en'='commune')) 



# shp_pop_merge <- commune_shp_clean %>% 
#   anti_join(commune_pop_clean, by=c('adm1_en'='province', 'adm3_en'='commune')) %>% 
#   as.data.frame() %>% 
#   select(adm1_en, adm3_en)


# Import oncho data from ESPEN
ang_espen_data <-
  read.csv(here('data', 'input', 'espen_platform_oncho.csv')) %>%
  filter(Year == 2020) %>% 
  select(IU_ID,ADMIN2, Endemicity)

loa_loa_raw <-
  read.csv(here('data', 'input', 'espen_platform_loaloa.csv'))

loa_loa_wd <- loa_loa_raw %>%
  rename(loaloa_status = Endemicity) %>% 
  filter(Year==2020) %>% 
  select(IU_ID, loaloa_status)
rm (loa_loa_raw)

# Filter IU shapefile to only include unknown endemicity
# iu_unknown_endemicity <- iu_shp %>% 
#   left_join(ang_espen_data, by='IU_ID') %>% 
#   filter(grepl('Unknown', Endemicity)) %>% 
#   select(-Endemicity) %>% 
#   left_join(loa_loa_wd, by='IU_ID')

commune_shp_unknown <- commune_shp_linked %>% 
  left_join(ang_espen_data, by=c('iu_id' = 'IU_ID')) %>% 
  filter(grepl('Unknown', Endemicity)) %>% 
  select(-Endemicity) %>% 
  left_join(loa_loa_wd, by=c('iu_id' = 'IU_ID')) %>% 
  mutate(oncho_status="Unknown")

  
# Melt commune shapefile to ADM2 level
# iu_combine_melt <- commune_shp_pop_merge %>% 
#   group_by(ADM2_EN) %>% 
#   dplyr::summarise()
# 
# # Join IU & melted ADM2 shapefiles
# iu_commune <- sf::st_join(iu_unknown_endemicity, iu_combine_melt,
#                           left = FALSE, largest = TRUE, join = st_within)
# 
# # Extract ADM2 name from joined datasets
# adm2_unknown <- iu_commune %>% 
#   as_tibble() %>% 
#   select(ADM2_EN, loaloa_status)
# 
# #Filter commune dataset to only include IUs identified as "Unknown" endemicity
# unknown_commune_shp <- commune_shp_pop_merge %>% right_join(adm2_unknown,by='ADM2_EN')
# 
# # Map to check data
# (iu_commune_map <- unknown_commune_shp %>%
#   ggplot() +
#   geom_sf(aes(fill = ADM3_EN)) +
#   #geom_sf_label(aes(label = ADM3_EN)) +
#   geom_sf(
#     fill = "transparent",
#     color = "black",
#     size = 1.5,
#     data = iu_unknown_endemicity
#   ) +
#   theme(legend.position = "none"))

iu_commune_map <- commune_shp_unknown %>%
  ggplot() +
  geom_sf(aes(fill = oncho_status)) +
  geom_sf(fill = "transparent", color = "black", size = 1.5,
          data=adm1_shp) +
  theme(legend.position = "none")

#Import environmental suitability as raster data
africa_oncho_rs <-
  stack(here('data', 'IHME_AFRICA_ONCHO_ENV_SUIT_MEAN_Y2020M08D26.TIF'))

#Crop raster to shapefile of communes within IUs with unknown endemicity
unknown_iu_oncho_combined <-
  exact_extract(africa_oncho_rs,
                commune_shp_unknown,
                progress = FALSE)  %>%
  bind_rows(., .id = "id") %>%
  as_tibble() %>% 
  mutate(id = as.numeric(id)) %>%
  #--- group summary ---#
  filter(!is.na(value))

# Merge raster values with shapefile
angola_commune_oncho <- commune_shp_unknown %>%
  mutate(id := seq_len(nrow(.))) %>%
  left_join(., unknown_iu_oncho_combined, by = "id") 

#Calculate weighted mean
angola_commune_oncho_collapse <- angola_commune_oncho %>%
  group_by(adm1_en,adm2_en,adm3_en) %>%
  summarise(oncho_mean_aw = sum(value * coverage_fraction) / sum(coverage_fraction))  

# Map weighted mean by ADM3  
(angola_commune_oncho_map <- angola_commune_oncho_collapse %>% 
  ggplot() +
  geom_sf(aes(fill = oncho_mean_aw)) +
    commune_fill)

commune_boxwhisker <- angola_commune_oncho %>% 
split(.$ADM1_EN) %>% 
  imap(~ ggplot(data=.x) +
        geom_boxplot(aes(value, reorder(ADM3_EN,desc(ADM3_EN)))) + 
        theme_bw() +
        scale_x_continuous(sec.axis = dup_axis()) +
        labs(x='Environmental suitability', 
             y='Commune') +
          ggtitle(.y) 
  )

commune_map <-  angola_commune_oncho_collapse %>%
  split(.$ADM1_EN) %>%
  imap(~ ggplot(data = .x) +
      geom_sf(aes(fill = oncho_mean_aw)) +
      geom_sf_label(aes(label = ADM3_EN)) +
      theme(legend.position = 'top') +
      labs(x = "", y = "", fill = "Environmental suitability") +
      commune_fill +
      ggtitle(.y)
  )

all_plots <- map2(commune_map,commune_boxwhisker,~{.x / .y})

# Check env suitability by loaloa endemicity


angola_commune_oncho %>%
  ggplot(aes(x = loaloa_status, y = value)) + 
  #ylim(0,30)
  geom_boxplot(color = 'black', fill = 'firebrick') +
  labs(x = "Loa loa", y = "Oncho environmental suitability")


library(broom)
library(kableExtra)
library(ggthemes)
oncho_env_loaloa_compare <- ggplot(data=angola_commune_oncho, aes(x=value, group=loaloa_status, fill=loaloa_status)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(x="Environmental suitability", fill="Loa loa status") +
    theme_tufte()
  
 oncho_env_loaloa_ttest <-t.test(value ~ loaloa_status, data = angola_commune_oncho, var.equal = TRUE) %>% 
    tidy()
    kable()
  
  
plot_points <- ggplot() +
    geom_jitter(aes(x = factor(loaloa_status), y = value),
                data = angola_commune_oncho,
                width = 0.1) +
    xlab("Group") +
    ylab("Response") +
    theme_bw()

  model <- lm(value ~ factor(loaloa_status), data = angola_commune_oncho)
  result <- tidy(model)
  
  plot_difference <- ggplot() +
    geom_pointrange(aes(x = term, y = estimate,
                        ymin = estimate - 2 * std.error,
                        ymax = estimate + 2 * std.error),
                    data = result) +
    #ylim(-5, 5) +
    ylab("Value") +
    xlab("Coefficient") +
    coord_flip() +
    theme_bw()

  library(ggpubr)  
  plot_combined <- ggarrange(plot_points,
                             plot_difference,
                             heights = c(2, 1))

###################################
  
library(ggridges)
uige_beeswarm_gph <- uige %>% 
  ggplot(., aes(x = value, y = ADM3_EN)) + 
  geom_quasirandom() +
  #geom_density_ridges(scale = 0.9) +
  geom_hline(yintercept = 71,
             color = "red", size=1, linetype="dashed") +
  labs(y = "Environmental suitability (0=lowest, 100=highest)", 
       x = "", 
       title = "Onchocerciasis environmental suitability score for communes in implementation units with unknown onchocerciasis endemicity", 
       subtitle = "Red dashed line at threshold value (>=71) for environmental suitability.") +
  #coord_flip() +
  theme(legend.position='top') 
  #facet_wrap(~ ADM2_EN, scales='free_y')



ggplot(uige, aes(x = value, y = ADM3_EN, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c( 0.95)
  ) +
  scale_fill_manual(
    name = "Probability", values = c( "#A0A0A0A0", "#0000FFA0"),
    #labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )
  facet_wrap(~ADM2_EN, scales='free_y')

ggplot(uige, aes(x = value, y = ADM3_EN)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.95), alpha = 0.7) +
  facet_wrap(~ADM2_EN, scales='free_y')


ggplot(uige, aes(x = value, color = ADM3_EN, fill = ADM3_EN)) +
  geom_density(alpha = .15)

ggplot(uige, aes(value, ADM3_EN)) +
  geom_boxplot(aes(group = cut_width(ADM3_EN, 0.25)), outlier.alpha = 0.1)

labs1 <- levels(factor(uige$ADM3_EN))

ggplot(uige, aes(value, reorder(ADM3_EN,desc(ADM3_EN)))) + 
  geom_boxplot(outlier.alpha = 0.5) +
  scale_x_continuous(sec.axis = dup_axis()) +
  labs(x='Environmental suitability', y='Commune')

  theme_classic()

aes(x = reorder(the_factor, desc(the_factor)), ...)

######################################
st_crs(iu_unknown_endemicity) == st_crs(commune_shp_pop_merge)
# Join shapefiles

huambo_iu_shp <- iu_unknown_endemicity %>% filter(ADMIN2=="Huambo") %>% clean_data()
huambo_commune_shp <- commune_shp_pop_merge %>% filter(ADM2_EN=="Huambo") %>% clean_data()

cal_grid_neat <- st_intersection(iu_unknown_endemicity, commune_shp_pop_merge)

overlapping_pol_names <- cal_grid_neat %>% 
  as_tibble() %>% 
  mutate(key=paste0(ADM1_EN,ADM1_EN,ADM3_EN)) %>% 
  clean_data() %>% 
  pull(key)


overlapping_commune <- commune_shp_pop_merge %>% 
  as_tibble() %>% 
  mutate(key=paste0(ADM1_EN,ADM1_EN,ADM3_EN)) %>% 
  clean_data() %>% 
  filter(key %in% overlapping_pol_names)
  

iu_commune_grid <- st_intersects(bengo_commune_shp, bengo_iu_shp, sparse = F) %>%
  cbind(bengo_commune_shp, 'keep' = .) 
  filter(keep == TRUE)


iu_commune_intersection <- st_intersection(bengo_commune_shp, bengo_iu_shp) %>% filter(n.overlaps < 2)

inter <- st_intersection(poly) %>% filter(n.overlaps < 2)

commune_shp_pop_merge$indicator <- st_within(commune_shp_pop_merge, iu_unknown_endemicity) %>% lengths > 0

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
  filter(st_filter(commune_shp_pop_merge, .predicate = st_within))

filtered = st_crop(commune_shp_pop_merge, iu_unknown_endemicity) %>% filter(indicator==T)

joined = st_join(commune_shp_pop_merge, iu_unknown_endemicity, join = st_intersects) %>% 
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

commune_unknown_endemicity <- commune_shp_pop_merge %>% 

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