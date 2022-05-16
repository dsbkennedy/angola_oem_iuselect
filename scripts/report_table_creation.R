


final_table <- angola_commune_oncho_noloaloa_over71 %>% as_tibble() %>% 
  select(province=adm1_en, municipality=adm2_en, commune=adm3_en, population, Endemicity )

df1 <- angola_commune_oncho_collapse %>% as_tibble() %>% 
  select( "adm1_pcode","adm2_pcode","adm3_pcode","oncho_mean_aw" )

df2 <- angola_commune_oncho_noloaloa_over71 %>% as_tibble() %>% 
  select("adm1_pcode","adm2_pcode","adm3_pcode","adm1_en","adm2_en","adm3_en",propover71,total_pop, Endemicity )

df3 <- neighbour_endemic_area %>% as_tibble() %>% select(adm1_en, adm2_en, adm3_en) %>% mutate(neighbours_endemic_iu='Yes')

commune_survey_data <- readRDS(here('data','output','commune_survey_data.Rds')) %>% 
  group_by(adm1_en, adm2_en, adm3_en) %>% 
  filter(SurveyYear==max(SurveyYear))
        
df2_merge_df3 <- df2 %>% left_join(df3, by=c("adm1_en","adm2_en","adm3_en")) %>% 
  left_join(commune_survey_data, by=c("adm1_en","adm2_en","adm3_en"))

df_merge <- df2_merge_df3 %>% left_join(df1, by=c("adm1_pcode","adm2_pcode","adm3_pcode")) %>% 
  mutate(lf_treatment=case_when(grepl('lf', Endemicity, ignore.case=T) ~ 'Yes')) %>% 
  left_join(case_reported, by=c('adm1_en'='province', 'adm2_en'='municipality')) %>% 
  arrange(adm1_en, adm2_en) %>% 
  mutate(propover71=propover71/100,
         oncho_mean_aw=oncho_mean_aw/100,
         total_pop=total_pop/1000
         )

env_rank_df <- df_merge %>% 
  mutate(env_rank=dense_rank(desc(oncho_mean_aw))) %>% 
  filter(env_rank<=50)
  
prop_rank_df <- df_merge %>% 
  mutate(prop_rank=dense_rank(desc(propover71))) %>% 
  filter(prop_rank<=50)

prev_rank_df <- df_merge %>% 
  filter(Prevalence>0) %>% 
  mutate(prev_rank=dense_rank(desc(Prevalence))) %>% 
  filter(prev_rank<=50)

longlist <- angola_commune_oncho_noloaloa_over71 %>% as_tibble() %>% pull(adm3_pcode)

shortlist <- env_rank_df %>% bind_rows(prop_rank_df,prev_rank_df) %>% 
  count(adm1_en, adm2_en,adm3_en,adm3_pcode) %>% 
  #count(adm1_pcode, adm2_pcode, adm3_pcode) %>% 
  filter(n==3) %>% 
  pull(adm3_pcode)

high_ranking_map <- commune_shp %>% 
  mutate(list_cat=case_when((adm3_pcode %in% longlist & !adm3_pcode %in% shortlist)~ "Longlist",
    adm3_pcode %in% shortlist ~ 'Shortlist')) %>% 
  filter(!is.na(list_cat)) %>% 
  ggplot(data=.) +
  #geom_sf(aes(fill=list_cat), size = 0.5) +
  geom_sf(fill = "transparent", color = "yellow", size = 2,
          data=adm1_shp %>% st_simplify()) +
  # geom_sf(fill = "transparent", color = "black", size = 1,
  #         data=iu_shp) +
  labs(fill='') +
  theme(legend.position = 'top') +
  theme_minimal()
library(tidyverse)
library(sf)

high_ranking_map <- commune_shp %>% 
  mutate(list_cat=case_when((adm3_pcode %in% longlist & !adm3_pcode %in% shortlist)~ "Longlist",
                            adm3_pcode %in% shortlist ~ 'Shortlist')) %>% 
  filter(!is.na(list_cat)) %>% 
  ggplot(data=.) +
  geom_sf(aes(fill=list_cat), size = 0.5, linetype="dotted", size=0.7) +
  # geom_sf(fill = "transparent", color = "dark grey", size = 1.2,
  #         data=iu_shp %>% st_simplify()) +
  geom_sf(fill = "transparent", color = "black", size = 2.5,
          data=adm1_shp %>% st_simplify()) +
  labs(fill='', title='Identifying priority areas for onchocercisis elimination mapping in Angola',
       subtitle='Borders: Thick black line = Province, Thin grey line = Municipality, Dashed line = Commune ') +
  theme(legend.position = 'top') +
  theme_minimal() 

cairo_ps(filename = "images/angola_oem_allborders.eps",
         width = 7, height = 7, pointsize = 12,
         fallback_resolution = 300)
print(p)
dev.off()

ggsave("images/angola_oem_province.pdf", high_ranking_map, width=25, height=25, units="cm", dpi=300)

library(leaflet)

listed_communes <- commune_shp %>% 
  mutate(list_cat=case_when((adm3_pcode %in% longlist & !adm3_pcode %in% shortlist)~ "Longlist",
                                                             adm3_pcode %in% shortlist ~ 'Shortlist')) %>% 
  filter(!is.na(list_cat))

pal <- colorFactor(
  palette = c('#F8766D', '#00BFC4'),
  domain = listed_communes$list_cat
)


label_data <- listed_communes %>% 
  as_tibble() %>% 
  select(adm1_en, adm2_en, adm3_en)
labs <- lapply(seq(nrow(label_data)), function(i) {
  paste0( '<p>', 'Province:', label_data[i, "adm1_en"], '<p></p>', 
          'Municipality:', label_data[i, "adm2_en"],'</p><p>', 
          'Commune:', label_data[i, "adm3_en"], '</p>' ) 
})

library(htmltools)
leaflet(data = listed_communes) %>% 
  addTiles(group='OpenStreetMap') %>% 
  addBingTiles('AiiOX4mbrgeTPwQbrDVOhP8WSiIMcWDa6zErvThpZ4dvbCF0ddyi_srF85u_SJNK', group='Satellite') %>% 
  addPolygons(data=adm1_shp, 
              weight = 9,
              opacity = 1,
              color = "black",
              fillOpacity = 0, 
              group="Province") %>% 
  addPolygons(data=iu_shp, 
              weight = 3,
              opacity = 1,
              color = "black",
              fillOpacity = 0,
              group="Municipality"
              ) %>% 
  addPolygons(data=listed_communes, 
              color =~pal(list_cat),
              weight = 2,
              opacity=1,
              dashArray = "3", 
              label = lapply(labs, htmltools::HTML),
              group="Commune") %>% 
  addLayersControl(
    baseGroups = c("OpenStreetMap (default)", "Satellite"),
    #overlayGroups = c("Province", "Municipality"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend("bottomright", pal = pal, values = ~list_cat,
            title = "",
            opacity = 1
  )

              
pacman::p_load("leaflet.extras")

full_table <- df_merge %>% 
  mutate(list=case_when(adm3_pcode %in% shortlist ~ 'Shortlist')) %>% 
  select(province=adm1_en, municipality=adm2_en, commune=adm3_en,population=total_pop,
         env_suit=oncho_mean_aw, 
         propover71,
         lf_treatment,
         list,
         SurveyYear, Prevalence,
         case_reported,neighbours_endemic_iu) %>% 
  mutate(province=gsub("_", " ", province)) %>% 
  mutate(municipality=gsub("_", " ", municipality)) %>% 
  mutate(commune=gsub("_", " ", commune)) %>% 
  arrange(desc(env_suit),desc(propover71),desc(neighbours_endemic_iu))

saveRDS(full_table, here('tables', 'full_table.Rds'))


write_csv(full_table, here('data', 'output', 'full_table.csv'))

filtered_table <- df_merge %>% 
  filter(adm3_pcode %in% high_ranking) %>% 
  select(province=adm1_en, municipality=adm2_en, commune=adm3_en,population=total_pop,
         env_suit=oncho_mean_aw, 
         propover71,
         lf_treatment,
         SurveyYear,DiagnosticMethod, Examined, Positive, Prevalence,
         case_reported,neighbours_endemic_iu) %>% 
  mutate(province=gsub("_", " ", province)) %>% 
  mutate(municipality=gsub("_", " ", municipality)) %>% 
  mutate(commune=gsub("_", " ", commune)) %>% 
  arrange(desc(env_suit,propover71,neighbours_endemic_iu))


write.csv(filtered_table, here('data', 'output', 'filtered_table.csv'))
