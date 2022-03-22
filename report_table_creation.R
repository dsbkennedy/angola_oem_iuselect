


final_table <- angola_commune_oncho_noloaloa_over71 %>% as_tibble() %>% 
  select(province=adm1_en, municipality=adm2_en, commune=adm3_en, population, Endemicity )



df1 <- angola_commune_oncho_collapse %>% as_tibble() %>% 
  select( "adm1_pcode","adm2_pcode","adm3_pcode","oncho_mean_aw" )

df2 <- angola_commune_oncho_noloaloa_over71 %>% as_tibble() %>% 
  select("adm1_pcode","adm2_pcode","adm3_pcode","adm1_en","adm2_en","adm3_en",propover71,total_pop, Endemicity )

df3 <- neighbour_endemic_area %>% as_tibble() %>% select(adm1_en, adm2_en, adm3_en) %>% mutate(neighbours_endemic_iu='Yes')

commune_survey_data <- readRDS(here('data','output','commune_survey_data.Rds'))
commune_survey_data %>% count(adm1_en, adm2_en, adm3_en) %>% filter(n>1)

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

high_ranking <- env_rank_df %>% bind_rows(prop_rank_df) %>% 
  count(adm1_pcode, adm2_pcode, adm3_pcode) %>% 
  filter(n>1) %>% 
  pull(adm3_pcode)

high_ranking_map <- commune_shp %>% filter(adm3_pcode %in% high_ranking) %>% 
  ggplot() +
  geom_sf(fill = "red", size = 0.5) +
  geom_sf(fill = "transparent", color = "black", size = 1.5,
          data=adm1_shp)

high_ranking_map

full_table <- df_merge %>% 
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
