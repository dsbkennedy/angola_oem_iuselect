

angola_commune_oncho_noloaloa_over71

final_table <- angola_commune_oncho_noloaloa_over71 %>% as_tibble() %>% 
  select(province=adm1_en, municipality=adm2_en, commune=adm3_en, population, Endemicity )

names(final_table)



names(angola_commune_oncho_collapse)

df1 <- angola_commune_oncho_collapse %>% as_tibble() %>% 
  select( "adm1_pcode","adm2_pcode","adm3_pcode","oncho_mean_aw" )

df2 <- angola_commune_oncho_noloaloa_over71 %>% as_tibble() %>% 
  select("adm1_pcode","adm2_pcode","adm3_pcode","adm1_en","adm2_en","adm3_en",propover71,total_pop, Endemicity )

df_merge <- df2 %>% left_join(df1, by=c("adm1_pcode","adm2_pcode","adm3_pcode")) %>% 
  select(province=adm1_en, municipality=adm2_en, commune=adm3_en,Endemicity,population=total_pop,env_suit=oncho_mean_aw, propover71) %>% 
  mutate(province=gsub("_", " ", province)) %>% 
  mutate(municipality=gsub("_", " ", municipality)) %>% 
  mutate(commune=gsub("_", " ", commune))



write.csv(df_merge, here('data', 'output', 'df_merge.csv'))
