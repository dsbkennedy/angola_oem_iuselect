

short_long_list_fn <- function(x) {
  commune_over71 <-  angola_commune_oncho_loaloa_over71 %>%
    as_tibble() %>% 
    select(
      "adm1_pcode",
      "adm2_pcode",
      "adm3_pcode",
      "adm1_en",
      "adm2_en",
      "adm3_en",
      propover71,
      total_pop,
      Endemicity, 
      loaloa_status
    )
  
  commune_neighbour_endemic <- neighbour_endemic_area %>%
    as_tibble() %>%
    select(adm1_en, adm2_en, adm3_en) %>%
    mutate(neighbours_endemic_iu = 'Yes')
  
  commune_survey_data <-
    readRDS(here('data', 'output', 'commune_survey_data.Rds')) %>%
    group_by(adm1_en, adm2_en, adm3_en) %>%
    filter(SurveyYear == max(SurveyYear))
  
  commune_over71_merge_commune_neighbour_endemic <-
    commune_over71 %>%
    left_join(commune_neighbour_endemic,
              by = c("adm1_en", "adm2_en", "adm3_en")) %>%
    left_join(commune_survey_data, by = c("adm1_en", "adm2_en", "adm3_en"))
  
  df_merge <- commune_over71_merge_commune_neighbour_endemic %>%
    left_join(commune_oncho_mean_aw,
              by = c("adm1_pcode", "adm2_pcode", "adm3_pcode")) %>%
    mutate(lf_treatment = case_when(grepl('lf', Endemicity, ignore.case =
                                            T) ~ 'Yes')) %>%
    left_join(case_reported,
              by = c('adm1_en' = 'province', 'adm2_en' = 'municipality')) %>%
    arrange(adm1_en, adm2_en) %>%
    mutate(
      propover71 = propover71 / 100,
      oncho_mean_aw = oncho_mean_aw / 100,
      total_pop = total_pop / 1000
    )
  
  
  shortlist_df <- df_merge %>%
    #filter(loaloa_status=='Non-endemic') %>% 
    mutate(env_rank = dense_rank(desc(oncho_mean_aw))) %>% 
    mutate(prop_rank = dense_rank(desc(propover71))) %>% 
    mutate(prev_rank = dense_rank(desc(Prevalence))) %>% 
   #  
   # # bind_rows(prop_rank_df, prev_rank_df) %>%
   #  left_join(prop_rank_df, by=c('adm1_en', 'adm2_en', 'adm3_en', 'adm3_pcode')) %>% 
   #  left_join(prev_rank_df, by=c('adm1_en', 'adm2_en', 'adm3_en', 'adm3_pcode')) %>% 
    rowwise() %>%
    mutate(sum = sum(c_across(env_rank:prev_rank), na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(overall_rank = dense_rank((sum))) 

  return(shortlist_df)
}

shortlist <- short_long_list_fn() %>% 
  filter(sum <=100) %>%
  pull(adm3_pcode)

  
