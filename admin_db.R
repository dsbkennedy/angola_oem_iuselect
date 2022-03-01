# ADMIN CODES


commune_admin_codes <- read_sf(here('data', 'input', 'shp', 'ago_admbnda_adm3_gadm_ine_ocha_20180904.shp')) %>%  
  as_tibble() %>% 
  clean_data() %>% 
  select(adm1_en, adm1_pcode, adm2_en, adm2_pcode, adm3_en, adm3_pcode)  %>% 
  mutate(adm2_en = case_when(adm2_en=='amboim_gabela' ~ 'amboim',
                             adm2_en=='belas_samba' ~ 'belas',
                            adm2_en=='cacongo_landana' ~ 'cacongo',
                            adm2_en=='cangola_alto_cauale' ~ 'cangola',
                            adm2_en=='cela_waku_kungo' ~ 'cela',
                            adm2_en=='chitato_lovua' ~ 'chitato',
                            adm2_en=='dembos_quibaxe' ~ 'dembos',
                            adm2_en=='ecunha_ekunha' ~ 'ekunha',
                            adm2_en=='tombwa_porto_alexandre' ~ 'tombua',
                            adm2_en=='tchinjenje' ~ 'tchindjenje',
                            adm2_en=='sumbe_ngangula' ~ 'sumbe',
                            adm2_en=='seles_uku_seles' ~ 'seles',
                            adm2_en=='quipungo_tchipungo' ~ 'quipungo',
                            adm2_en=='mbanza_kongo' ~ 'mbanza_congo',
                            adm2_en=='londuimbali' ~ 'londuimbale',
                            adm2_en=='libolo_calulo' ~ 'libolo',
                            adm2_en=='kiwaba_n_zogi' ~ 'kiuaba_n_zoji_cuaba_nzogo',
                            adm2_en=='gambos_ex_chiange' ~ 'gambos',
                            adm2_en=='cuito_kuito' ~ 'kuito',
                            adm2_en=='n_harea' ~ 'nharea',
                            adm2_en=='gambos_ex_chiange' ~ 'gambos',
                            adm2_en=='cambambe_kambambe' ~ 'kambambe',
                            adm2_en=='cazengo' ~ 'kazengo',
                            adm2_en=='quiculungo' ~ 'kiculungo',
                            adm2_en=='lucala' ~ 'lukala',
                            adm2_en=='quibala' ~ 'kibala',
                            adm2_en=='quilenda' ~ 'kilenda',
                            adm2_en=='n_zeto' ~ 'nzeto',
                            adm2_en=='mbanza_congo' ~ 'mbanza_kongo',
                            adm2_en=='cameia_lumeje' ~ 'lumege',
                            adm2_en=='luena' ~ 'moxico',
                            adm2_en=='bundas_lumbala_nguimbo' ~ 'lumbala_nguimbo',
                            adm2_en=='calandula' ~ 'kalandula',
                            adm2_en=='kiuaba_n_zoji_cuaba_nzogo' ~ 'kiwaba_n_zogi',
                            adm2_en=='cunda_dia_baze' ~ 'kunda_ia_baze',
                            adm2_en=='cuvango_kuvango' ~ 'kuvango',
                            adm2_en=='catchiungo' ~ 'kachiungo',
                            adm2_en=='londuimbale' ~ 'londuimbali',
                            adm2_en=='quela' ~ 'khela',
                            adm2_en=='mucari' ~ 'caculama',
                            adm2_en=='cuvango_kuvango' ~ 'kuvango',
                            adm2_en=='quissama_muxima_quicama' ~ 'kissama',
                            #adm2_en=='quissama_muxima_quicama' ~ 'talatona',
                            #adm2_en=='luanda' ~ 'kilamba_kiaxi',
                            #adm2_en=='chitato_lovua' ~ 'lovua',
                            TRUE ~ adm2_en))


iu_admin_codes <-
  read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>%
  as_tibble() %>% 
  filter(ADMIN0 == 'Angola') %>% 
  clean_data() %>% 
  select(admin1, admin1id, admin2, admin2id, iu_id) %>% 
  mutate(admin1= case_when(admin1=='kwanza_sul' ~ "cuanza_sul", 
                            TRUE ~admin1))


master_list <- iu_admin_codes %>% distinct(admin1, admin2) %>% mutate(iu_data=1) %>%
  full_join(commune_admin_codes %>% distinct(adm1_en, adm2_en) %>% mutate(commune_data=1),
            by=(c('admin1' = 'adm1_en', 'admin2'='adm2_en'))) %>%
  arrange(admin1, admin2)


  # mutate(admin1= case_when(admin1=='kwanza_sul' ~ "cuanza_sul", 
  #                           TRUE ~admin1)) %>% 
  # mutate(admin2 = case_when(admin2=='amboim' ~ 'amboim_gabela',
  #                           admin2=='belas' ~ 'belas_samba',
  #                           admin2=='cacongo' ~ 'cacongo_landana',
  #                           admin2=='cangola' ~ 'cangola_alto_cauale',
  #                           admin2=='cela' ~ 'cela_waku_kungo',
  #                           admin2=='chitato' ~ 'chitato_lovua',
  #                           admin2=='dembos' ~ 'dembos_quibaxe',
  #                           admin2=='ekunha' ~ 'ecunha_ekunha',
  #                           admin2=='tombua' ~ 'tombwa_porto_alexandre',
  #                           admin2=='tchindjenje' ~ 'tchinjenje',
  #                           admin2=='sumbe' ~ 'sumbe_ngangula',
  #                           admin2=='seles' ~ 'seles_uku_seles',
  #                           admin2=='quipungo' ~ 'quipungo_tchipungo',
  #                           admin2=='mbanza_congo' ~ 'mbanza_kongo',
  #                           admin2=='londuimbale' ~ 'londuimbali',
  #                           admin2=='libolo' ~ 'libolo_calulo',
  #                           admin2=='kiuaba_n_zoji_cuaba_nzogo' ~ 'kiwaba_n_zogi',
  #                           admin2=='gambos' ~ 'gambos_ex_chiange',
  #                           admin2=='kuito' ~ 'cuito_kuito',
  #                           admin2=='nharea' ~ 'n_harea',
  #                           admin2=='gambos' ~ 'gambos_ex_chiange',
  #                           admin2=='kambambe' ~ 'cambambe_kambambe',
  #                           admin2=='kazengo' ~ 'cazengo',
  #                           admin2=='kiculungo' ~ 'quiculungo',
  #                           admin2=='lukala' ~ 'lucala',
  #                           admin2=='kibala' ~ 'quibala',
  #                           admin2=='kilenda' ~ 'quilenda',
  #                           admin2=='nzeto' ~ 'n_zeto',
  #                           admin2=='mbanza_kongo' ~ 'mbanza_congo',
  #                           admin2=='lumege' ~ 'cameia_lumeje',
  #                           admin2=='moxico' ~ 'luena',
  #                           admin2=='lumbala_nguimbo' ~ 'bundas_lumbala_nguimbo',
  #                           admin2=='kalandula' ~ 'calandula',
  #                           admin2=='kiwaba_n_zogi' ~ 'kiuaba_n_zoji_cuaba_nzogo',
  #                           admin2=='kunda_ia_baze' ~ 'cunda_dia_baze',
  #                           admin2=='kuvango' ~ 'cuvango_kuvango',
  #                           admin2=='kachiungo' ~ 'catchiungo',
  #                           admin2=='londuimbali' ~ 'londuimbale',
  #                           admin2=='khela' ~ 'quela',
  #                           admin2=='caculama' ~ 'mucari',
  #                           admin2=='kuvango' ~ 'cuvango_kuvango',
  #                           admin2=='kissama' ~ 'quissama_muxima_quicama',
  #                           admin2=='talatona' ~ 'quissama_muxima_quicama',
  #                           admin2=='kilamba_kiaxi' ~ 'luanda',
  #                           admin2=='lovua' ~ 'chitato_lovua',
  #                           TRUE ~ admin2))

linking_db <- commune_admin_codes %>% 
  right_join(iu_admin_codes, by=(c('adm1_en' = 'admin1', 'adm2_en' = 'admin2')))

# master_list <- commune_admin_codes %>% distinct(adm1_en,adm2_en) %>% mutate(commune_data=1) %>% 
#   right_join(iu_admin_codes %>% distinct(admin1,admin2) %>% mutate(iu_data=1), 
#             by=(c('adm1_en' = 'admin1', 'adm2_en' = 'admin2'))) %>% 
#   arrange(adm1_en,adm2_en)
