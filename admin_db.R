# ADMIN CODES
library(here)
library(sf)
library(tidyverse)
library(linelist)

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
                            TRUE ~ adm2_en))

iu_admin_codes <-
  read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>%
  as_tibble() %>% 
  filter(ADMIN0 == 'Angola') %>% 
  clean_data() %>% 
  select(admin1, admin1id, admin2, admin2id, iu_id) %>% 
  mutate(admin1= case_when(admin1=='kwanza_sul' ~ "cuanza_sul", 
                            TRUE ~admin1))

linking_db <- commune_admin_codes %>% 
  right_join(iu_admin_codes, by=(c('adm1_en' = 'admin1', 'adm2_en' = 'admin2')))

rm(commune_admin_codes, iu_admin_codes)
