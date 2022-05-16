library(tidyverse)
library(here)
library(janitor)
library(sf)
library(linelist)
library(gt)
library(flextable)
ang_shp <- read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>% 
  filter(ADMIN0=='Angola')

commune_shp <- read_sf(here('data', 'input', 'shp', 'ago_admbnda_adm3_gadm_ine_ocha_20180904.shp')) %>% 
  select(ADM1_EN, ADM1_PCODE, ADM2_EN,ADM2_PCODE, ADM3_EN,ADM3_PCODE, geometry) %>% 
  clean_data()

adm1_shp <- commune_shp %>%
  group_by(adm1_en) %>%
  summarise()

ang_crs <- st_crs(ang_shp)

angola_oncho_dados_raw <- readxl::read_excel(here('data', 
                                                  'input', 
                                                  'Oncho DADOS GERAL DE TODOS OS MAPEAMENTOS DA ONCHO Angola SL.xlsx'), sheet='ONCHO - DESDE 2002 ate 2020') 


angola_oncho_espen_sitelevel <- read.csv(here('data', 'input', 'espen_platform_oncho_sitelevel.csv')) %>% 
  mutate(IU_ID=as.numeric(IU_ID))

oncho_wd <-
  read.csv(here('data', 'input', 'espen_platform_oncho.csv')) %>%
  filter(Year == 2020) %>% 
  select(IU_ID, Endemicity, population=PopTot) 

espen_sitelevel_sf <- angola_oncho_espen_sitelevel %>% 
  st_as_sf(., coords = c("Longitude", "Latitude"),
           crs= 4326) %>% 
  left_join(oncho_wd, by='IU_ID') 

espen_sitelevel_sf %>% as_tibble() %>% 
  filter(ADMIN1_NAME=='Benguela') %>% 
  #select(Endemicity,Positive,Examined) %>% 
  #filter(Examined>=50) %>% 
  #group_by(Endemicity) %>% 
  # summarise(Positive=sum(Positive, na.rm=TRUE),
  #           Examined=sum(Examined,na.rm=TRUE)) %>% 
mutate(rate = map2(Positive, Examined, ~ prop.test(.x, .y, conf.level=0.95) %>%
                     broom::tidy())) %>%
  unnest(rate) %>%
  ungroup() %>% 
  ggplot(aes(x=SurveyYear, y=estimate)) +
  geom_point(position="jitter") +
  # geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
  #               position = position_dodge(width = 0.1)) +
  theme_minimal()

espen_sitelevel_sf %>% 
  ggplot(aes(x=Examined, y=Positive, color=Endemicity)) +
  geom_point() +
  facet_wrap(~Period, scales='free_x')

source(here('admin_db.R'))

commune_shp_linked <- commune_shp %>% 
  inner_join(final_linked_db %>% 
               select('adm1_pcode', 'adm2_pcode', 'adm3_pcode','iu_id', 'total_pop'),
             by=c('adm1_pcode', 'adm2_pcode', 'adm3_pcode')) %>% 
  distinct(adm1_pcode, adm2_pcode, adm3_pcode, .keep_all=TRUE)


# Import oncho data from ESPEN
oncho_wd <-
  read.csv(here('data', 'input', 'espen_platform_oncho.csv')) %>%
  filter(Year == 2020) %>% 
  select(IU_ID, Endemicity, population=PopTot) 

commune_oncho_shp <- commune_shp_linked %>% 
  inner_join(oncho_wd, by=c('iu_id' = 'IU_ID')) 
# 
#   filter(grepl('Unknown', Endemicity)) %>% 
#   ggplot() +
#   geom_sf(aes(color = Endemicity), show.legend = "point") +
#   geom_sf(fill = "transparent", color = "black", size = .1,
#           data = adm1_shp) +
#   facet_wrap(~Period)

iu_join <- st_join(espen_sitelevel_sf, ang_shp)



df <- iu_join %>% group_by(IUs_NAME) %>% 
  filter(SurveyYear==max(SurveyYear))
  #filter(Examined==max(Examined)) %>% 
  #filter(Positive==max(Positive)) 

df %>% as_tibble() %>% 
  filter(!is.na(Endemicity)) %>% 
  group_by(Endemicity) %>% 
  summarise(prevalence=mean(Prevalence,na.rm=TRUE),
            min_year=min(SurveyYear,na.rm = TRUE),
            max_year=max(SurveyYear,na.rm=TRUE))

x <- df %>% as_tibble() %>% filter(grepl('Unknown', Endemicity)) %>% 
  select(IUs_NAME,Endemicity,SurveyYear,Positive,Examined)



iu_join %>% as_tibble() %>% 
  filter(Examined>=50) %>% 
  filter(!is.na(Endemicity)) %>% 
  group_by(SurveyYear,Endemicity) %>% 
  summarise(total_positive=sum(Positive),
            total_examined=sum(Examined), 
    rate = map2(total_positive, total_examined, ~ prop.test(.x, .y, conf.level=0.95) %>%
                     broom::tidy())) %>%
  unnest(rate) %>% 
  ggplot() +
  geom_line(aes(x=SurveyYear, y=estimate)) +
  facet_wrap(~Endemicity)

commune_join <- st_join(espen_sitelevel_sf, commune_oncho_shp)

commune_prevalence_cat_table <- commune_join %>% 
  filter(grepl('Unknown', Endemicity.x)) %>% 
  mutate(prevalence_cat=case_when(Prevalence==0 ~ '0',
                                   (Prevalence>0 & Prevalence<0.05) ~ '>0 & <5',
                                   Prevalence>=0.05 ~ '>=5')) %>% 
  as_tibble() %>% 
  mutate(prevalence_cat=factor(prevalence_cat, levels=c('0','>0 & <5', '>=5'))) %>% 
  tabyl(prevalence_cat,Period) %>%   adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns()


saveRDS(commune_prevalence_cat_table, here('tables', 'commune_prevalence_cat_table.Rds'))

  #filter(grepl('sumbe', adm2_en)) %>% 
  ggplot(.) +
  geom_sf(fill="red") +
  #geom_sf(data=adm1_shp,fill = "transparent", color = "black", size = 1.5)
  geom_sf(
          #%>% filter(grepl('sumbe', adm2_en)),
          aes(fill = prevaelence_cat))

commune_join %>% tabyl(Examined, Endemicity)

