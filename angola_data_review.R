library(tidyverse)
library(here)
library(janitor)
library(sf)
library(linelist)

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

commune_mapping_filter <- commune_join %>% as_tibble() %>%   filter(Method_1=='Parasitological') %>% 
  distinct(adm2_pcode, adm3_pcode) %>% 
  select(adm2_pcode, adm3_pcode)

saveRDS(commune_mapping_filter, here('data', 'output', 'commune_mapping_filter.Rds'))

names(commune_join)

df <- commune_join %>% 
  filter(grepl('Unknown', Endemicity.x)) %>% 
  mutate(prevaelence_cat=case_when(Prevalence==0 ~ '0',
                                   (Prevalence>0 & Prevalence<0.05) ~ '>0 & <5',
                                   Prevalence>=0.05 ~ '>=5')) %>% 
  as_tibble() %>% filter(prevaelence_cat=='>=5' & Period=='2011 - 2016')
  
df %>% 
tabyl(Period,prevaelence_cat,Method_2) 
  #filter(grepl('sumbe', adm2_en)) %>% 
  ggplot(.) +
  geom_sf(fill="red") +
  #geom_sf(data=adm1_shp,fill = "transparent", color = "black", size = 1.5)
  geom_sf(
          #%>% filter(grepl('sumbe', adm2_en)),
          aes(fill = prevaelence_cat))

commune_join %>% tabyl(Examined, Endemicity)

angola_oncho_dados_clean <- angola_oncho_dados_raw %>%
  clean_data() %>%
  filter(!is.na(latitude_decimal_degrees)) %>%
  filter(!is.na(longitude_decimal_degrees)) %>%
  mutate(
    latitude_clean1 = str_replace(latitude_decimal_degrees, 's_|s', ''),
    latitude_clean2 = str_replace(latitude_clean1, '^_', ''),
    latitude_clean3 = as.numeric(str_replace(latitude_clean2, '_', '.')),
    latitude_clean4 = case_when((latitude_clean3 > 10000) ~ latitude_clean3 /
                                  10000,
                                (latitude_clean3 > 1000 &
                                   latitude_clean3 < 10000) ~ latitude_clean3 / 1000,
                                TRUE ~ latitude_clean3
    ),
    latitude_final = case_when(
      latitude_clean1 == '14239' ~ 14.239,
      latitude_clean1 == '1268677' ~ 12.68677,
      latitude_clean1 == '13038' ~ 13.038,
      latitude_clean1 == '1096825' ~ 10.96825,
      TRUE ~ latitude_clean4
    ),
    latitude_final=latitude_final*-1
  ) %>%
  mutate(
    longitude_clean1 = str_replace(longitude_decimal_degrees, 'e_|e', ''),
    longitude_clean2 = str_replace(longitude_clean1, '^_', ''),
    longitude_clean3 = as.numeric(str_replace(longitude_clean2, '_', '.')),
    longitude_final = case_when(
      longitude_clean1 == '16534' ~ 16.534,
      longitude_clean1 == '172639' ~ 17.2639,
      longitude_clean1 == '1546165' ~ 15.46165,
      longitude_clean1 == '153117' ~ 15.3117,
      longitude_clean1 == '145117' ~ 14.5117,
      longitude_clean1 == '145448' ~ 14.5448,
      TRUE ~ longitude_clean3
    )
  ) %>%
  select(-contains('_clean')) %>% 
  st_as_sf(., coords = c("longitude_final", "latitude_final"),
           crs= 4326)

commune_survey_data <- commune_join %>% 
  as_tibble() %>% 
  #filter(Method_1=='Parasitological') %>% 
  group_by(adm1_en, adm2_en, adm3_en) %>% 
  filter(SurveyYear==max(SurveyYear, na.rm=TRUE)) %>% 
  filter(Examined==max(Examined, na.rm=TRUE)) %>% 
  filter(Prevalence==max(Prevalence, na.rm=TRUE)) %>% 
  distinct(SurveyYear,Examined,Positive,Prevalence,DiagnosticMethod=Method_1) %>% 
  select(DiagnosticMethod,SurveyYear,Examined,Positive, Prevalence)

saveRDS(commune_survey_data, here('data', 'output', 'commune_survey_data.Rds'))


angola_oncho_dados_clean %>% 
  #filter(name_of_administrative_level_2_implementation_unit=="camacupa") %>% 
  ggplot() + 
  #geom_point(aes(x=longitude_final, y=latitude_final))
  geom_sf(aes(color = name_of_administrative_level_1_state_province_region), show.legend = "point") +
  geom_sf(fill = "transparent", color = "black", size = .1,
          data = adm1_shp) +
  theme(legend.position="none") +
  facet_wrap(~name_of_administrative_level_1_state_province_region )


province_join <- st_join(angola_oncho_dados_clean, adm1_shp)
iu_join <- st_join(angola_oncho_dados_clean, ang_shp)
commune_join <- st_join(angola_oncho_dados_clean, commune_shp)


province_join %>% 
  as_tibble() %>% 
  group_by(adm1_en,year_of_mapping) %>% 
  summarise(total_examined=sum(number_of_people_examined_14, na.rm=TRUE),
            total_positive=sum(number_of_people_positive_16, na.rm=TRUE)) %>% 
  mutate(positive=total_positive/total_examined) %>% 
  left_join(adm1_shp,., by=c('adm1_en'))  %>% 
  ggplot() +
  geom_sf(aes(fill="positive")) +
  facet_wrap(~year_of_mapping )

iu_join %>% as_tibble() %>% 
  group_by(ADMIN1, ADMIN2,year_of_mapping) %>% 
  summarise(total_examined=sum(number_of_people_examined_14, na.rm=TRUE),
            total_positive=sum(number_of_people_positive_16, na.rm=TRUE))

commune_prevalence <- commune_join %>% as_tibble() %>% 
  group_by(adm1_en, adm2_en,adm3_en,year_of_mapping, .drop = FALSE) %>% 
  summarise(total_examined=sum(number_of_people_examined_14, na.rm=TRUE),
            total_positive=sum(number_of_people_positive_16, na.rm=TRUE)) %>% 
  mutate(positive=total_positive/total_examined) %>% 
  select(-c(contains('total'))) %>% 
  inner_join(commune_shp_linked, by=c('adm1_en', 'adm2_en','adm3_en')) %>% 
  left_join(oncho_loa_loa_lf_data, by=c('iu_id' = 'IU_ID'))

x <- commune_prevalence %>% as_tibble() %>% 
  group_by(Endemicity,year_of_mapping) %>% 
  summarise(average_positive=mean(positive,na.rm=TRUE))

x <- commune_prevalence %>% as_tibble() %>% filter(year_of_mapping==2015) %>% 
  group_by(adm1_en, adm2_en,adm3_en) %>% 
  summarise(total_examined=sum(number_of_people_examined_14, na.rm=TRUE),
            total_positive=sum(number_of_people_positive_16, na.rm=TRUE))

# check if 2015 endemicity data is correct 


commune_no_survey <- commune_prevalence %>% 
  distinct(adm1_en, adm2_en,adm3_en) %>% 
  mutate(no_surveys=1)

commune_prevalence %>% ggplot() +
  geom_point(aes(x=year_of_mapping, y=positivity)) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = .4) +
  stat_summary(fun = "mean", geom = "point")


nosurvey <- commune_shp %>% 
  anti_join(commune_prevalence, by=c('adm1_en', 'adm2_en','adm3_en')) %>% 
  as_tibble() %>% 
  distinct(adm1_en, adm2_en,adm3_en) %>% 
  mutate(no_surveys=1)

angola_commune_oncho_noloaloa_over71 %>% 
  inner_join(nosurvey,by=c('adm1_en', 'adm2_en','adm3_en')) %>% 
  ggplot() +
  geom_sf(aes(fill = propover71)) +
  geom_sf(fill = "transparent", color = "black", size = 1.5,
          data = adm1_shp) +
  labs(fill="% of pixels (5x5km) in a commune exceeding environmental threshold >=0.71") +
  theme(legend.position = "top") +
  scale_fill_viridis_c(option = "magma", begin=0, end=1)


angola_commune_oncho_noloaloa_over71 %>% inner_join(nosurvey,by=c('adm1_en', 'adm2_en','adm3_en'))
  

commune_shp %>% 
  anti_join(commune_prevalence, by=c('adm1_en', 'adm2_en','adm3_en')) %>% 
  ggplot() + 
  #geom_point(aes(x=longitude_final, y=latitude_final))
  geom_sf()

head(province_join)
names(province_join)
 

df!

df2 <- df1 %>% filter(!between(latitude_final, 4, 18) ) %>% 
  mutaet(latit)

  arrange(desc(latitude_final))

  mutate(prevalence=number_of_people_positive_16/number_of_people_examined_14) %>% 
  #mutate(prevalence=as.numeric(percent_positive_17)) 
  mutate(latitude_clean=ifelse(latitude_clean<0,latitude_clean*-1,latitude_clean)) %>% 
  filter(latitude_clean <18 & latitude_clean>4) %>% 
  mutate(latitude_clean=latitude_clean*-1) %>% 
  st_as_sf(., coords = c("longitude_clean", "latitude_clean"),
           crs= ang_crs, agr = "constant")



joined <- st_join(angola_oncho_dados_clean, ang_shp)

st_is_longlat(angola_oncho_dados_clean)

angola_geo = st_set_crs(angola_oncho_dados_clean, 4326)
st_is_longlat(angola_geo)

st_distance(ang_shp, angola_geo)

angola_oncho_dados_clean %>% 
  #filter(name_of_administrative_level_2_implementation_unit=="Huambo") %>% 
ggplot() + 
  #geom_point(aes(x=longitude_clean, y=latitude_clean))
  geom_sf(aes(color = prevalence), show.legend = "point") +
  geom_sf(fill = "transparent", color = "black", size = .1,
          data = ang_shp) 


ggplot() + 
  geom_sf(fill = "transparent", color = "black", size = .1,
          data = ang_shp)
  #geom_sf(data = angola_oncho_dados_clean, aes(size = prevalence), color = "black", show.legend = "point")


  
library(sp)
coordinates(angola_oncho_dados_clean) <- c('longitude_clean', 'latitude_clean') #this is the line which answers the question.
proj4string(angola_oncho_dados_clean) <- CRS("+init=epsg:4326")
str(angola_oncho_dados_clean)
