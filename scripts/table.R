
library(tidyverse)
library(here)
library(janitor)

oncho_raw <- read.csv(here('data', 'input', 'espen_platform_oncho.csv'))

oncho_wd <- oncho_raw %>%  rename(oncho_status=Endemicity,
                                  oncho_mda=MDA_scheme) 

loa_loa_raw <- read.csv(here('data', 'input', 'espen_platform_loaloa.csv')) 

loa_loa_wd <- loa_loa_raw %>% 
  rename(loaloa_status=Endemicity)  
  #filter(Year==2020) %>% 
  #select(ADMIN1,IU_ID, IUs_NAME, loaloa_status)

lf_raw <- read.csv(here('data', 'input', 'espen_platform_lf.csv')) 

lf_wd <- lf_raw %>% 
  rename(lf_status=Endemicity,
         lf_mda=MDA_scheme)  
  #filter(Year==2020) %>% 
  #select(ADMIN1,IU_ID, IUs_NAME, lf_status)

all_diseases <- oncho_wd %>% 
  # filter(Year==2020) %>% 
  # select(ADMIN1,IU_ID, IUs_NAME, oncho_status) %>% 
  left_join(loa_loa_wd, by=c('ADMIN1', 'IU_ID', 'Year')) %>% 
  left_join(lf_wd, by=c('ADMIN1','IU_ID','Year'))

mismatch_mda <- all_diseases %>% 
  filter(oncho_status=="Unknown (under LF MDA)" & lf_status=="Non-endemic") %>% 
  select(ADMIN1, IU_ID, IUs_NAME,oncho_status,lf_status,Year,oncho_mda, lf_mda)

mismatch_mda_id <- mismatch_mda %>% pull(IU_ID)
  

all_diseases_summary <- all_diseases %>% 
  # filter(Year==2020) %>% 
  # count(ADMIN1,oncho_status,loaloa_status,lf_status, oncho_mda, lf_mda) %>% 
  mutate(oncho_endemic=case_when(grepl("Endemic", oncho_status) ~"Yes",
                                 TRUE ~ "Unknown"),
         # oncho_mda=case_when(oncho_status=="Endemic (under MDA)" ~ "Yes", 
         #                     oncho_status=="Unknown (under LF MDA)" ~ "LF MDA",
         #                     TRUE ~ "No"),
         loaloa_endemic=case_when(grepl("Endemic", loaloa_status) ~"Yes",
                                  TRUE ~ "No"),
         lf_endemic=case_when(grepl("\\<Endemic\\>", lf_status) ~"Yes",
                              grepl("\\<Non-endemic\\>", lf_status) ~"No",
                              TRUE ~ "Unknown"))
         # lf_mda=case_when(grepl("\\<under MDA\\>", lf_status) ~"Yes",
         #                  grepl("\\<MDA not delivered\\>", lf_status) ~ "No",
         #                  TRUE ~"No"))
library(gt)                             

response_levels <- c("Yes", "No", "Unknown")
all_diseases_summary %>%  
  filter(Year==2020) %>% 
  count(oncho_endemic,loaloa_endemic,lf_endemic, oncho_mda, lf_mda) %>% 
  select(oncho_endemic,loaloa_endemic,lf_endemic,oncho_mda,lf_mda,n) %>% 
  mutate(oncho_endemic= fct_relevel(oncho_endemic, "Yes", "Unknown")) %>% 
  mutate(loaloa_endemic= fct_relevel(loaloa_endemic, "Yes", "No")) %>% 
  mutate(lf_endemic= fct_relevel(lf_endemic, "Yes", "No", "Unknown")) %>% 
  #mutate(oncho_mda= fct_relevel(oncho_mda, "Yes", "No", "LF MDA")) %>% 
  #mutate(lf_mda= fct_relevel(lf_mda, "Yes", "No")) %>% 
  arrange(oncho_endemic, loaloa_endemic,lf_endemic,oncho_mda,lf_mda) %>% 
  #group_by(oncho_endemic) %>% 
  gt() %>% 
  tab_header(title="NTDs in Angola") %>% 
  opt_align_table_header(align = "left") %>% 
  tab_spanner(label = "Endemicity", 
              columns=(c(oncho_endemic, loaloa_endemic,lf_endemic))) %>% 
  tab_spanner(label = "MDA", 
              columns=(c(oncho_mda, lf_mda))) %>% 
  cols_label(oncho_endemic = "Oncho", 
            oncho_mda = "Oncho", 
           loaloa_endemic = "Loa loa", 
          lf_endemic = "LF", 
          lf_mda = "LF",
          n="Number of IUs") 

all_diseases_summary %>%  
  filter(Year==2020) %>% 
  count(ADMIN1,oncho_endemic,loaloa_endemic,lf_endemic, oncho_mda, lf_mda) %>% 
  select(ADMIN1,oncho_endemic,loaloa_endemic,lf_endemic,oncho_mda,lf_mda,n) %>% 
  mutate(oncho_endemic= fct_relevel(oncho_endemic, "Yes", "Unknown")) %>% 
  mutate(loaloa_endemic= fct_relevel(loaloa_endemic, "Yes", "No")) %>% 
  mutate(lf_endemic= fct_relevel(lf_endemic, "Yes", "No", "Unknown")) %>% 
  #mutate(oncho_mda= fct_relevel(oncho_mda, "Yes", "No", "LF MDA")) %>% 
  #mutate(lf_mda= fct_relevel(lf_mda, "Yes", "No")) %>% 
  arrange(oncho_endemic, loaloa_endemic,lf_endemic,oncho_mda,lf_mda) %>% 
  group_by(ADMIN1) %>% 
  gt() %>% 
  tab_header(title="NTDs in Angola") %>% 
  opt_align_table_header(align = "left") %>% 
  tab_spanner(label = "Endemicity", 
              columns=(c(oncho_endemic, loaloa_endemic,lf_endemic))) %>% 
  tab_spanner(label = "MDA", 
              columns=(c(oncho_mda, lf_mda))) %>% 
  cols_label(oncho_endemic = "Oncho", 
             oncho_mda = "Oncho", 
             loaloa_endemic = "Loa loa", 
             lf_endemic = "LF", 
             lf_mda = "LF",
             n="Number of IUs") 
  
# Questions for Jorge
# Some IUs (n=9) are reported as under LF MDA in the oncho data, but no MDA is reported in the LF data. Are there different classifications for whether MDA is taking place for these IUs?

all_diseases_summary %>%  
  filter(Year==2020) %>% 
  tabyl(lf_mda)

endemicity_categories <- all_diseases_summary %>%  
  filter(Year==2020,oncho_endemic=="Unknown") %>% 
  select(IU_ID,oncho_endemic,loaloa_endemic,lf_endemic) %>% 
  count(oncho_endemic,loaloa_endemic,lf_endemic, sort=TRUE) 
  mutate(final_endemicity=paste(loaloa_endemic,lf_endemic, sep="-"))

endemicity_categories_count <- all_diseases_summary %>%  
  filter(Year==2020) %>% 
  count(IU_ID,oncho_endemic,loaloa_endemic,lf_endemic, sort=TRUE)  

endemicity_cat_long <- all_diseases_summary %>%  
  filter(Year==2020) %>% 
  select(IU_ID,oncho_endemic,loaloa_endemic,lf_endemic) %>% 
  pivot_longer(-IU_ID)

library(sf)
ang_shp <- read_sf(here('data', 'input', 'shp', 'ESPEN_IU_2020.shp')) %>% 
  filter(ADMIN0=='Angola')

ang_adm3_shp <- read_sf(here('data', 'input', 'shp', 'ago_admbnda_adm3_gadm_ine_ocha_20180904.shp')) 

ang_endemicity_data <- ang_shp %>%
  left_join(endemicity_categories,
            by = c('IU_ID'))

ang_lf_endemicity_data <- ang_shp %>% 
  left_join(lf_wd %>% filter(Year==2020), by='IU_ID')

province_sf <- ang_adm3_shp %>% group_by(ADM1_EN) %>% select(ADM1_EN,geometry) %>% 
  summarise()

ggplot() +
  geom_sf(data=ang_endemicity_data,aes(fill = final_endemicity), color = "gray50", size = 0.5) +
  # geom_sf(fill = "transparent", color = "gray20", size = 1,
  #         data = province_sf) +
  labs(fill='Oncho-Loaloa-LF') + 
  theme(legend.position=c(0.85, 0.9))
  
ggplot() +
  geom_sf(data=ang_endemicity_data,aes(fill = value), color = "gray50", size = 0.5) +
    # geom_sf(fill = "transparent", color = "gray20", size = 1, 
    #         data = province_sf) +
  labs(fill='') + 
  theme(legend.position='top') +
  facet_wrap(~name, ncol=1)

ang_endemicity_map

all_diseases_summary %>% 
  filter(oncho_mda=="LF MDA" & lf_mda=="No")

#cols_label(oncho_endemic = "Endemic", 
   #          oncho_mda = "MDA", 
    #         loaloa_endemic = "Endemic", 
     #        lf_endemic = "Endemic", 
     #        lf_mda = "MDA") %>% 
  #tab_spanner(label = "Onchocerciasis", 
  #            columns=(c(oncho_endemic, oncho_mda))) %>% 
  #tab_spanner( label = "Loa loa", 
#columns=(loaloa_endemic)) %>% 
#  tab_spanner(label = "LF", 
 #              columns=(c(lf_endemic, lf_mda)))

