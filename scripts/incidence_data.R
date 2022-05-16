#Endemicity analysis

library(here)
library(tidyverse)
library(linelist)
library(janitor)
library(readxl)
library(tidyxl)
library(unpivotr)
library(gt)

(list.files(here('data', 'input', 'oncho_incidence'), full.names=T, recursive=T) %>%
    keep(~ str_detect(.x, '.xls')) %>%
    map_dfr(~ readxl::read_xls(.x) %>% mutate(province = .x) %>% 
              mutate(province= str_replace(
                province, 
                "/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/analysis/angola_oem_iuselect/data/input/oncho_incidence/", 
                "")) %>% 
              clean_names)
     ->
    incidence_raw)

incidence_clean <- incidence_raw %>% 
  mutate(new = coalesce(!!! select(., contains("provincia")))) %>% 
  mutate(municipality=coalesce(organisationunitname, new)) %>% 
  select(-c(organisationunitname, new, contains('provincia'))) %>% 
  filter(!province=='ANUAL-2017.xls') %>% 
  mutate(province= str_replace(
    province, 
    ".xls", 
    "")) %>% 
  clean_data() %>% 
  select(province, municipality, everything()) %>% 
  mutate(province = case_when ( province=="benguela222" ~ 'huila', 
                                 TRUE ~ province)) %>% 
  pivot_longer(
    -c(province, municipality)) %>% 
  mutate(
    year = str_replace(name, 'ne_onconcercose_casos_filariosis_ocular_', '')) %>% 
  mutate(
    municipality = str_replace(municipality, 'municipio_de_', '')) %>%
  mutate(province = case_when(province=='c_norte' ~ 'cuanza_norte', 
                              province=='c_sul' ~ 'cuanza_sul', 
                              province=='l_norte' ~ 'lunda_norte', 
                              
                              TRUE ~ province)) %>% 
  select(-name) 

# Municipality level populations


#https://medium.com/@samukweku/reshaping-an-excel-table-in-r-71d7d9020124

#sourcefile
filename <- here('data/input/Estimativas_Comunais_Geral_Final.xlsx')
#get list of sheet_names
#which also represents the company names
sheet_names <- xlsx_sheet_names(filename)#read in file, using the tidyxl package
#filter for only character and numeric
#and pick specific rows and columns
def <- function(sheetname){xlsx_cells(filename,sheetname)%>%
    filter(data_type %in% c("character","numeric"), 
           row >=5, 
           col >=1)}
#stage1
#apply function to sheetnames
#using the purrr library
stage1 <- map(sheet_names, def)

def_main <- function(datum){datum%>%
    select(sheet,row,col,data_type,character,numeric)%>%
    #behead("NNE",years)%>%
    behead("NNE",commune)%>%
    fill(character, .direction = "down") %>% 
    filter(!is.na(commune))
}#apply function to every tibble in stage1
stage2 <- map(stage1,def_main)

municpality_population <- stage2  %>% 
  bind_rows(.id="province") %>% 
  select(province=sheet, municipality=commune, population=numeric)  %>% 
  filter(!is.na(population)) %>% 
  group_by(province, municipality) %>% 
  slice(which.max(population))  %>% 
  clean_data() %>% 
  filter(!municipality %in% c('talatona')) %>% 
  mutate(municipality= case_when(municipality=='quilombo_dos_dembos' ~ 'gonguembo', 
                                 municipality=='lumbala_nguimbo' ~ 'bundas',
                                 municipality=='muxima' ~ 'quicama',
                                 municipality=='cazombo' ~ 'alto_zambeze',
                                 municipality=='ambaca_ex_camabatela' ~ 'ambaca',
                                 municipality=='ndalatando' ~ 'cazengo',
                                 municipality=='omukolongonjo' ~ 'cuvelai',
                                 municipality=='catete' ~ 'icolo_e_bengo',
                                 municipality=='quinzala' ~ 'mucaba', 
                                 municipality=='benfica' ~ 'talatona', 
                                 
                                 TRUE ~ municipality))




#rm(stage1, stage2,filename, def)

incidence_pop_merge <- incidence_clean %>%   mutate(municipality=case_when(municipality=='ambaca_ex_camabatela' ~ 'ambaca', 
                                                    municipality=='malange' ~ 'malanje', 
                                                    municipality=='tchicala_tcholohanga' ~ 'tchikala_tcholohanga', 
                                                    municipality=='quibala' ~ 'kibala', 
                                                    municipality=='sumbe' ~ 'sumbe_sede', 
                                                    municipality=='belas' ~ 'futungo_de_belas', 
                                                    municipality=='kilamba_kiaxi' ~ 'kilamba', 
                                                    municipality=='kiwaba_nzogi' ~ 'kiwaba_n_zogi',
                                                    municipality=='quirima' ~ 'kirima',
                                                    municipality=='cela' ~ 'cela_ex_waku_kungo',
                                                    municipality=='lumeje_cameia' ~ 'cameia',
                                                    TRUE ~ municipality)) %>% 
  left_join(municpality_population, by=c('province', 'municipality')) %>% 
  filter(!value>1000) %>% 
  mutate(incidence_per100k=(value/population)*100000) %>% 
  mutate(municipality=case_when(municipality== 'cela_ex_waku_kungo' ~ 'cela_waku_kungo', 
                                municipality== 'quicama' ~ 'quissama_muxima_quicama', 
                                municipality== 'kiwaba_n_zogi' ~ 'kiuaba_n_zoji_cuaba_nzogo', 
                                municipality== 'kuimba' ~ 'cuimba', 
                                TRUE ~ municipality))

incidence_pop_merge %>% 
  #sample_n(5) %>% 
  ggplot(aes(x=year, y=incidence_per100k, group=municipality, color=municipality)) +
  theme(legend.position = 'none') +
  geom_point(stat = 'identity',position = position_dodge(0.9)) +
  facet_wrap(~province)

province_incidence <- incidence_pop_merge %>% 
  group_by(year, province) %>% 
  summarise(province_incidence=sum(value, na.rm=TRUE),
      province_population=sum(population,na.rm=TRUE)) %>% 
  mutate(province_incidence_per100k=(province_incidence/province_population)*100000)

municipality_incidence <- incidence_pop_merge %>% 
  group_by(year, province,municipality) %>% 
  summarise(municipality_incidence=sum(value, na.rm=TRUE),
            municpality_population=sum(population,na.rm=TRUE)) %>% 
  mutate(municipality_incidence_per100k=(municipality_incidence/municpality_population)*100000) 


case_reported <- municipality_incidence %>% 
  filter(municipality_incidence>0) %>% 
  ungroup() %>% 
  distinct(province, municipality) %>% 
  mutate(case_reported='YES')


municipality_pivot <- municipality_incidence %>% select(province, municipality, year, municipality_incidence_per100k) %>% 
  pivot_wider(names_from='year', 
              values_from='municipality_incidence_per100k')

library(kableExtra)
municipality_incidence_table <-  municipality_pivot %>% mutate(province=str_replace_all(province,'_', ' ')) %>% 
  mutate(municipality=str_replace_all(municipality,'_', ' ')) %>% 
  ungroup() %>% 
  arrange((province)) 
  # kable()
  # 
  # gt(rowname_col = "province") %>% 
  # tab_header(
  #   title = "Onchocerciasis incidence",
  #   subtitle = "Cases per 100,000 people") %>% 
  # fmt_number(columns=c(`2017`,`2018`,`2019`,`2020`,`2021`), decimals=1) %>% 
  # fmt_missing(
  #   columns = 3:7,
  #   missing_text = ""
  # )

saveRDS(municipality_incidence_table, here('tables', 'municipality_incidence_table.Rds'))



