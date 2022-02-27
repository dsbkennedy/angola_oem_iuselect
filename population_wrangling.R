

library(readxl)
library(here)
library(janitor)
library(tidyverse)
all_sheets <- readxl::excel_sheets(here('data/input/Estimativas_Comunais_Geral_Final.xlsx'))

population_raw <- read_excel("data/input/Estimativas_Comunais_Geral_Final.xlsx", skip = 4, sheet="Zaire")

population_clean <- population_raw %>% clean_names() %>% remove_empty("cols") %>% 
  pivot_longer(-1)

zaire_raw <- read_excel("data/input/Estimativas_Comunais_Geral_Final.xlsx", skip = 4, sheet="Zaire")

zaire_clean <- zaire_raw %>% clean_names() %>% remove_empty("cols") %>% 
  pivot_longer(-1)


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename) 
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,skip = 4) %>% clean_names() %>% remove_empty("cols"))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

all_population_raw <- read_excel_allsheets(here('data/input/Estimativas_Comunais_Geral_Final.xlsx'))

all_population_clean <- bind_rows(all_population_raw)


library(tidyxl)
library(unpivotr)

x <- xlsx_cells(here('data/input/Estimativas_Comunais_Geral_Final.xlsx'), sheets="Zaire")

dplyr::glimpse(x)

file <- x%>%
  dplyr::filter(data_type %in% c("character",
                          "numeric"),
         row >=5, 
         
         col >=1)


behead_result <- file%>%
  select(sheet,row,col,data_type,character,numeric)%>%    
  behead("N",commune) %>% 
  fill(character, .direction = "down") %>% 
  filter(!is.na(commune))

final_pop <- behead_result %>% 
  select(province=sheet,commune, age_group=character, population=numeric)

#############

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
    behead("N",commune)%>%
    fill(character, .direction = "down") %>% 
    filter(!is.na(commune))
  }#apply function to every tibble in stage1
stage2 <- map(stage1,def_main)

final <- bind_rows(stage2) %>% 
  select(province=sheet,commune, age_group=character, population=numeric)


import_fn <- function(x) {
  x <- xlsx_cells(here('data/input/Estimativas_Comunais_Geral_Final.xlsx'), sheets=.x)
  file <- x %>%
  dplyr::filter(data_type %in% c("character",
                                 "numeric"),
                row >=5, col >=1)

behead_result <- file %>%
  select(sheet,row,col,data_type,character,numeric)%>%    
  behead("N",commune) %>% 
  fill(character, .direction = "down") %>% 
  filter(!is.na(commune))

final_pop <- behead_result %>% 
  select(province=sheet,commune, age_group=character, population=numeric)
}


