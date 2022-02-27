

library(readxl)
library(here)
library(janitor)
library(tidyverse)
library(tidyxl)
library(unpivotr)

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
    behead("N",commune)%>%
    fill(character, .direction = "down") %>% 
    filter(!is.na(commune))
  }#apply function to every tibble in stage1
stage2 <- map(stage1,def_main)

final <- bind_rows(stage2) %>% 
  select(province=sheet,commune, age_group=character, population=numeric)




