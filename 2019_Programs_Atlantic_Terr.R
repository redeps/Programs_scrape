library(tidyverse)
library(rvest)
library(RSelenium)
#setwd("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs")

source("2019_Programs_Functions.R")


#### Reading in helper data sets
colleges <- read_rds("All_colleges.rds")
colleges %>% 
  filter(grepl("Nova Scotia|New Brunswick|Newfoundland|Prince|Terr|Nuna|Yuk", prov_terr))  %>% 
  mutate(member = substr(record_id, 8,12),
         member = ifelse(member != "", T, F)) %>% 
  as.data.frame()

#Remove:
#college de technologie forestiere des maritimes             New Brunswick
#moncton hospital school of radiologic technology             New Brunswick
#saint john school of radiation therapy             New Brunswick
##                          canadian coast guard college               Nova Scotia
##               halifax regional police training school               Nova Scotia
##              maritime conservatory of performing arts               Nova Scotia


##     Collège communautaire du Nouveau-Brunswick (CCNB)             New Brunswick
##       college de technologie forestiere des maritimes             New Brunswick
##                 Maritime College of Forest Technology             New Brunswick
##      moncton hospital school of radiologic technology             New Brunswick
##             New Brunswick College of Craft and Design             New Brunswick
##                New Brunswick Community College (NBCC)             New Brunswick
##                saint john school of radiation therapy             New Brunswick
##                            Centre for Nursing Studies Newfoundland and Labrador
##                   College of the North Atlantic (CNA) Newfoundland and Labrador
##                                      Marine Institute Newfoundland and Labrador
##                                        Aurora College     Northwest Territories
##                          Collège Nordique Francophone     Northwest Territories
##                          canadian coast guard college               Nova Scotia
##                                Cape Breton University               Nova Scotia
## Dalhousie Agricultural Campus of Dalhousie University               Nova Scotia
##               halifax regional police training school               Nova Scotia
##              maritime conservatory of performing arts               Nova Scotia
##                  Nova Scotia Community College (NSCC)               Nova Scotia
##          Université Sainte-Anne - Collège de l'Acadie               Nova Scotia
##                                Nunavut Arctic College                   Nunavut
##                                      Collège de l'Île      Prince Edward Island
##                                       Holland College      Prince Edward Island
##                                         Yukon College                     Yukon

##### Basic breakdown of script:
### Reading/getting dataset
### adding WIL variable
### Going through all urls to either
##  a) add WIL
##  b) add WIL and missing components
##  Adding and saving courses (needs to be updated for earlier entries)


###########################################
######### Atlantic Canada + Terr ##########
###########################################


program_scrapes = list.files("2019_Programs_Atlantic_Terr")
#source the above files
#print(Instittution$instituition_name)
program_scrapes %>% 
  map(~ source(paste0("2019_Programs_Atlantic_Terr/", .)))


program_datas = list.files("programs/Atlantic", pattern = "programs.rds")
Atlantic_programs = program_datas %>% 
  map(~ read_rds(paste0("programs/Atlantic/", .))) %>% 
  bind_rows()


program_datas = list.files("programs/Territories", pattern = "programs.rds")
Terr_programs = program_datas %>% 
  map(~ read_rds(paste0("programs/Territories/", .))) %>% 
  bind_rows()

Atl_terr_programs = Atlantic_programs %>% 
  bind_rows(Terr_programs)

#Check vs old files, clean and enter
#CIP matching?






