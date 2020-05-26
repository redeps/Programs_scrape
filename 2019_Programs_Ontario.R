
library(tidyverse)
library(rvest)
library(RSelenium)
library(XML)
#setwd("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs")

source("2019_Programs_Functions.R")


#### Reading in helper data sets
colleges <- read_rds("All_colleges.rds")
colleges %>% 
  filter(grepl("Ontario", prov_terr))  %>% 
  mutate(member = substr(record_id, 8,12),
         member = ifelse(member != "", T, F)) %>% 
  as.data.frame()


#                                              institution_name     member
#                                              Algonquin College   TRUEe
#                Cambrian College of Applied Arts and Technology    TRUE
#                Canadore College of Applied Arts and Technology    TRUE
#              Centennial College of Applied Arts and Technology    TRUE
#                                                 Collège Boréal    TRUE
#Conestoga College Institute of Technology and Advanced Learning    TRUE
#           Confederation College of Applied Arts and Technology    TRUE
#                                                 Durham College    TRUE
#                Fanshawe College of Applied Arts and Technology    TRUE
#                              First Nations Technical Institute    TRUE
#                                                Fleming College    TRUE
#                                           George Brown College    TRUE
#                Georgian College of Applied Arts and Technology    TRUE
#     Humber College Institute of Technology & Advanced Learning    TRUE
#                     Kenjgewin Teg Educational Institute (KTEI)    TRUE
#                                                        La Cité    TRUE
#                 Lambton College of Applied Arts and Technology    TRUE
#                Loyalist College of Applied Arts and Technology    TRUE
#                  Mohawk College of Applied Arts and Technology    TRUE
#                                                Niagara College    TRUE
#                niagara parks commission school of horticulture   FALSE
#                                               Northern College    TRUE
#                   ontario college of art and design university   FALSE
#                                                  Sault College    TRUE
#                  Seneca College of Applied Arts and Technology    TRUE
#                                               Sheridan College    TRUE
#                                        Six Nations Polytechnic    TRUE
#                                              St. Clair College    TRUE
#                                           St. Lawrence College    TRUE
#                                    the audio recording academy   FALSE
#                     The Michener Institute of Education at UHN    TRUE
#                       university of guelph - ridgetown college   FALSE


###########################################
#########         Ontario        ##########
###########################################


program_scrapes = list.files("2019_Programs_Ontario")
#source the above files
#print(Instittution$instituition_name)
program_scrapes %>% 
  map(~ source(paste0("2019_Programs_Ontario/", .)))


program_datas = list.files("programs/Ontario", pattern = "programs.rds")
Atlantic_programs = program_datas %>% 
  map(~ read_rds(paste0("programs/Ontario/", .))) %>% 
  bind_rows()


program_datas = list.files("programs/Territories", pattern = "programs.rds")
Terr_programs = program_datas %>% 
  map(~ read_rds(paste0("programs/Territories/", .))) %>% 
  bind_rows()

Atl_terr_programs = Atlantic_programs %>% 
  bind_rows(Terr_programs)
