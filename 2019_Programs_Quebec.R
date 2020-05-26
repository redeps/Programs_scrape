
library(tidyverse)
library(rvest)
library(RSelenium)
library(XML)
#setwd("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs")

source("2019_Programs_Functions.R")


#### Reading in helper data sets
colleges <- read_rds("All_colleges.rds")
colleges %>% 
  filter(grepl("Qu", prov_terr))  %>% 
  mutate(member = substr(record_id, 8,12),
         member = ifelse(member != "", T, F)) %>% 
  as.data.frame()


#1                                  cegep - college de la gaspesie et des i	FALSE
#2                                            cégep <e0> distance (anglopho	FALSE
#3                                                      Cégep André-Laurend	 TRUE
#4                                                     cégep beauce-appalac	FALSE
#5                                                            cégep d'ahunt	FALSE
#6                                                        cégep de baie-com	FALSE
#7                                                   cégep de bois-de-boulo	FALSE
#8                                                         Cégep de Chicout	 TRUE
#9                                                      cégep de drummondvi	FALSE
#10                                                            cégep de gra	FALSE
#11                                                         Cégep de Jonqui	 TRUE
#12                                           Cégep de la Gaspésie et des Î	 TRUE
#13                                                      Cégep de La Pocati	 TRUE
#14                                           Cégep de l'Abitibi-Témiscamin	 TRUE
#15                                                      cégep de lévis-lau	FALSE
#16                                                       cégep de l'outaou	FALSE
#17                                                            Cégep de Mat	 TRUE
#18                                                          Cégep de Rimou	 TRUE
#19                                                   Cégep de Rivière-du-L	 TRUE
#20                                                      Cégep de Saint Jér	 TRUE
#21                                                        Cégep de Sainte-	 TRUE
#22                                                    Cégep de Saint-Félic	 TRUE
#23                                                     Cégep de Saint-Laur	 TRUE
#24                                                         Cégep de Sept-Î	 TRUE
#25                                                        Cégep de Sherbro	 TRUE
#26                                                       cégep de sorel-tr	FALSE
#27                                                      cégep de st-hyacin	FALSE
#28                                                          cégep de thetf	FALSE
#29                                                    Cégep de Trois-Riviè	 TRUE
#30                                                       cégep de valleyfi	FALSE
#31                                                     cégep de victoriavi	FALSE
#32                                                    Cégep du Vieux Montr	 TRUE
#33                                                    Cégep Édouard Montpe	 TRUE
#34                                                              Cégep Garn	 TRUE
#35                                                         cégep gérald-go	FALSE
#36                                                     Cégep Heritage Coll	 TRUE
#37                                                  Cégep John Abbott Coll	 TRUE
#38                                                             Cégep Limoi	 TRUE
#39                                                        cégep lionel gro	FALSE
#40                                                       Cégep Marie-Victo	 TRUE
#41                                               Cégep régional de Lanaudi	 TRUE
#42                                             Cégep Saint-Jean-sur-Richel	 TRUE
#43                          centre de formation professionel gabriel-rouss	FALSE
#44                    centre de formation professionnelle (c.f.p.) bel ave	FALSE
#45                       centre de formation professionnelle (c.f.p.) rela	FALSE
#46               centre de formation professionnelle (c.f.p.)mont-joli-mit	FALSE
#47                                 centre de formation professionnelle acc	FALSE
#48 centre de formation professionnelle lennoxville vocational training cen	FALSE
#49                                centre de formation professionnelle l'oa	FALSE
#50                   centre de formation professionnelle pavillon de l'ave	FALSE
#51                      centre de formation proffessionelle (c.f.p.) limoi	FALSE
#52                                                     centre frere moffet	FALSE
#53                                                  cfp competences outaou	FALSE
#54                                                 Champlain Regional Coll	 TRUE
#55                                                           Collège Ahunt	 TRUE
#56                                                      Collège André-Gras	 TRUE
#57                                                             Collège d'A	 TRUE
#58                                                Collège de Bois-de-Boulo	 TRUE
#59                                                     Collège de Maisonne	 TRUE
#60                                                              Collège El	 TRUE
#61                                                            Collège LaSa	 TRUE
#62                                                        Collège Montmore	 TRUE
#63                                                           Collège Rosem	 TRUE
#64                                                         Collège Shawini	 TRUE
#65                                                  college ultra de montr	FALSE
#66                   conservatoire de musique et d'art dramatique de montr	FALSE
#67                                                             Dawson Coll	 TRUE
#68                                                eastern shores school bo	FALSE
#69                                            ecole de technologie superie	FALSE
#70                               ecole des metiers de lequipement motorise	FALSE
#71                                 ecole des metiers du sud-ouest de montr	FALSE
#72                                  école nationale d'administration publi	FALSE
#73                                           institut de technologie agric	FALSE
#74                       institut de technologie agroalimentaire st hyacin	FALSE
#75                             Institut de tourisme et d'hotellerie du Qué	 TRUE
#76                                                    institut teccart (20	FALSE
#77                                            john f. kennedy business cen	FALSE
#78                                         listuguj migmag development cen	FALSE
#79                                      macdonald college - université mcg	FALSE
#80                                                 montreal technical coll	FALSE
#81                                                         nova career cen	FALSE
#82                                                                tav coll	FALSE
#83                                                             Vanier Coll	TRUE









rD <- rsDriver(browser = "firefox", 
               verbose = FALSE)

remDr <- rD$client
#remDr$open(silent = T)
url <- "https://www.inforoutefpt.org/progCol.aspx"
remDr$navigate(url)

extended <- remDr$findElement(using = "xpath", '//*[@id="lnkVoirTous"]')
extended$clickElement()



doc = htmlParse(remDr$getPageSource()[[1]])
responseTable = readHTMLTable(doc)[[1]]
responseTable = responseTable %>% 
  as_tibble()

url <- remDr$findElements(using = "xpath", '//*[@id="lnkProg"]')

url <- lapply(url, function(x) x$getElementAttribute("href")) %>% unlist()

programs_table_dec <- responseTable %>% 
  mutate(url = url)


url <- "https://www.inforoutefpt.org/progCol.aspx?sanction=2"
remDr$navigate(url)

extended <- remDr$findElement(using = "xpath", '//*[@id="lnkVoirTous"]')
extended$clickElement()



doc = htmlParse(remDr$getPageSource()[[1]])
responseTable = readHTMLTable(doc)[[1]]
responseTable = responseTable %>% 
  as_tibble()

url <- remDr$findElements(using = "xpath", '//*[@id="lnkProg"]')

url <- lapply(url, function(x) x$getElementAttribute("href")) %>% unlist()

programs_table_aec <- responseTable %>% 
  mutate(url = url)

programs_table <- programs_table_dec %>% 
  mutate(Credential = "DEC") %>% 
  bind_rows(programs_table_aec %>% 
              mutate(Credential = "AEC"))
  
programs_table <- programs_table %>% 
  distinct()

remDr$close()

programs_table$Duration      = NA
programs_table$Cost          = NA
programs_table$NumberCourses = NA
programs_table$WIL           = NA
programs_table$Description   = NA
programs_table$Campus        = NA
programs_table$Sector        = NA

for(url in 1:nrow(programs_table)){
  print(url)
  tryCatch({
    
    webPage = read_webPage(programs_table$url[url])
    program <- webPage %>% 
      html_nodes("table.table-sm") 
    
    ### Getting program details from tables
    tryCatch({
      remove(program_details)
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    
    for(det_tbl in 1:length(program)){
      #print(det_tbl)
      if(!exists("program_details")){
        tryCatch({
          program_details = program[det_tbl] %>% html_table(fill = T) %>% map(~ mutate_all(., as.character))
        }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
      } else {
        tryCatch({
          program_details = program_details %>% 
            bind_rows(program[det_tbl] %>% html_table(fill = T) %>% map(~ mutate_all(., as.character)))
        }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
      }
      
      if(det_tbl == length(program)){
        program_details = program_details %>% 
          set_names(paste(colnames(program_details)))
        
        program_details = program_details[!grepl("NA", colnames(program_details))]
      }
    }
    
    
    
    
    if(grepl("sanction=1", programs_table$url[url])){
      Campus = webPage %>% 
        html_nodes(xpath = '//*[@id="lnkProgOffres"]') %>% 
        html_attr("href") %>% 
        paste0("https://www.inforoutefpt.org/", .)
    } else {
      Campus = webPage %>% 
        html_nodes("h5") %>% 
        html_text()
    }
    
    Description = webPage %>% 
      html_nodes("div.patchh3h5") %>% 
      html_text()
      
    tryCatch({
      remove("WIL")
      courses = program_details %>% select(c("Numéro ou code","Énoncé de la compétence")) %>% 
        filter(!is.na(`Numéro ou code`)) %>% 
        set_names(c("Code", "Name"))
      
      WIL = course_eval(courses, Institution = "Inforoute", programs_table$Nom[url], programs_table$url[url], FR = T)
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})

    if(!exists("WIL")){
      WIL =  program_details$X2[grepl("Stages", program_details$X1)]
    }    
    
    Duration =  program_details$X2[grepl("Durée totale", program_details$X1)]
    Cost =  program_details$X2[grepl("Coût", program_details$X1)]
    NumberCourses = program_details$X2[grepl("de cours", program_details$X1)]
    Sector = program_details$X2[grepl("Secteur", program_details$X1)]
    
    programs_table$Duration[url]      = Duration %>% replace_empty_na()
    programs_table$Cost[url]          = Cost %>% replace_empty_na()
    programs_table$NumberCourses[url] = NumberCourses %>% replace_empty_na()
    programs_table$WIL[url]           = WIL %>% replace_empty_na()
    programs_table$Description[url]   = Description %>% replace_empty_na()
    programs_table$Campus[url]        = Campus %>% replace_empty_na()
    programs_table$Sector[url]        = Sector %>% replace_empty_na()
    
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

#write_rds(programs_table, "2019_Programs_Quebec/programs_table.rds")
#programs_table <- read_rds("2019_Programs_Quebec/programs_table.rds")
 
for(url in 1:nrow(programs_table)){
  
  tryCatch({
    if(grepl("sanction=1", programs_table$url[url])){
      print(url)
      webPage = read_webPage(programs_table$Campus[url] %>% unlist())
      campus_table = webPage %>% 
        html_nodes("table.table") %>% 
        html_table() %>% 
        bind_rows()
      
      programs_table$Campus[url] = campus_table[1] %>% unlist() %>% paste(collapse = "|")
    } else {
      programs_table$Campus[url] = gsub("[[:space:]]{2,}|^[[:space:]]*", "", programs_table$Campus[url])
    }
    

  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

programs_table_exp = programs_table %>% 
  separate_rows(Campus, sep = "\\|") %>% 
  filter(Campus != "") %>% 
  filter(!grepl("https", Campus)) %>% 
  mutate(Campus = gsub("( - |\\(|, )(campus|centre|École|Institut)", "Campus separator \\2", Campus, ignore.case = T)) %>% 
  separate(Campus, c("Institution", "Campus"), sep = "Campus separator ")
 


#write_rds(programs_table_exp, "2019_Programs_Quebec/programs_table_exp.rds")
#programs_table_exp <- read_rds("2019_Programs_Quebec/programs_table_exp.rds") 

programs_table_exp = programs_table_exp %>% 
  mutate(institution_name = NA,
         prov_terr = NA,
         record_id = NA,
         member = NA)

for(r in 1:nrow(programs_table_exp)){
  Institution = CIConnexName(programs_table_exp$Institution[r])
  if(nrow(Institution) > 0){
    programs_table_exp$institution_name[r] = Institution$institution_name
    programs_table_exp$prov_terr[r] = Institution$prov_terr
    programs_table_exp$record_id[r] = Institution$record_id
    programs_table_exp$member[r] = Institution$member
  } else {
    Institution = CIConnexName(gsub("Air |Institut | \\(1975\\)| collégial privé|Cégep de|[[:space:]]*Cégep[[:space:]]*|[[:space:]]*College[[:space:]]*|[[:space:]]*Collège[[:space:]]*|[[:space:]]*Séminaire[[:space:]]*|Collège de[[:space:]]*|[[:space:]]*Regional[[:space:]]*|[[:space:]]*collégial privé[[:space:]]*", "", programs_table_exp$Institution[r], ignore.case = T))
    if(nrow(Institution) > 0){
      programs_table_exp$institution_name[r] = Institution$institution_name
      programs_table_exp$prov_terr[r] = Institution$prov_terr
      programs_table_exp$record_id[r] = Institution$record_id
      programs_table_exp$member[r] = Institution$member
    } else {
      Institution = ifelse(grepl("Lanaudière", programs_table_exp$Institution[r], ignore.case = T), "lanaudière",
                           ifelse(grepl("jérôme", programs_table_exp$Institution[r], ignore.case = T), "jérôme",
                                  ifelse(grepl("tourisme", programs_table_exp$Institution[r], ignore.case = T), "tourisme",
                                         ifelse(grepl("édouard", programs_table_exp$Institution[r], ignore.case = T), "édouard",
                                                ifelse(grepl("ellis", programs_table_exp$Institution[r], ignore.case = T), "ellis",
                                                       ifelse(grepl("agroalimentaire", programs_table_exp$Institution[r], ignore.case = T), "agroalimentaire",
                                                              ifelse(grepl("félicien", programs_table_exp$Institution[r], ignore.case = T), "félicien",
                                                                     ifelse(grepl("distance", programs_table_exp$Institution[r], ignore.case = T), "distance",programs_table_exp$Institution[r]))))))))
      Institution = CIConnexName(Institution)
      if(nrow(Institution) > 0){
        programs_table_exp$institution_name[r] = Institution$institution_name
        programs_table_exp$prov_terr[r] = Institution$prov_terr
        programs_table_exp$record_id[r] = Institution$record_id
        programs_table_exp$member[r] = Institution$member
      } else {
        programs_table_exp$institution_name[r] = programs_table_exp$Institution[r]
        programs_table_exp$prov_terr[r] = "Québec"
        programs_table_exp$record_id[r] = NA
        programs_table_exp$member[r] = FALSE
      }
    }
  }
}

#institution_name, prov_terr, record_id, member, url, Program, Credential, Campus, Duration, Description, WIL
programs = programs_table_exp %>% 
  rename(Program = Nom) %>% 
  select(institution_name, prov_terr, record_id, member, url, Program, Credential, Campus, Duration, Description, WIL, Sector, Cost)

region = "Quebec"
Institution = "Inforoute"

old_files = list.files(paste0("programs/",region))
old_files = if(length(old_files) > 1) old_files[grepl(clean_string(Institution), old_files)]

if(length(old_files) > 0){
  file.copy(paste0("programs/",region, "/",old_files), paste0("programs/",region, "/Archive/",old_files))
  file.remove(paste0("programs/",region, "/",old_files))
}

write_rds(programs, paste0("programs/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_programs.rds"))
write_csv(programs, paste0("programs/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_programs.csv"), na = "")
if(exists("Fees")){
  write_rds(programs, paste0("programs/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_fees.rds"))
  
}


