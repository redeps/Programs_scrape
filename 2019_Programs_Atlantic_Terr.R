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


list.files("2019_Programs_Atlantic_Terr")
#source the above files


#Check vs old files, clean and enter
#CIP matching?



#############################
##### Nova Scotia Community College (NSCC)
## Type of website:
Institution <- CIConnexName("Nova Scotia Community College (NSCC)")


#############################
##### Université Sainte-Anne - Collège de l'Acadie
## Type of website:
Institution <- CIConnexName("Université Sainte-Anne - Collège de l'Acadie")


#############################
##### Nunavut Arctic College
## Type of website:
Institution <- CIConnexName("Nunavut Arctic College")


#############################
##### Collège de l'Île
## Type of website:
Institution <- CIConnexName("Collège de l'Île")


#############################
##### Holland College
## Type of website:
Institution <- CIConnexName("Holland College")


#############################
##### Yukon College
## Type of website:
Institution <- CIConnexName("Yukon College")







#Marine institute
#ae473735-3420-40bc-8e19-845dfc04e86f

#### Add below to each  member segment ####
Institution <- CIConnexName("Marine Institute")


d <- read_csv("temp/2019_Prgs_MarineInstitute_2of2-(Crawl-Run)---2019-09-30T143137Z.csv")

d$WIL <- NA
for(prog in 1:nrow(d)){
  print(prog)
  url <- d$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    webPageData <- webPage %>% 
      html_nodes("p.inline_detail_title") 
    
    if(length(webPageData) > 0){
      
      webPageData <- tibble(
        category = webPage %>% 
          html_nodes("p.inline_detail_title") %>% 
          html_text(),
        
        categoryValue = webPage %>% 
          html_nodes("p.inline_detail_content")%>% 
          html_text()
      )
      
      webPageData <- webPageData %>% 
        filter(category == "Practical Experience:")
      
      if(nrow(webPageData)> 0){
        d$WIL[prog] <- gsub("\r|\n| {2,}", "", webPageData$categoryValue)
      }
      
    }
    
    
  }
  
}


### Update for each member segment
d$CIConnexName <- Institution
write_csv(d, paste0("temp/cleaned_data/", gsub(" ", "_", Institution), ".csv"))


#Cape Breton
#32769b67-ec61-4610-b726-6cbbf4eb6e24
Institution <- CIConnexName("Breton")

d <- read_csv("temp/2019_Prgs_CapeBretonU_2of2-(Crawl-Run)---2019-09-30T161057Z.csv")
d$WIL <- NA


#h2
#Internship Experience
#Field course
#Co-operative Education     small header
#div.eightcol main__content
#p
#https://www.cbu.ca/academic-programs/program/bachelor-of-business-administration/degree-details/


for(prog in 1:nrow(d)){
  print(prog)
  url <- paste0(d$url[prog], "degree-details/")
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    webPageData <- webPage %>% 
      html_nodes("h2") %>% 
      html_text()
    
    webPageData <- webPageData[grepl("Internship|Field", webPageData)]
    
    if(length(webPageData) > 0){
      
      d$WIL[prog] <- webPageData
      
    }
    
    
  }
  
}


d$CIConnexName <- Institution
write_csv(d, paste0("temp/cleaned_data/", gsub(" ", "_", Institution), ".csv"))



#NSCC
#32769b67-ec61-4610-b726-6cbbf4eb6e24
Institution <- CIConnexName("NSCC")
d <- read_csv("temp/2019_Prgs_NSCC_2of2-(Crawl-Run)---2019-07-08T180358Z.csv")
d$WIL <- NA


#h2
#Internship Experience
#Field course
#Co-operative Education     small header
#div.eightcol main__content
#p
#https://www.cbu.ca/academic-programs/program/bachelor-of-business-administration/degree-details/


for(prog in 1:nrow(d)){
  print(prog)
  url <- d$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    webPageData <- webPage %>% 
      html_nodes("h2") %>% 
      html_text()
    
    webPageData <- webPageData[grepl("Work experience", webPageData, ignore.case = T)]
    
    campusInfo <- webPage %>% 
      html_nodes("table.program-grid") %>% 
      html_table(fill = T)
    
    if(length(webPageData) > 0){
      
      d$WIL[prog] <- webPageData
      
    }
    
    if(length(campusInfo) > 0){
      campusInfo <- campusInfo[[1]][1]
      index <- (grep("Campus/Location", unlist(campusInfo)) + 1)
      if(index <= length(unlist(campusInfo))){
        campus <- paste(unlist(campusInfo)[length(unlist(campusInfo))])
      } else {
        campus <- paste(unlist(campusInfo)[(grep("Campus/Location", unlist(campusInfo)) + 1), length(unlist(campusInfo))], sep = " AND ")
      }
      
      d$Campus[prog] <- campus
      
    }
    
    
  }
  
}

d <- d %>% 
  select(-Campus_link)

ncol(d)
colnames(d)
write_csv(d, "temp/cleaned_data/NSCC.csv")




#NBCC
#c7dbe940-1c32-46f2-8d26-2d01f9e8716d
Institution <- CIConnexName("NBCC")

d <- read_csv("temp/2019_Prgs_NBCC_2of2-(Crawl-Run)---2019-09-30T182515Z.csv")
d$WIL <- NA

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)

XpathPossibleError <- function(xpathValue){
  tryCatch(
    remDr$findElement(using = "xpath", xpathValue),
    error = function(e) e
  )
}

for(prog in 1:nrow(d)){
  print(prog)
  url <- d$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    
    #need RSelenium:: solution
    remDr$navigate(url)
    Sys.sleep(sample(2:5, 1))
    
    firstElementErrorCheck = suppressMessages(XpathPossibleError('/html/body/form/div/div[3]/div/div[2]/div[2]/div/div/div[1]/div[1]/div[8]/div[2]/div'))
    if(!inherits(firstElementErrorCheck, "error")) {
      
      webElem <- remDr$findElement(using = "xpath", '/html/body/form/div/div[3]/div/div[2]/div[2]/div/div/div[1]/div[1]/div[8]/div[2]/div')
      webElemTxt <- unlist(webElem$getElementText()) %>% 
        strsplit('\n')
      
      Practicum <- unlist(webElemTxt)[grepl("Practicum|Field placement|Placement|Work experience|Co-op|coop", unlist(webElemTxt), ignore.case = T)]
      
      
      
      if(length(Practicum) > 0){
        
        d$WIL[prog] <- paste(Practicum, collapse = " AND ")
        
      }
      
    } else {
      firstElementErrorCheck = suppressMessages(XpathPossibleError('/html/body/form/div/div[3]/div/div[2]/div[2]/div/div/div[1]/div[1]/div[7]/div[2]/div'))
      if(!inherits(firstElementErrorCheck, "error")) {
        
        webElem <- remDr$findElement(using = "xpath", '/html/body/form/div/div[3]/div/div[2]/div[2]/div/div/div[1]/div[1]/div[7]/div[2]/div')
        webElemTxt <- unlist(webElem$getElementText()) %>% 
          strsplit('\n')
        
        Practicum <- unlist(webElemTxt)[grepl("Practicum|Field placement|Placement|Work experience|Co-op|coop", unlist(webElemTxt), ignore.case = T)]
        
        
        
        if(length(Practicum) > 0){
          
          d$WIL[prog] <- paste(Practicum, collapse = " AND ")
          
        }
        
      }
    }
    
    
    
    
  }
  
}

d <- d %>% 
  select(-Campus_link)

ncol(d)
colnames(d)
d <- d %>% 
  mutate(Duration = gsub("(.*within)(.*year)(s)*(.*)", "\\2\\3", Duration))

write_csv(d, "temp/cleaned_data/NBCC.csv")
d <- read_csv("temp/cleaned_data/NBCC.csv")




# Aurora
## b4b0ea74-5be8-4b3a-8efe-f2bd83c7b4f6

Institution <- CIConnexName("Aurora")

d <- read_csv("temp/2019_Prgs_Aurora_2of2-(Crawl-Run)---2019-07-16T133357Z.csv")

d <- d %>% 
  mutate(WIL = NA) %>% 
  select(url, Program, Credential, Campus, Duration, Description, WIL)



for(prog in 1:nrow(d)){
  print(prog)
  
  
  url <- paste0(d$url[prog])
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    print(paste("moving on with:", prog))
    webPage <- read_html(url)
    
    
    
    
    Courses <- webPage %>% 
      html_nodes("span.prg-text") %>%  
      html_nodes("table") %>% 
      html_table(fill = T)
    
    if(length(Courses) > 0){
      Courses <- Courses %>% 
        purrr::map(~ set_names(., letters[1:ncol(.)])) %>%  
        purrr::map(~ mutate_all(., as.character))
      
      course_df <- Courses %>%
        purrr::map(~ select_if(., function(x) !(all(is.na(x)) | all(x=="")))) %>%
        bind_rows() %>% 
        set_names(c("Code", "Name")) %>% 
        #separate(a, c("Code", "Name"), sep = "[[:space:]]{1,}-[[:space:]]{1,}") %>% 
        select(Code, Name) %>% 
        mutate(Description = NA,
               Institution = Institution,
               Program = d$Program[prog],
               Program_url = d$url[prog]) %>% 
        select(Institution, Program, Program_url, Code, Name, Description)
      
      
      print("found some courses")
      
      dir.create(paste0("temp/Courses/", Institution), showWarnings = FALSE)
      progFileName <- gsub(" |/", "_", gsub('\\"|\n|\\(|\\)|&', "", d$Program[prog]))
      #if(nchar(progFileName) > 50){
      #  progFileName <- substr(progFileName,1,50)
      #}
      write.csv(course_df, paste0("temp/Courses/", Institution, "/", progFileName, ".csv"))
      WIL <- course_df$Name[grepl("Pratique|Practicum|Field placement|Placement|Work experience|Co-op|coop|Apprentice|internship|field practice|clinical practice|clinical work", course_df$Name, ignore.case = T)]
      if(length(WIL) > 0){
        print(paste(WIL, collapse = " AND "))
        d$WIL[prog] <- paste(WIL, collapse = " AND ")
      }
    }
  }
}



ncol(d)
colnames(d)

write_csv(d,paste0("temp/cleaned_data/", Institution, ".csv"))






# Yukon
## 26857b97-0d75-4852-99fa-d90573096d54

Institution <- CIConnexName("Yukon")

d <- read_csv("temp/2019_Prgs_Yukon_2of2-(Crawl-Run)---2019-07-16T153728Z.csv")

d <- d %>% 
  mutate(WIL = NA,
         Campus = NA,
         Credential = NA) %>% 
  select(url, Program, Credential, Campus, Duration, Description, WIL)



for(prog in 1:nrow(d)){
  print(prog)
  
  
  url <- paste0(d$url[prog])
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    print(paste("moving on with:", prog))
    webPage <- read_html(url)
    
    Courses <- webPage %>% 
      html_nodes("div.view-content") %>%  
      html_nodes("table,cols-4") %>% 
      html_table(fill = T)
    
    prog_dets <- webPage %>% 
      html_nodes("div.field") %>% 
      html_nodes("div.shaded") %>% 
      #html_nodes("p") %>% 
      html_text() %>% 
      strsplit("\n") %>% 
      unlist()
    
    #prog_labs <- webPage %>% 
    #  html_nodes("div.field") %>% 
    #  html_nodes("div.shaded") %>% 
    #  html_nodes("h3") %>% 
    #  html_text()
    
    if(length(prog_dets) > 0){
      Credential <- paste(prog_dets[grepl("degree|certificate|credential|diploma|bachelor", prog_dets, ignore.case = T)], collapse = " OR ")
      if(length(Credential) > 0){
        print(Credential)
        d$Credential[prog] <- Credential
      }
      
      Campus <- paste(prog_dets[grepl("campus|online", prog_dets, ignore.case = T)], collapse = " OR ")
      if(length(Campus) > 0){
        print(Campus)
        d$Campus[prog] <- Campus
      }
    }
    
    if(length(Courses) > 0){
      Courses <- Courses %>% 
        purrr::map(~ set_names(., letters[1:ncol(.)])) %>%  
        purrr::map(~ mutate_all(., as.character))
      
      course_df <- Courses %>%
        purrr::map(~ select_if(., function(x) !(all(is.na(x)) | all(x=="")))) %>%
        bind_rows() %>% 
        #set_names(c("Code", "Name")) %>% 
        separate(a, c("Code", "Name"), sep = "[[:space:]]{1,}-[[:space:]]{1,}") %>% 
        select(Code, Name) %>% 
        mutate(Description = NA,
               Institution = Institution,
               Program = d$Program[prog],
               Program_url = d$url[prog]) %>% 
        select(Institution, Program, Program_url, Code, Name, Description)
      
      
      print("found some courses")
      
      dir.create(paste0("temp/Courses/", Institution), showWarnings = FALSE)
      progFileName <- gsub(" |/", "_", gsub('\\"|\n|\\(|\\)|&', "", d$Program[prog]))
      #if(nchar(progFileName) > 50){
      #  progFileName <- substr(progFileName,1,50)
      #}
      write.csv(course_df, paste0("temp/Courses/", Institution, "/", progFileName, ".csv"))
      WIL <- course_df$Name[grepl("Pratique|Practicum|Field placement|Placement|Work experience|Co-op|coop|Apprentice|internship|field practice|clinical practice|clinical work", course_df$Name, ignore.case = T)]
      if(length(WIL) > 0){
        print(paste(WIL, collapse = " AND "))
        d$WIL[prog] <- paste(WIL, collapse = " AND ")
      }
    }
  }
}



ncol(d)
colnames(d)

write_csv(d,paste0("temp/cleaned_data/", Institution, ".csv"))

#### Need to fix this




###Nunavut Arctic College
Institution <- Institution <- CIConnexName("Nunavut Arctic")

url <- "https://static1.squarespace.com/static/5b1954d75cfd798b94327249/t/5ca667df74e0e50001aa9d32/1554409446952/2019-2020+Academic+Program+v.2.pdf"

d <- pdftools::pdf_text(url)
Programs <- d %>% 
  strsplit("\r\n") %>% 
  unlist()

Programs <- Programs[grepl("^Degree|^Post-Diploma|^Diploma|^Certificate|^Non-Post|^Apprenticeship|^Pre-Appren", Programs)] %>% 
  gsub("Post-Diploma Certificate ", "Post-Diploma Certificate  ", .) %>% 
  gsub("(Nunatta)([[:space:]]{1,})(Community)", "\\1_NA_NA_NA_\\3", .) %>% 
  gsub("(Trades)([[:space:]]{1,})(N/A)", "\\1_NA_NA_\\3", .) %>% 
  gsub("(\\))([[:space:]]{1,})(Certificate)", "\\1_NA_\\3", .) %>% 
  gsub("(Technical Qualifications)( )(Rankin Inlet)", "\\1_\\3", .) %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("(\\))([A-Z])", "\\1_\\2", .) %>% 
  gsub("(\\))( [A-Z])", "\\1_\\2", .) %>% 
  strsplit("_") %>% 
  unlist() %>% 
  strToDf(., 8, c("Career", "Program", "Campus", "Campus_responsible", "Starts", "Ends", "Credential", "Division"))


###figure out where it breaks above
##
##for(i in 1:length(Programs)){
##  if(i %% 8 == 0){
##    print(paste(i, Programs[i]))
##  }
##}

d <- Programs %>% 
  mutate(joinName = gsub(" -.*$|\\*| \\(.*$", "", Program))

url <- "https://www.arcticcollege.ca/programs"
webPage <- read_html(url) 

urls <- webPage %>%
  html_nodes("a.image-overlay") %>% 
  html_attr("href")

d_extension <- tibble(Program = NA,
                      Program_url = NA,
                      joinName = NA)


for(topic in 1:length(urls)){
  print(topic)
  url <- paste0("https://www.arcticcollege.ca", urls[topic])
  
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
    
    
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    prog_titles <- webPage %>% 
      html_nodes("h2.programTitle") %>% 
      html_text()
    
    prog_url <- webPage %>% 
      html_nodes("a.sqs-block-button-element--small") %>% 
      html_attr("href") %>% 
      paste0("https://www.arcticcollege.ca", .)
    
    if(length(prog_url) > length(prog_titles)){
      prog_subtitles <- webPage %>% 
        html_nodes("h2") %>% 
        html_text() %>% 
        gsub("\n|[[:space:]]{2,}", "", .)
      prog_titles <- prog_subtitles[-2]
    }
    
    if(length(prog_titles) > 0){
      programDf <- tibble(Program = prog_titles,
                          Program_url = prog_url,
                          joinName = gsub("^[[:space:]]", "", gsub(" -.*$|\\*| \\(.*$|\n", "", prog_titles)))
      
      d_extension <- d_extension %>% 
        bind_rows(programDf)
      
      
    }
  }
}

d_extension <- d_extension %>% 
  mutate(Program = gsub("\n ", "", Program),
         Program = gsub("^[[:space:]]{1,}", "", Program),
         joinName = gsub("[[:space:]]{1,}$", "", gsub("^ |Program", "", joinName ))) %>% 
  filter(!is.na(Program))

d_joined <- d %>% 
  left_join(d_extension %>% 
              rename(Program_joined = Program,
                     Program_url_joined = Program_url), by = "joinName")

d_joined_sep <- d_joined %>% 
  separate(Program, c("Program", "Year") , sep = " - ") %>% 
  separate(Year, c("Label", "Year"), sep = " ") %>% 
  group_by(Program) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Duration = paste(max(Year, na.rm = T), "Years"),
         Campus = paste(unique(Campus), collapse = " AND ")) %>% 
  select(-c(Label, Year, Campus_responsible, Starts, Ends, Division, joinName, Program_joined, Credential)) %>% 
  distinct() %>% 
  set_names(c("Credential", "Program", "Campus", "Program_url", "Duration")) %>% 
  mutate(WIL = NA,
         Description = NA) %>%
  select(c(Program_url, Program, Credential, Campus, Duration, Description, WIL))


ncol(d_joined_sep)
colnames(d_joined_sep)
write_csv(d_joined_sep,paste0("temp/cleaned_data/", Institution, ".csv"))



#Holland
Institution <- CIConnexName("Holland")
urls <- read_html("https://www.hollandcollege.com/programs/#programList1") %>% 
  html_nodes(xpath = '//*[@id="programList"]/div/div[1]/div[2]/div') %>% 
  html_nodes("a") %>% 
  html_attr("href")


urls <- gsub(" ", "%20", urls)
urls <- paste0("https://www.hollandcollege.com/programs/", urls)

d <- tibble(
  Program_url = urls,
  Program = NA,
  Credential = NA,
  Campus = NA,
  Duration = NA,
  Description = NA,
  WIL = NA)


for(prog in 1:length(urls)){
  print(prog)
  
  url <- d$Program_url[prog]
  
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    if(length(webPage %>% 
              html_nodes("h2.programTitle") %>% 
              html_text()) > 0){
      d$Program[prog] <- webPage %>% 
        html_nodes("h2.programTitle") %>% 
        html_text()
      
      d$Credential[prog] <- webPage %>% 
        html_node(xpath = '/html/body/div[4]/div/div[1]/div[2]/div[2]/span[2]') %>% 
        html_text()
      
      d$Campus[prog] <- webPage %>% 
        html_node(xpath = '/html/body/div[4]/div/div[1]/div[2]/div[1]/span[2]') %>% 
        html_text() %>% 
        paste(., collapse = " AND ")
      
      d$Duration[prog] <- webPage %>% 
        html_node(xpath = '/html/body/div[4]/div/div[1]/div[1]/div[2]/span[2]') %>% 
        html_text()
      
      d$Description[prog] <- webPage %>% 
        html_node(xpath = '/html/body/div[4]/div/div[3]') %>% 
        html_text() %>% 
        paste(., collapse = " AND ")
      
      WILurl <- webPage %>% 
        html_node(xpath = '/html/body/div[4]/div/div[3]/div') %>% 
        html_nodes("a") %>% 
        html_attr("href")
      
      if(length(WILurl) > 0){
        WILurl <- WILurl[grepl("course-details-list", WILurl)] 
        if(length(WILurl) > 0){
          WILurl <-  if(!grepl("http", WILurl)) {
            paste0("https://www.hollandcollege.com", WILurl)
          } else {
            WILurl
          }
          WILwebPage <- read_html(WILurl)
          
          
          
          
          Courses <- WILwebPage %>% 
            html_node(xpath = '/html/body/div[3]/div') %>% 
            html_nodes("h2") %>% 
            html_text()
          
          
          CourseDescs  <- WILwebPage %>% 
            html_node(xpath = '/html/body/div[3]/div') %>% 
            html_nodes("div") %>% 
            html_text() %>% 
            strsplit("\n") %>% 
            set_names(1:length(.))
          
          if(length(Courses) > 1){
            Courses <- Courses[2:length(Courses)] %>% 
              as_tibble() %>% 
              set_names("Name") %>% 
              filter(!grepl("^take - ", Name, ignore.case = T))
            
            course_df <- CourseDescs %>% 
              bind_rows() %>% 
              filter(`1` != "") %>% 
              gather(Description, Details) %>% 
              mutate(Index = rep(1:2, nrow(.)/2)) %>% 
              spread(Index, Details) %>% 
              mutate(Description = as.numeric(Description)) %>% 
              arrange(Description) %>% 
              separate(`2`, c("Hours", "Code"), sep = "Code: ") %>% 
              select(-c(Description, Hours)) %>% 
              set_names(c("Description", "Code")) %>% 
              bind_cols(Courses) %>%
              mutate(Description = NA,
                     Program = d$Program[prog],
                     Program_url = url,
                     Institution = Institution) %>% 
              select(Institution, Program, Program_url, Code, Name, Description)
            
            print("found some courses")
            
            dir.create(paste0("temp/Courses/", Institution), showWarnings = FALSE)
            progFileName <- gsub(" |/|:|,", "_", gsub("\n|\\(|\\)|&", "", d$Program[prog]))
            if(nchar(progFileName) > 50){
              progFileName <- substr(progFileName,1,50)
            }
            write.csv(course_df, paste0("temp/Courses/", Institution, "/", progFileName, ".csv"))
            WIL <- course_df$Name[grepl("Pratique|Practicum|Field placement|Placement|Work experience|Co-op|coop|Apprentice|internship|field practice|clinical practice|clinical work", course_df$Name, ignore.case = T)]
            if(length(WIL) > 0){
              print(paste(WIL, collapse = " AND "))
              d$WIL[prog] <- paste(WIL, collapse = " AND ")
            }
          }
          
        }
        
      }
      
    }
    
  }
  
}


ncol(d)
colnames(d)
write_csv(d,paste0("temp/cleaned_data/", Institution, ".csv"))

