library(tidyverse)
library(rvest)
library(RSelenium)
setwd("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs")

##### Declaring Functions ########
strToDf <- function(string, columnsToProduce = 2, columnNames = c("Code", "Name")){
  stringDf = tibble(Values = string) %>% 
    mutate(Categories = rep(1:columnsToProduce, (nrow(.)/columnsToProduce))) %>% 
    mutate(Groupings = rep(1:(nrow(.)/columnsToProduce), each = columnsToProduce)) %>% 
    spread(Categories, Values ) %>% 
    select(-Groupings) %>% 
    set_names(columnNames)
  
  return(stringDf)
}

CIConnexName <- function(x){
  return(
    colleges %>% 
      filter(grepl(x, institution_name))
  )
}


### scraping helper functions

get_urls <- function(class, url){
  
  read_html(url) %>% 
    html_nodes(class) %>% 
    html_nodes("a") %>% 
    html_attr("href")
}


possibleError <- function(url){
  
  Error = tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  return(Error)
}

possibleSelError <- function(remDr, xpath){
  
  Error = tryCatch(
    remDr$findElement(using = "xpath", xpath),
    error = function(e) e
  )
  
  return(Error)
}

clean_tags <- function(stringVar){
  return(gsub("^[[:blank:]]+", "", gsub("[[:blank:]]{2,}", " ", gsub("\r|\n|\t", "", stringVar))))
}

# key workhorse - reads in html structure
read_webPage <- function(url){
  
  if(!inherits(possibleError(url), "error")){
    webPage = read_html(url)
    return(webPage)
  }
}

# key workhorse - gets text from specific class in html structure
get_details <- function(webPage, class, urlGrab = F){
  
  if(urlGrab == F){
    detail = webPage %>% 
      html_nodes(class) %>% 
      html_text()
    
    detail = sapply(detail, function(x) clean_tags(x))
  } else {
    detail = webPage %>% 
      html_nodes(class) %>% 
      html_attr("href")
  }
  
  return(detail)
  
}

# key workhorse - gets table from specific class in html structure
get_details_table <- function(webPage, class){
  detail = webPage %>% 
    html_nodes(class) %>% 
    html_table(fill = T)
  return(detail)
}

get_details_id <- function(webPage, id, class = NA){
  if(is.na(class)){
    detail = webPage %>% 
      html_nodes(xpath = paste0('//*[@id="', id, '"]')) %>% 
      html_text()
  } else {
    detail = webPage %>% 
      html_nodes(class) %>% 
      html_nodes(xpath = paste0('//*[@id="', id, '"]')) %>% 
      html_text()
    
  }
  
  detail = sapply(detail, function(x) clean_tags(x))
  
  return(detail)
}

## using regex search through first set of texts to find a specific node within nodes
# Use only if there is no other tag to go by
# add 3rd class if you want to go one step deeper
get_details_index <- function(webPage, searchString, class_1, class_2, class_3 = NA, urlGrab = F){
  index = webPage %>% 
    html_nodes(class_1) %>% 
    html_text()
  
  index = grep(searchString, index, ignore.case = T)
  
  detail = webPage %>% 
    html_nodes(class_2)
  
  if(length(index) > 0){
    if(urlGrab == F) {
      if(is.na(class_3)){
        detail = detail[index] %>% 
          html_text()
        
        detail = sapply(detail, function(x) clean_tags(x))
        return(detail)
      } else {
        detail = detail[index] %>% 
          html_nodes(class_3) %>% 
          html_text()
        
        detail = sapply(detail, function(x) clean_tags(x))
        return(detail)
      }
    }
    
    if(urlGrab == T) {
      if(is.na(class_3)){
        detail = detail[index] %>% 
          html_attr("href")
        
        return(detail)
      } else {
        detail = detail[index] %>% 
          html_nodes(class_3) %>% 
          html_attr("href")
        
        return(detail)
      }
    }
    
  }
  
  return(NA)
}

#Function for finding button to click, then finding subsequently revealed data using RSelenium variable
find_and_click <- function(remDr, clickPath, dataPath, pause = .75){
  
  
  if(!inherits(possibleSelError(remDr, clicPath), "error")){
    webElem <- remDr$findElement(using = "xpath", clickPath)
    Sys.sleep(pause)
    webElem$clickElement()
    Sys.sleep(pause)
    
    if(!inherits(possibleSelError(remDr, dataPath), "error")){
      errorCheck = possibleSelError(remDr, dataPath)
      returnData <- remDr$findElement(using = "xpath", dataPath)
      return(returnData)
    } else {
      return(NA)
    }
    
  }else {
    return(NA)
  }
  
}

# key workhorse - combines class names and class content (if structured differently)
combine_details <- function(webPage, class_names, class_content){
  names = get_details(webPage, class_names)
  content = get_details(webPage, class_content)
  
  detailDf = tibble(names = names,
                    content = content) %>% 
    spread(names, content)
  
  return(detailDf)
}


create_program_df <- function(Institution, url = NA, Program = NA, Credential = NA, Campus = NA, Duration = NA, Description = NA, WIL = NA){
  program = Institution %>% 
    left_join(tibble(institution_name = Institution$institution_name,
                     url = url, 
                     Program = Program, 
                     Credential = Credential, 
                     Campus = Campus, 
                     Duration = Duration, 
                     Description = Description, 
                     WIL = WIL),
              by = "institution_name")
  
  return(program)
}

get_courses <- function(url, isTable = T, tableContClass){
  if(!is.na(url)){
    #reading in web page structure
    webPage = read_webPage(url)
    
    #checking to see if web structure includes the table tag
    includesTable = webPage %>% grepl("<table", .)
    #print(includesTable)
    if(isTable == T & includesTable == T){
      courses = webPage %>% 
        html_nodes(tableContClass) %>%
        html_nodes("table") %>% 
        html_table(fill = T) %>% 
        bind_rows() %>% 
        as_tibble() 
      
      #Returning data frame (tibble)
      return(courses)
    } else {
      courses = tibble()
    }
  } else {
    courses = tibble()
  }
}

clean_string <- function(stringVar){
  return(gsub(" |/", "_", gsub('\\"|\n|\\(|\\)|&|-', "", stringVar)))
}


##### This is the shakiest funciton - getting course data
course_eval <- function(courses, Institution, Program, Program_url, noDescription = T){
  
  if(noDescription == T){
    course_df = courses %>% 
      mutate(Institution = Institution,
             Program = Program,
             Program_url = Program_url,
             Description = NA) %>% 
      select(Institution, Program, Program_url, Code, Name, Description)
  } else {
    course_df = courses %>% 
      mutate(Institution = Institution,
             Program = Program,
             Program_url = Program_url) %>% 
      select(Institution, Program, Program_url, Code, Name, Description)
  }
  
  
  
  dir.create(paste0("temp/Courses/", clean_string(Institution)), showWarnings = FALSE)
  progFileName <- gsub(" |/|_2,}", "_", gsub('\\"|\n|\\(|\\)|&|-', "", Program))
  if(nchar(progFileName) > 50){
    progFileName <- substr(progFileName,1,50)
  }
  write.csv(course_df, paste0("temp/Courses/", clean_string(Institution), "/", progFileName, ".csv"))
  WIL <- course_df$Name[grepl("Pratique|Practicum|Field placement|Placement|Work experience|Co-op|coop|Apprentice|internship|field practice|clinical practice|clinical work|work term| stage ", course_df$Name, ignore.case = T)]
  if(length(WIL) > 0){
    #print(paste(WIL, collapse = " AND "))
    return(paste(WIL, collapse = " AND "))
  }
}



save_out_standard_file <- function(programs, Institution, region){
  programs = programs %>% 
    select(institution_name, prov_terr, record_id, member, url, Program, Credential, Campus, Duration, Description, WIL)
  
  old_files = list.files(paste0("temp/Scrapes/",region))
  old_files = old_files[grepl(clean_string(Institution), old_files)]
  
  if(length(old_files) > 0){
    file.copy(paste0("temp/Scrapes/",region, "/",old_files), paste0("temp/Scrapes/",region, "/Archive/",old_files))
    file.remove(paste0("temp/Scrapes/",region, "/",old_files))
  }
  
  write_rds(programs, paste0("temp/Scrapes/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_programs.rds"))
  write_csv(programs, paste0("temp/Scrapes/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_programs.csv"), na = "")
  if(exists("Fees")){
    write_rds(programs, paste0("temp/Scrapes/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_fees.rds"))
    
  }
}

### End data handling helper functions



#### Reading in helper data sets
colleges <- read_rds("temp/All_colleges.rds")
colleges %>% 
  filter(grepl("Nova Scotia|New Brunswick|Newfoundland|Prince|Terr|Nuna|Yuk", prov_terr))  %>% 
  mutate(member = substr(record_id, 8,12),
         member = ifelse(member != "", T, F)) %>% 
  as.data.frame()

#Remove:
#college de technologie forestiere des maritimes             New Brunswick
#moncton hospital school of radiologic technology             New Brunswick
#saint john school of radiation therapy             New Brunswick

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

#############################
#### CCNB
## Type of website: semi structured, unstructured text for programs
Institution <- CIConnexName("Collège communautaire du Nouveau-Brunswick")


url <- "https://ccnb.ca/programme-detudes/nos-programmes.aspx"


### Setting up dataset to run scrape on
program_urls <- read_html(url) %>% 
  html_nodes("table.programs-list") %>% 
  html_table() %>% 
  bind_rows() %>% 
  filter(X2 != "BA")
#X2:X6 == "Campus"

program_urls$url <- read_html(url) %>% 
  html_nodes("table.programs-list") %>% 
  html_nodes("td") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  paste0("https://ccnb.ca/programme-detudes/", .)

program_urls <- program_urls %>% 
  set_names(c("Program", "Bathurst", "Campbellton", "Dieppe", "Edmundston", "Peninsule_acadienne", "profile", "url")) %>% 
  select(-profile)

remove(programs)
remove(Fees)
remove(courses)
for(url in 1:nrow(program_urls)){
  print(url)
  webPage = read_webPage(program_urls$url[url])
  program = create_program_df(Institution = Institution,
                              Program = program_urls$Program[url],
                              Campus = program_urls[url,] %>% 
                                select(Bathurst:Peninsule_acadienne) %>% 
                                gather(Campus, value) %>% 
                                filter(value != "") %>% 
                                select(Campus) %>%
                                summarize(Campus = paste(Campus, collapse = "_")) %>% 
                                paste() ,
                              url = program_urls$url[url],
                              Duration = get_details_id(webPage, "ContentWrapperContentPlaceHolder_ProgramBrowser_5_ProgramViewer_7_ProgramDurationParagraph", class = "div.userContent"),
                              Description = get_details_id(webPage, "ContentWrapperContentPlaceHolder_ProgramBrowser_5_ProgramViewer_7_ProgramOverviewParagraph", class = "div.userContent"))
  
  
  courses = webPage %>% 
    html_nodes(xpath = '//*[@id="ContentWrapperContentPlaceHolder_ProgramBrowser_5_divProgramViewer"]/div[5]/ul[1]') %>% 
    html_nodes("li") %>% 
    html_text() %>% 
    clean_tags(.)
  
  #Cleaning file for course function - needs to produce a df with Code and Name columns
  if(length(courses) > 0){
    courses = tibble(Code = NA,
                     Name = courses)
    
    program$WIL = course_eval(courses, Institution$institution_name, program$Program, program$url)
    
  }
  
  if(exists("programs")){
    programs = programs %>% 
      bind_rows(program)
  } else {
    programs = program
  }
  
}
programs <- programs %>% 
  separate(Duration, c("Duration", "WIL_2"), sep = ",") %>% 
  mutate(WIL = ifelse(is.na(WIL), WIL_2, WIL)) %>% 
  select(-WIL_2)


#the following funciton selects the right columns (and order) and saves out an rds and csv file
save_out_standard_file(programs, Institution$institution_name, "Atlantic")

#############################
##### Martime College of Forest tech
## Type of website: very untidy - needs a more manual approach
Institution <- CIConnexName("Maritime College of Forest Technology")

url <- "https://mcft.ca/en/explore/program"

## untidy website
webPage <- read_webPage(url)

programs = create_program_df(Institution = Institution,
                             Program = get_details(webPage, class = "h1"),
                             Campus = "Fredricton AND Bathurst",
                             url = url,
                             Duration = "Two years",
                             Description = webPage %>% html_nodes(xpath = '//*[@id="content"]/p[1]') %>% html_text(),
                             WIL = "10 week practicum") %>% 
  filter(Program != "") %>% 
  bind_rows(create_program_df(Institution = Institution,
                              Program = get_details(webPage, class = "h2"),
                              Campus = "Fredricton AND Bathurst",
                              url = url,
                              Duration = "Two years",
                              Description = webPage %>%  html_nodes(xpath = '//*[@id="content"]/p[7]') %>%   html_text(),
                              WIL = NA)
  )



courses = tibble(Code = NA,
                 Name = get_details(webPage %>% html_nodes("dl.twocolumn"), class = "li"))

course_eval(courses, Institution$institution_name, programs$Program[1], url)

tuition <- read_webPage("https://mcft.ca/en/explore/tuition-and-fees") %>% 
  html_nodes("table.tuitionTable") %>% 
  html_table()

tuition_ad_dip <- tuition[[5]] %>% 
  spread(X1, X2) %>% 
  janitor::clean_names() %>% 
  select(total_fish_wildlife) %>% 
  set_names("Totals") %>% 
  mutate(Program = programs$Program[2],
         url = url) 

Fees <- tuition[1:4] %>% 
  bind_rows() %>% 
  set_names(.[1,]) %>% 
  filter(!Totals %in% c("Totals")) %>% 
  separate(Totals, c("Totals", "Grad_fees"), sep = " \\+ ") %>% 
  filter(Single != "Double") %>% 
  mutate(Semester = as.character(c(rep.int(1, 3), rep.int(2, 3))),
         Res_type = rep(c("Single", "Double", "Single"), 2),
         Program = programs$Program[1],
         url = url) %>% 
  bind_rows(tuition_ad_dip %>% 
              mutate(Semester = "All"))

save_out_standard_file(programs, Institution$institution_name, "Atlantic")


#############################
##### New Brunswick College of Craft and Design
## Type of website: Releatively structured, info in unstructured text - using xpaths and string matching
Institution <- CIConnexName("New Brunswick College of Craft and Design")
program_urls <- tibble(url = read_webPage("https://nbccd.ca/programs/overview/") %>% 
                         html_nodes(xpath = '//*[@id="post-41"]/div[1]/ul') %>% 
                         html_nodes("a") %>% 
                         html_attr("href"))

Credentials = tibble(Credential = c("Certificate", "Diploma", "Bachelor of Applied Arts with UNB"),
                     text = read_webPage("https://nbccd.ca/programs/overview/") %>% 
                       html_nodes(xpath = '//*[@id="post-41"]/div[1]/ul') %>% 
                       html_nodes("li") %>% 
                       html_text())

tuition <- read_webPage("https://nbccd.ca/admissions/tuition-fees/") %>% 
  html_nodes(xpath = '//*[@id="post-75"]/div[2]/div/section[1]/div/table[1]/tbody/tr[3]/td[2]/text()[1]') %>% 
  html_text()

remove(programs)
remove(Fees)
remove(fees)
#for(url in 1:1){
for(url in 1:nrow(program_urls)){
  print(url)
  webPage = read_webPage(program_urls$url[url])
  
  Description = get_details(webPage, "div.col6")[1] %>% strsplit(., "Duration|Interested in finding out more")
  names(Description) = "Description"
  webPage %>% 
    html_nodes("h6") %>% 
    html_text()
  webPage %>% 
    html_nodes("div.inner")
  
  
  program = create_program_df(Institution = Institution,
                              Program = get_details(webPage, "h2"),
                              Campus = "NBCCD" ,
                              url = program_urls$url[url],
                              Duration = Description$Description[2],
                              Description = Description$Description[1])
  
  program$Credential = Credentials$Credential[grep(program$Program, Credentials$text)]
  
  courses = tibble(Code = NA,
                   Name = get_details_index(webPage, "course", "h6", "div.inner", "li"))
  program$WIL = course_eval(courses, Institution$institution_name, program$Program, program$url)
  
  fees <- tibble(Program = program$Program,
                 url = program$url,
                 Full_time_tuition_per_year = tuition)
  
  if(!exists("Fees")){
    Fees <- fees
  } else {
    Fees <- rbind(Fees, fees)
  }
  
  if(exists("programs")){
    programs = programs %>% 
      bind_rows(program)
  } else {
    programs = program
  }
  
}

save_out_standard_file(programs, Institution$institution_name, "Atlantic")


#############################
##### New Brunswick Community College (NBCC)
## Type of website: Tidy url grab, unstructured text on program pages
Institution <- CIConnexName("New Brunswick Community College")

program_urls <- get_details_table(read_webPage("https://nbcc.ca/programs-courses/full-time-programs"), "table.table-striped" ) %>% 
  bind_rows() %>% 
  set_names(c("Program", "Campus")) %>%
  mutate(url = read_webPage("https://nbcc.ca/programs-courses/full-time-programs") %>% 
           html_nodes("table.table-striped") %>% 
           html_nodes('a') %>% 
           html_attr("href"),
         Credential = "Full time program") %>% 
  bind_rows(get_details_table(read_webPage("https://nbcc.ca/programs-courses/specialized-full-time-programs"), "table.k-table" ) %>% 
              bind_rows() %>% 
              set_names(c("Program", "Campus")) %>% 
              mutate(url = read_webPage("https://nbcc.ca/programs-courses/specialized-full-time-programs") %>% 
                       html_nodes("table.k-table") %>% 
                       html_nodes('a') %>% 
                       html_attr("href"),
                     Credential = "Specialized Full time program"))%>% 
  bind_rows(get_details_table(read_webPage("https://nbcc.ca/programs-courses/post-graduate-programs"), "table.k-table" ) %>% 
              bind_rows() %>% 
              set_names(c("Program", "Campus")) %>% 
              mutate(url = read_webPage("https://nbcc.ca/programs-courses/post-graduate-programs") %>% 
                       html_nodes("table.k-table") %>% 
                       html_nodes('a') %>% 
                       html_attr("href"),
                     Credential = "Post graduate program"))%>% 
  bind_rows(get_details_table(read_webPage("https://nbcc.ca/programs-courses/apprenticeship"), "table" ) %>% 
              bind_rows() %>% 
              set_names(c("Program", "Campus")) %>% 
              mutate(url = NA,
                     Credential = "Apprenticeship"))%>% 
  bind_rows(get_details_table(read_webPage("https://nbcc.ca/programs-courses/professional-development"), "table.k-table" ) %>% 
              bind_rows() %>% 
              set_names(c("Program", "Campus")) %>% 
              mutate(url = read_webPage("https://nbcc.ca/programs-courses/professional-development") %>% 
                       html_nodes("table.k-table") %>% 
                       html_nodes('a') %>% 
                       html_attr("href"),
                     Credential = "Other"))

program_urls <- program_urls %>% 
  mutate(url = ifelse((!is.na(url) & !grepl("^http", url)), paste0("https://nbcc.ca", url), url))


remove(programs)
remove(Fees)
remove(fees)
#for(url in 1:1){
for(url in 1:nrow(program_urls)){
  print(url)
  webPage = read_webPage(program_urls$url[url])
  
  program = create_program_df(Institution = Institution,
                              Program = program_urls$Program[url],
                              Campus = program_urls$Campus[url] ,
                              url = program_urls$url[url],
                              Credential = program_urls$Credential[url])
  
  ## Separate scrape for each type of 'credential'
  if(program_urls$Credential[url] == "Full time program"){
    program$Description = get_details(webPage, "p.mb-lg")[1]
    
    courses = tibble(Name = webPage %>% 
                       html_nodes("div.toggle-primary") %>% 
                       get_details("label"),
                     Description = webPage %>% 
                       html_nodes("div.toggle-primary") %>% 
                       get_details("p")) %>% 
      separate(Name, c("Code", "Name"), sep = " - ")
    
    program$WIL = course_eval(courses, Institution$institution_name, program$Program, program$url, noDescription = F)
    
    fee_url = webPage %>% 
      html_nodes("div.col-md-4") %>% 
      html_nodes("a") %>% 
      html_attr('href')
    
    fee_url = fee_url[grepl("tuition", fee_url)]
    
    fees = read_webPage(paste0("https://nbcc.ca", gsub(" ", "", fee_url)))%>% 
      html_nodes('div.tuitionfees') %>% 
      html_nodes("table.k-table") %>% 
      html_nodes('td') %>% 
      html_text()
    
    fees = tibble(values = fees[1:9],
                  index = rep(1:3, each = 3),
                  category = rep(c("Term", "Domestic", "International"), 3)) %>% 
      spread(category, values) %>% 
      mutate(Program = program$Program,
             url = program$url) %>% 
      select(-index) %>% 
      gather(Category, Fee, Domestic:International)
    
    program$Duration = get_details_index(webPage %>% html_nodes("div.feature-box-info"), class_1 = "h4", class_2 = "p", searchString = "duration")
    
    if(!exists("Fees")){
      Fees <- fees
    } else {
      Fees <- rbind(Fees, fees)
    }
    
    if(exists("programs")){
      programs = programs %>% 
        bind_rows(program)
    } else {
      programs = program
    }
    
  }
  if(program_urls$Credential[url] == "Specialized Full time program"){
    program_details = get_details_table(webPage, "table.k-table") %>% 
      bind_rows()
    
    names(program_details) <- gsub("Cost", "Tuition", names(program_details), ignore.case = T)
    
    program$Duration = program_details$Duration
    program$Campus = program_details$Location
    
    fees = tibble(Program = program$Program,
                  url = program$url,
                  Fee = program_details$Tuition,
                  Term = NA,
                  Category = "Tuition")
    
    program$Description = get_details(webPage, "div.col-md-9")
    program$Credential = get_details_index(webPage %>% html_nodes("div.col-md-9"), class_1 = "p", class_2 = "p", searchString = "Credential")
    
    if(!exists("Fees")){
      Fees <- fees
    } else {
      Fees <- rbind(Fees, fees)
    }
    
    if(exists("programs")){
      programs = programs %>% 
        bind_rows(program)
    } else {
      programs = program
    }
    
  }
  if(program_urls$Credential[url] == "Post graduate program"){
    credential = get_details_index(webPage %>% html_nodes("div.col-md-12"), class_1 = "div", class_2 = "div", searchString = "^credential")
    if(!is.na(credential)) {
      credential = credential %>% strsplit("\\.") %>% set_names(letters[1:length(.)])
      program$Credential = credential$a[1]
    }
    
    fees = tibble(Category = webPage %>% 
                    html_nodes("div.highlightBox") %>% 
                    html_nodes("ul") %>% 
                    html_nodes("li") %>% 
                    html_text()) %>% 
      separate(Category, c("Category", "Fee"), sep = "\\$") %>% 
      filter(!is.na(Fee)) %>% 
      spread(Category, Fee) %>% 
      janitor::clean_names() %>% 
      gather(Category, Fee) %>% 
      mutate(Program = program$Program,
             url = program$url,
             Term = NA)
    
    Description = get_details(webPage, "div.col-md-8")
    if(length(Description) > 0){
      program$Description = Description
    }
    
    
    
    courses = tibble(Name = webPage %>% 
                       html_nodes("div.toggle-primary") %>% 
                       get_details("label"),
                     Description = webPage %>% 
                       html_nodes("div.toggle-primary") %>% 
                       get_details("p")) %>% 
      separate(Name, c("Code", "Name"), sep = " - ")
    
    program$WIL = course_eval(courses, Institution$institution_name, program$Program, program$url, noDescription = F)
    
    
    if(!exists("Fees")){
      Fees <- fees
    } else {
      Fees <- rbind(Fees, fees)
    }
    
    if(exists("programs")){
      programs = programs %>% 
        bind_rows(program)
    } else {
      programs = program
    }
  }
  if(program_urls$Credential[url] == "Apprenticeship"){
    
    if(exists("programs")){
      programs = programs %>% 
        bind_rows(program)
    } else {
      programs = program
    }
    
  }
  if(program_urls$Credential[url] == "Other"){
    program_details = get_details_table(webPage, "table.k-table") %>% 
      bind_rows()
    
    program$Duration = program_details$Duration %>% paste0(collapse = " AND ")
    program$Campus = program_details$Location %>% paste0(collapse = " AND ")
    
    if(exists("programs")){
      programs = programs %>% 
        bind_rows(program)
    } else {
      programs = program
    }
  }
}


save_out_standard_file(programs, Institution$institution_name, "Atlantic")



#############################
##### Centre for Nursing Studies
## Type of website: Very unstructured, basically a manual entry
Institution <- CIConnexName("Centre for Nursing Studies")

webPage = read_webPage("http://www.cns.nf.ca/programs")
program_urls = tibble(url = get_details_index(webPage %>% html_nodes("div.menu"), class_1 = "li", class_2 = "li", class_3 = "a", searchString = "program", urlGrab = T)) %>% 
  filter(url != "../programs/") %>% 
  mutate(url = gsub("^\\.{2}", "http://www.cns.nf.ca", url))

remove(programs)
remove(Fees)
remove(fees)
#for(url in 1:1){
for(url in 1:nrow(program_urls)){
  print(url)
  webPage = read_webPage(program_urls$url[url])
  
  content = webPage %>% 
    html_nodes("div.contentContainer") %>% 
    html_nodes("p")
  
  program = create_program_df(Institution = Institution,
                              Program = get_details(webPage, "h2"),
                              Campus = "Memorial University",
                              url = program_urls$url[url],
                              Description = content[1] %>% html_text())
  
  
  if(exists("programs")){
    programs = programs %>% 
      bind_rows(program)
  } else {
    programs = program
  }
}

programs <- programs %>% 
  mutate(Credential = case_when(grepl("bachelor", Program, ignore.case = T) ~ "Bachelor degree",
                                grepl("practical nursing", Program, ignore.case = T) ~ "Diploma",
                                grepl("continuing nursing", Program, ignore.case = T) ~ "Other"),
         Duration = case_when(grepl("bachelor", Program, ignore.case = T) ~ "4 years",
                              grepl("practical nursing", Program, ignore.case = T) ~ "16 months",
                              grepl("continuing nursing", Program, ignore.case = T) ~ ""))


save_out_standard_file(programs, Institution$institution_name, "Atlantic")


#############################
##### College of the North Atlantic (CNA)
## Type of website: Very orderly, but behind buttons
## RSelenium solution
Institution <- CIConnexName("College of the North Atlantic")
webPage = read_webPage("https://www.cna.nl.ca/programs-courses/")


program_urls <- tibble(Program = get_details(webPage %>% html_nodes("div.cmsData") %>% html_nodes("div") %>% html_nodes("span"), "a"),
                       url = get_details(webPage %>% html_nodes("div.cmsData") %>% html_nodes("div") %>% html_nodes("span"), "a", urlGrab = T))

details <- get_details(webPage %>% html_nodes("div.cmsData"), "div")
details <- details[grepl("duration", details, ignore.case = T)] 
program_urls <- program_urls %>% 
  mutate(Detail = details) %>% 
  filter(!grepl("Reset Filters", Program, ignore.case = T)) %>% 
  separate(Detail, c("Program_2", "Duration", "Credential", "Campus"), sep = "Duration: | \\| Credential: | Campus/Delivery: ") %>% 
  select(-Program_2) %>% 
  mutate(url = paste0("https://www.cna.nl.ca", url))

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)


remove(programs)
remove(Fees)
remove(fees)
for(url in 95:nrow(program_urls)){
  print(url)
  
  
  program = create_program_df(Institution = Institution,
                              Program = program_urls$Program[url],
                              Campus = program_urls$Campus[url],
                              url = program_urls$url[url],
                              Duration = program_urls$Duration[url],
                              Credential = program_urls$Credential[url])
  
  
  remDr$navigate(program_urls$url[url])
  Sys.sleep(1.5)
  
  #Finding button to click, then finding subsequently revealed data
  Description <- find_and_click(remDr, 
                                clickPath = '/html/body/div[2]/main/form/section/div/div/div/div/div[1]/div/div/div/div/div[2]/div[8]/div/div[1]/h3/div', 
                                dataPath = '//*[@id="MainContent_Default1_updatePanel1"]',
                                pause = 0.75)
  
  if(!is.na(Description)){
    program$Description = Description$getElementText() %>% 
      unlist()
  }
  
  
  #Checking to see if value is empty, then running the above again
  if(!is.na(program$Description) & program$Description == ""){
    Description <- find_and_click(remDr, 
                                  clickPath = '/html/body/div[2]/main/form/section/div/div/div/div/div[1]/div/div/div/div/div[2]/div[8]/div/div[1]/h3/div', 
                                  dataPath = '//*[@id="MainContent_Default1_updatePanel1"]',
                                  pause = 1.5)
    
    program$Description = Description$getElementText() %>% 
      unlist()
  }
  
  Sys.sleep(.75)
  #Finding button to click, then finding subsequently revealed data
  courses <- find_and_click(remDr, 
                            clickPath = '/html/body/div[2]/main/form/section/div/div/div/div/div[1]/div/div/div/div/div[2]/div[8]/div/div[3]/h3/div', 
                            dataPath = '/html/body/div[2]/main/form/section/div/div/div/div/div[1]/div/div/div/div/div[2]/div[8]/div/div[4]/div/table')
  
  if(!is.na(courses)){
    courses = tibble(Code = courses$getElementText() %>% 
                       unlist() %>% 
                       strsplit("\n") %>% 
                       unlist())
    
    if(nrow(courses) < 1){
      courses <- find_and_click(remDr, 
                                clickPath = '/html/body/div[2]/main/form/section/div/div/div/div/div[1]/div/div/div/div/div[2]/div[8]/div/div[3]/h3/div', 
                                dataPath = '/html/body/div[2]/main/form/section/div/div/div/div/div[1]/div/div/div/div/div[2]/div[8]/div/div[4]/div/table',
                                pause = 1.5)
      courses = tibble(Code = courses$getElementText() %>% 
                         unlist() %>% 
                         strsplit("\n") %>% 
                         unlist())
    }
  }
  
  
  #Checking if the dataset is empty, then running the above again
  
  
  #If the above doesn't capture courses, then move on - the below filters out empty datasets
  if(!is.na(courses) ){
    if(nrow(courses) > 0){
      courses = courses %>% 
        #filter(grepl("^[A-Z]{2}[[:digit:]]{3,4}", Code)) %>% 
        filter(!grepl("can be completed at campuses|Semester|CODE|Note:|COURSES|The Course and Lab hours per week|\\*", Code)) %>% 
        mutate(Code = ifelse(grepl("^[A-Z]{2}[[:digit:]]{3,4}$", lag(Code)), paste(lag(Code), Code, sep = "_"), Code),
               Code = gsub("(^[A-Z]{2}[[:digit:]]{3,4})([[:space:]]*)([A-Z])", "\\1_\\3", Code)) %>% 
        separate(Code, c("Code", "Name"), sep = "_") %>% 
        filter(!is.na(Name))
      
      program$WIL = course_eval(courses, Institution$institution_name, program$Program, program$url)
      
    }
  }
  
  if(exists("programs")){
    programs = programs %>% 
      bind_rows(program)
  } else {
    programs = program
  }
}

remDr$close()

save_out_standard_file(programs, Institution$institution_name, "Atlantic")


#############################
##### Marine Institute
## Type of website: structured
Institution <- CIConnexName("Marine Institute")


url <- "https://www.mi.mun.ca/programsandcourses/programs/"

program_urls <- tibble(class = read_html(url) %>% 
                         html_nodes("div.two_column_right") %>% 
                         html_nodes("div") %>% 
                         html_attr("class"),
                       text = read_html(url) %>% 
                         html_nodes("div.two_column_right") %>% 
                         html_nodes("div") %>% 
                         html_text(),
                       url = read_html(url) %>% 
                         html_nodes("div.two_column_right") %>% 
                         html_nodes("div"))
## Adding tuition fee links
feeUrl <- "https://www.mi.mun.ca/departments/officeoftheregistrar/programsandfees/"
webPage <- read_html(feeUrl)
feeUrls <- tibble(feeUrl = webPage %>% 
                    html_nodes("ul") %>% 
                    html_nodes("a") %>% 
                    html_attr("href") %>% 
                    paste0("https://www.mi.mun.ca/", .),
                  program = gsub("https://www.mi.mun.ca//departments/officeoftheregistrar/programsandfees/|/$", "", feeUrl))

#Setting up program_urls dataframe with credential names and urls
program_urls <- program_urls %>% 
  separate_rows(url, sep = "<li>|\n") %>% 
  mutate(url= ifelse(grepl("href", url), url, NA),
         url = gsub('<a href=\\"|\\">.*$', "", url)) %>% 
  filter(!is.na(class)) %>% 
  filter(!(class == "section_content" & is.na(url))) %>% 
  mutate(text = zoo::na.locf(ifelse(!is.na(url), NA, text))) %>% 
  filter(!is.na(url)) %>% 
  mutate(url = paste0("https://www.mi.mun.ca", url),
         program = gsub("https://www.mi.mun.ca/programsandcourses/programs/|/$", "", url)) %>% 
  left_join(feeUrls, by = "program")


remove(programs)
remove(Fees)
#for(url in 1:1){
for(url in 1:nrow(program_urls)){
  Credential = program_urls$text[url]
  feeUrl = program_urls$feeUrl[url]
  url = program_urls$url[url]
  print(url)
  
  #Reading in web page structure
  webPage = read_webPage(url)
  feeWebPage = read_webPage(feeUrl)
  
  #Using the combining function "combine_details" created above to combine informationg
  #gathered using the "get_details" function also created above into a data frame (tibble)
  program = combine_details(webPage, "p.inline_detail_title", "p.inline_detail_content")
  program = program %>% 
    mutate(institution_name = Institution$institution_name,
           url = url) 
  program = Institution %>% 
    left_join(program,
              by = "institution_name") %>% 
    mutate(Name = get_details(webPage, "div.page_title"),
           Credential = Credential,
           Description = clean_tags(paste(get_details(webPage, "div.section_content"), collapse = " || ")))
  
  
  #### Getting fee information
  if(length(feeWebPage) > 0){
    fees = get_details_table(feeWebPage, "table.fee_table")
    names(fees) = get_details(feeWebPage, "p.content_title")
    
    for(dS in 1:length(fees)){
      colnames(fees[[dS]])[1] <- "timePeriod"
    }
    
    fees <- fees %>% 
      purrr::map(~ filter(., timePeriod != "" )) %>% 
      purrr::map(~ mutate_all(., as.character)) %>% 
      bind_rows(.id = "name") %>% 
      gather(category, feeValue, 3:ncol(.)) %>% 
      mutate(programUrl = url)
    
    
    if(!exists("Fees")){
      Fees <- fees
    } else {
      Fees <- rbind(Fees, fees)
    }
    
  }
  
  #print(fees)
  
  
  courses = get_urls("div.section_content", url)
  courses = get_courses(courses[grepl("calendar", courses)][1], isTable = T, tableContClass = "div.calendar_content")
  
  if(nrow(courses) > 0){
    #Cleaning file for course function - needs to produce a df with Code and Name columns
    courses = courses %>% 
      gather(Position, Value) %>% 
      mutate(Value = gsub("([A-Z]{4} )", "|\\1", Value)) %>% 
      separate(Value, letters[1:10], sep = "\\|") %>% 
      gather(index, course, letters[2:10]) %>% 
      filter(!is.na(course)) %>% 
      select(-c(a, Position, index)) %>%
      separate(course, c("Code", "Name"), sep = "\\(") %>% 
      mutate(Name = gsub("^ |\\)", "", Name),
             Code = gsub(" $", "", Code)) %>% 
      set_names(c("Code", "Name")) 
    
    #Need to clean institution name before running the below
    program$WIL = course_eval(courses, Institution$institution_name, program$Name, url)
  }
  
  
  if(exists("programs")){
    programs = programs %>% 
      bind_rows(program)
  } else {
    programs = program
  }
  
}

names(programs)
programs <- programs %>% 
  select(institution_name, prov_terr, record_id, member, `Practical Experience:`:WIL) %>% 
  set_names(c("institution_name", "prov_terr", "record_id", "member", "WIL", "Duration", "Campus", "url", "Program", "Credential", "Description", "WIL_2")) %>% 
  mutate(WIL = ifelse(!is.na(WIL), WIL_2, WIL))

#the following funciton selects the right columns (and order) and saves out an rds and csv file
save_out_standard_file(programs, Institution$institution_name, "Atlantic")


#############################
##### Aurora College
## Type of website:
Institution <- CIConnexName("Aurora College")
webPage <- read_webPage("http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsAtoZ.aspx")
program_urls <- tibble(Program = get_details(webPage %>% html_nodes("td.prg-list-text"), "a"),
                       url = paste0("http://www.auroracollege.nt.ca", get_details(webPage %>% html_nodes("td.prg-list-text"), "a", urlGrab = T)))

url_combination <- function(url, xpath_id, credential){
  returnData = read_webPage(url)
  return(tibble(Program = get_details(returnData %>% html_nodes(xpath = paste0('//*[@id="', xpath_id, '"]/table')), "a"),
                url = paste0("www.auroracollege.nt.ca", get_details(returnData %>% html_nodes(xpath = paste0('//*[@id="', xpath_id, '"]/table')), "a", urlGrab = T)))) %>% 
    filter(grepl("[[:alpha:]]", Program)) %>% 
    mutate(Credential = credential)
}

url_df <- tibble(url = c("http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsDiploma.aspx", 
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsDegree.aspx",
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsApprenticeshipTrades.aspx",
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsOther.aspx",
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsAcademicUpgradingAndAccess.aspx",
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsCertificate.aspx"
),
xpath_id = c("CPH2_WPM_wp481640216_wp1279118871_divControlHolder",
             "CPH2_WPM_wp375978756_wp1611025563_divControlHolder",
             "CPH2_WPM_wp1049890377_wp1621563132_divControlHolder",
             "CPH2_WPM_wp1056227953_wp1808783534_divControlHolder",
             "CPH2_WPM_wp950191435_wp121123461_divControlHolder",
             "CPH2_WPM_wp1288918235_wp1396638403_divControlHolder"
),
credential = c("Diploma",
               "Bachelor",
               "Apprenticeship certificate",
               "Other",
               "Other",
               "Certificate"))

for(i in 1:nrow(url_df)){
  credential_df = url_combination(url_df$url[i], url_df$xpath_id[i], url_df$credential[i])
  if(!exists("program_urls")){
    program_urls = credential_df
  } else {
    program_urls = program_urls %>% bind_rows(credential_df)
  }
}


#### integrate data on campuses from http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsByLocation.aspx




#############################
##### Collège Nordique Francophone
## Type of website:
Institution <- CIConnexName("Collège Nordique Francophone")


#############################
#### canadian coast guard college
## Type of website:
Institution <- CIConnexName("canadian coast guard collegey")


#############################
##### Cape Breton University
## Type of website:
Institution <- CIConnexName("Cape Breton University")


#############################
##### Dalhousie Agricultural Campus of Dalhousie University
## Type of website:
Institution <- CIConnexName("Dalhousie Agricultural Campus of Dalhousie University")


#############################
##### halifax regional police training school
## Type of website:
Institution <- CIConnexName("halifax regional police training school")


#############################
##### maritime conservatory of performing arts
## Type of website:
Institution <- CIConnexName("maritime conservatory of performing arts")


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

