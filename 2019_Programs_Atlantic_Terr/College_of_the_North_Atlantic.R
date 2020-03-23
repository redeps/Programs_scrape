
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
