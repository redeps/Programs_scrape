

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
  tryCatch({
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
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

names(programs)
programs <- programs %>% 
  select(institution_name, prov_terr, record_id, member, `Practical Experience:`:WIL) %>% 
  set_names(c("institution_name", "prov_terr", "record_id", "member", "WIL", "Duration", "Campus", "url", "Program", "Credential", "Description", "WIL_2")) %>% 
  mutate(WIL = ifelse(!is.na(WIL), WIL_2, WIL))

#the following funciton selects the right columns (and order) and saves out an rds and csv file
save_out_standard_file(programs, Institution$institution_name, "Atlantic")

