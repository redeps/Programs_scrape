
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


