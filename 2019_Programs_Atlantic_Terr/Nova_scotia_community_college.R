
#############################
##### Nova Scotia Community College (NSCC)
## Type of website:
Institution <- CIConnexName("Nova Scotia Community College")


url_df <- tibble(Program_url = paste0("https://www.nscc.ca/learning_programs/programs/", get_details(read_webPage("https://www.nscc.ca/learning_programs/programs/ProgramListing.aspx?list=alpha") %>% html_nodes("ul.azWebCal"), class = "a", urlGrab = T)),
                 Program = get_details(read_webPage("https://www.nscc.ca/learning_programs/programs/ProgramListing.aspx?list=alpha") %>% html_nodes("ul.azWebCal"), class = "a"))

                 

remove(Fees)
remove(programs)
for(url in 1:nrow(url_df)){
  tryCatch({
    Program_url = url_df$Program_url[url]
    Program = url_df$Program[url]
    webPage = read_webPage(Program_url)
    print(paste(url, Program_url))
  
    details = tibble(category = get_details(webPage %>% html_nodes("dl.prog-progstart"), class = "dt"),
                     detail = get_details(webPage %>% html_nodes("dl.prog-progstart"), class = "dd"))
    
    #Make the below into a function
    Duration = details %>% group_by(category) %>% filter(grepl("length|duration", category, ignore.case = T)) %>% summarize(detail = paste(detail, collapse = " AND ")) %>% ungroup() %>% select(detail) %>% unlist()
    Credential = details %>% group_by(category) %>% filter(grepl("credential", category, ignore.case = T)) %>% summarize(detail = paste(detail, collapse = " AND ")) %>% ungroup() %>% select(detail) %>% unlist()
    Campus = get_details_table(webPage, 'table.tablesaw.tablesaw-stack') %>% bind_rows() %>% select(1) %>% set_names("campus") %>% summarize(campus = paste(campus, collapse = " AND ")) %>% unlist()
    courses = get_details(webPage %>% html_nodes(xpath = '//*[@id="courses"]'), "p")
    
  
    
    if(length(courses) > 0){
      courses = tibble(Code = courses) %>% 
        mutate(Code = ifelse(!grepl(" - ", Code), gsub("([[:digit:]]{3,4})( [A-Z])", "\\1 -\\2", Code), Code)) %>% 
        separate(Code, c("Code", "Description"), sep = " - ") %>% 
        filter(!is.na(Description)) %>% 
        mutate(Name =  get_details(webPage %>% html_nodes(xpath = '//*[@id="courses"]'), "strong")[1:nrow(.)]) %>% 
        separate(Name, c("Code_drop", "Name"), sep = " - ") %>% 
        select(c(Code, Name, Description))
      
      WIL = course_eval(courses, Institution$institution_name, Program, Program_url, noDescription = F)
    } else {
      WIL = NA
    }
    
    program = create_program_df(Institution, url = Program_url, Program = Program, Credential = Credential, Campus = Campus, Duration = Duration, Description = NA, WIL = WIL)
    
    tuition = tibble(category = get_details(webPage %>% html_nodes("div.tuition-amt-display"), class = "div.tuition-amt-title"),
                     value = get_details(webPage %>% html_nodes("div.tuition-amt-display"), class = "div.tuition-amt-total"))
    
    tuition_url_suggestions = get_details(webPage %>% html_nodes("div.prog-col-3") %>% html_nodes("ul"), class = "a", urlGrab = T)
    names(tuition_url_suggestions) =get_details(webPage %>% html_nodes("div.prog-col-3") %>% html_nodes("ul"), class = "a", urlGrab = F)
    
    tuition_urls = paste0("https://www.nscc.ca/", tuition_url_suggestions[grepl("cost_and_financial_aid", tuition_url_suggestions)])
    
    if(length(tuition_urls) > 0){
      names(tuition_urls) = names(tuition_url_suggestions)[grepl("cost_and_financial_aid", tuition_url_suggestions)]
      
      
      for(tuition_url in 1:length(tuition_urls)){
        tuition_webPage = read_webPage(tuition_urls[tuition_url])
        if(length(get_details_table(tuition_webPage, "table.tablesaw")) > 0){
          tuition_df = get_details_table(tuition_webPage, "table.tablesaw") %>% map(~set_names(., c("Category", colnames(.)[2:length(colnames(.))]))) %>% bind_rows() %>% select(1:2) %>% 
            mutate(Year = names(tuition_urls[tuition_url]),
                   Program = Program,
                   Program_url = Program_url,
                   Institution = Institution$institution_name)
          
          if(!exists("ProgramFees")){
            ProgramFees <- tuition_df
          } else {
            ProgramFees <- rbind(ProgramFees, tuition_df)
          }
        }
  
      }
      
      if(!exists("Fees")){
        Fees <- ProgramFees
      } else {
        Fees <- rbind(Fees, ProgramFees)
      }
    }
    
    if(!exists("programs")){
      programs = program
    } else {
      programs = programs %>% bind_rows(program)
    }
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
}

save_out_standard_file(programs, Institution$institution_name, "Atlantic")


