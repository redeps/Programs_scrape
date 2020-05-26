
#############################
##### Holland College
## Type of website:
Institution <- CIConnexName("Holland College")

webPage <- read_webPage("https://www.hollandcollege.com/programs/index.html#programList1")
program_urls <- tibble(Program = webPage %>% html_nodes("div.small-12") %>% html_nodes("a") %>% html_text(),
                       url = webPage %>% html_nodes("div.small-12") %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://www.hollandcollege.com/programs/", .)) %>% 
  filter(!grepl("apply now|contact us|make a gift", Program, ignore.case = T)) %>% 
  filter(!grepl("about", url)) %>% 
  mutate(url = gsub("programs//programs", "programs", url),
         url = gsub("https://www.hollandcollege.com/programs/https://www.hollandcollege.com/programs", "https://www.hollandcollege.com/programs", url))



program_urls$WIL <- NA
for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    detail_table = tibble(detail = get_details(webPage %>% html_nodes("div.small-12"), class = "span")) %>% 
      mutate(index = rep(1:2, nrow(.)/2),
             id = rep(1:(nrow(.)/2), each = 2)) %>% 
      pivot_wider(names_from = index, id_cols = id, values_from = detail) %>% 
      set_names(c("id", "detail_name", "detail")) %>% 
      filter(detail_name != "")
    
    Duration = detail_table$detail[grepl("Length", detail_table$detail_name, ignore.case = T)] %>% unlist() %>% paste(., collapse = " AND ")
    Credential = detail_table$detail[grepl("Credential", detail_table$detail_name, ignore.case = T)] %>% unlist() %>% paste(., collapse = " AND ")
    Campus = detail_table$detail[grepl("Location", detail_table$detail_name, ignore.case = T)] %>% unlist() %>% paste(., collapse = " AND ")
    
    Description = get_details(webPage %>% html_nodes("div.container"), class = "p") %>% paste(collapse = " ")
    #Intl_tuition = webPage %>% html_nodes("div.resp-tab-content") %>% html_nodes("strong") %>%  html_text()
    Program_tuition = get_details_table(webPage, "table.table") %>% 
      bind_rows() 
    
    if(ncol(Program_tuition) > 0){
      Program_tuition = Program_tuition %>% 
      pivot_longer(cols = 2:ncol(.)) %>% 
      mutate(Program = program_urls$Program[url],
             Program_url = program_urls$url[url]) %>% 
      select(Program, Program_url, 1:3)
    }
    
    course_url = get_details(webPage %>% html_nodes("div.container") %>% html_nodes("p"), "a", urlGrab = T)
    course_url = course_url[grepl("course-details-list", course_url)] %>% paste0("https://www.hollandcollege.com", .)
    courseWebPage = read_webPage(course_url[1])
    
    if(length(courseWebPage) > 0){
      course_names = courseWebPage %>% html_nodes("div.container") %>% html_nodes("div.row") %>% html_nodes("h2") %>% html_text()
      course_details = courseWebPage %>% html_nodes("div.container") %>% html_nodes("div.row") %>% html_nodes("div") %>% html_text()
      
      if(length(course_names) == length(course_details) +1) {
        courses = tibble(Code = course_details,
                         Name = course_names[2:length(course_names)]) %>% 
          mutate(Code = gsub("^\n", "", Code)) %>% 
          separate(Code, c("Description", "Duration", "Code"), sep = "\n|Code: ")
        
        WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      } else {
        WIL = NA
      }
    } else {
      WIL = NA
    }
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = Campus, Duration = Duration, Description = Description, WIL = WIL)
    #print(head(program_df))
    

    if(!exists("programs")){
      programs = program_df
    } else {
      programs = programs %>% bind_rows(program_df)
    }
    
    if(!exists("Fees")){
      Fees = Program_tuition
    } else {
      Fees = Fees %>% bind_rows(Program_tuition)
    }
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

save_out_standard_file(programs, Institution$institution_name, "Atlantic")
