#############################
##### Yukon University
## Type of website:


Institution <- CIConnexName("Yukon")

webPage <- read_webPage("https://www.yukonu.ca/programs/all")

degree_sub <- function(credential){
  return(gsub("NC", "Notice of Completion", credential))
}

program_urls <- webPage %>% 
  html_nodes("table.views-table") %>% 
  html_table() %>% 
  bind_rows() %>% 
  mutate(url = webPage %>% html_nodes("td.views-field") %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://www.yukonu.ca", .)) %>% 
  set_names(c("Program", "Campus", "Credential", "url")) %>% 
  mutate(Credential = gsub("^C|[[:space:]]C[[:space:]]|C$", "Certificate", 
                           gsub("De", "Degree", 
                                gsub("Di", "Diploma",
                                     gsub("NC", "Notice of Completion",
                                          gsub("G", "Graduate", Credential))))))
           


program_urls$WIL <- NA
remove(details)
for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    detail_names = webPage %>% html_nodes("div.field.field--name-title") %>% html_text()
    details = webPage %>% html_nodes("div.clearfix.text-formatted.field.field--name-field-body.field--type-text-long.field--label-hidden") %>% html_text()
    details = details[3:length(details)]
    
    if(length(details) == length(detail_names)){
      details = tibble(name = detail_names,
                       detail = details)
      
      Description = details$detail[grepl("description", details$name)] %>% paste(collapse = " ")
    } else {
      Description = NA
    }
    

    
    details = webPage %>% html_nodes("div.shaded") %>% html_text()
    details = tibble(detail = details %>% str_split("\n") %>% unlist()) %>% 
      mutate(category = zoo::na.locf(ifelse(grepl("program length|start|delivery", detail, ignore.case = T), detail, NA), na.rm = F)) %>% 
      filter(category != detail) %>% 
      filter(detail != "")
    
    Duration = details$detail[grepl("program length", details$category, ignore.case = T)] %>% paste(collapse = " AND ")
    
    
    courses = webPage %>% html_nodes("div.view.view-courses") %>% html_nodes("div.table-responsive") %>% html_nodes("table.views-table") %>% html_table()
    
    if(length(courses) > 1){
      courses = courses %>% 
        map(~ set_names(., letters[1:ncol(.)])) %>% 
        map(~ mutate_all(., as.character)) %>% 
        bind_rows() %>% 
        select(a) %>% 
        separate(a, c("Code", "Name"), sep = " - ")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
    } else {
      WIL = NA
    }
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = Duration, Description = Description, WIL = WIL)
    
    
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

save_out_standard_file(programs, Institution$institution_name, "Territories")
