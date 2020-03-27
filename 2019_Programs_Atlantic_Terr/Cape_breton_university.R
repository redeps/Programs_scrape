
#############################
##### Cape Breton University
## Type of website:
Institution <- CIConnexName("Cape Breton University")

url_df <- tibble(Program = get_details(read_webPage("https://www.cbu.ca/academics/programs/") %>% html_nodes("article.program-result"), "h1"),
                 Credential = get_details(read_webPage("https://www.cbu.ca/academics/programs/") %>% html_nodes("article.program-result"), "div.column.is-one-third.mobile-hide"),
                 Program_url = get_details(read_webPage("https://www.cbu.ca/academics/programs/") %>% html_nodes("div.program-content"), "a", urlGrab = T)[!grepl("apply-now", get_details(read_webPage("https://www.cbu.ca/academics/programs/") %>% html_nodes("div.program-content"), "a", urlGrab = T))],
                 Description = get_details(read_webPage("https://www.cbu.ca/academics/programs/") %>% html_nodes("article.program-result"), "p"))

Tuition = tibble(Tuition = read_html("https://www.cbu.ca/future-students/tuition-fees/") %>% 
  html_nodes("div.column.content.main-copy-block.is-two-thirds") %>% 
  html_nodes("p") %>% 
  html_text()) %>% 
  filter(grepl("^Tuition:|^Total:|^Fees:|^For a", Tuition, ignore.case = T)) %>% 
  separate(Tuition, c("Category", "Tuition"), sep = ":") %>% 
  mutate(Student_cat = zoo::na.locf(ifelse(Tuition == "", Category, NA)),
         index = rep(1:3, each = 4)) %>% 
  filter(Student_cat != Category) %>% 
  spread(Category, Tuition) %>% 
  select(-index) %>% 
  mutate(timePeriod = "per academic year (30 credits)")

remove(Fees)
remove(programs)
for(url in 1:nrow(url_df)){
  tryCatch({
  Program = url_df$Program[url]
  Credential = url_df$Credential[url]
  Program_url = url_df$Program_url[url]
  Description = url_df$Description[url]
  webPage = read_webPage(Program_url)
  print(Program)
  
  Details = tibble(category = get_details(webPage %>% html_nodes("div.call-out"), "small"),
                   value = get_details(webPage %>% html_nodes("div.call-out"), "span")[!grepl("This area of study is offered", get_details(webPage %>% html_nodes("div.call-out"), "span"))])

  Duration = Details %>% filter(grepl("duration", category, ignore.case = T)) %>% group_by(category) %>% summarize(value = paste(value, collapse = " OR ")) %>% ungroup() %>% select(value) %>% unlist()
  if(length(Duration) < 1){
    Duration = NA
  }
  WIL = paste(get_details_index(webPage %>% html_nodes("div.column.content.main-copy-block"), searchString = "Experiential Learning Opportunities", class_1 = "h2", class_2 = "ul", class_3 = "li", index_offset = -1, use_last_position = T), collapse = " AND ")
  
  courses = tibble(Name = get_details_index(webPage %>% html_nodes("div.column.content.main-copy-block"), searchString = "Sample Courses", class_1 = "h2", class_2 = "ul", class_3 = "li", index_offset = -1)) %>% 
    separate(Name, c("Code", "Name"), sep = ":") %>% 
    course_eval(., Institution$institution_name, Program, Program_url, noDescription = T)
  
  program_df = create_program_df(Institution, url = url_df$Program_url[url], Program = url_df$Program[url], Credential = url_df$Credential[url], Campus = NA, Duration = Duration, Description = url_df$Description[url], WIL = WIL)
  #print(head(program_df))
  
  Program_tuition = tibble(Program = rep(Program, 3),
                   Program_url = rep(Program_url, 3)) %>% 
    bind_cols(Tuition)
    
    
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
