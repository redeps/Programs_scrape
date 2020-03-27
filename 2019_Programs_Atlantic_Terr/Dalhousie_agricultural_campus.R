

#############################
##### Dalhousie Agricultural Campus of Dalhousie University
## Type of website:
Institution <- CIConnexName("Dalhousie Agricultural Campus of Dalhousie University")

url_df <- tibble(Program_url = paste0("https://www.dal.ca", read_webPage("https://www.dal.ca/faculty/agriculture/programs.html") %>% html_nodes("div.text.parbase.section") %>% html_nodes('a') %>% html_attr("href")),
                 Program = read_webPage("https://www.dal.ca/faculty/agriculture/programs.html") %>% html_nodes("div.text.parbase.section") %>% html_nodes('a') %>% html_text()) %>% 
  filter(Program != "View the programs")

remove(programs)
remove(Fees)
for(url in 1:nrow(url_df)){
  Program_url = url_df$Program_url[url] %>% gsub("https://www.dal.cahttps://", "https://", .)
  Program = url_df$Program[url]
  print(Program_url)
  webPage = read_webPage(Program_url)
  
  detail = get_details(webPage %>% html_nodes("div.aaPlainText.parbase.section"), class = "div") %>% gsub("([a-z])([A-Z])", "\\1||\\2", .) %>% strsplit("\\|\\|") %>% unlist()
  if(length(detail > 0)){
    details = tibble(detail = detail) %>% 
      separate(detail, c("Category", "Value"), sep = ":")
    
    Duration = details %>% filter(grepl("Duration|length", Category, ignore.case = T)) %>% group_by(Category) %>% summarize(Value = paste(Value, collapse = " AND ")) %>% select(Value) %>% unlist()
    Campus = details %>% filter(grepl("campus", Category, ignore.case = T)) %>% group_by(Category) %>% summarize(Value = paste(Value, collapse = " AND ")) %>% select(Value) %>% unlist()
    Credential = details[2,1] %>% unlist()
  } else {
    Duration = NA
    Campus = NA
    Credential = NA
  }

  

  program_df = create_program_df(Institution, url = Program_url, Program = Program, Credential = Credential, Campus = Campus, Duration = Duration, WIL = NA, )
  
  if(!exists("programs")){
    programs = program_df
  } else {
    programs = programs %>% bind_rows(program_df)
  }
}


save_out_standard_file(programs, Institution$institution_name, "Atlantic")
