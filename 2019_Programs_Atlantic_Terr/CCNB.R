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
