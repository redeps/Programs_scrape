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
