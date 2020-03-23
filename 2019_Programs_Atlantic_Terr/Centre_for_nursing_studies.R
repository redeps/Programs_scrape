
#############################
##### Centre for Nursing Studies
## Type of website: Very unstructured, basically a manual entry
Institution <- CIConnexName("Centre for Nursing Studies")

webPage = read_webPage("http://www.cns.nf.ca/programs")
program_urls = tibble(url = get_details_index(webPage %>% html_nodes("div.menu"), class_1 = "li", class_2 = "li", class_3 = "a", searchString = "program", urlGrab = T)) %>% 
  filter(url != "../programs/") %>% 
  mutate(url = gsub("^\\.{2}", "http://www.cns.nf.ca", url))

remove(programs)
remove(Fees)
remove(fees)
#for(url in 1:1){
for(url in 1:nrow(program_urls)){
  print(url)
  webPage = read_webPage(program_urls$url[url])
  
  content = webPage %>% 
    html_nodes("div.contentContainer") %>% 
    html_nodes("p")
  
  program = create_program_df(Institution = Institution,
                              Program = get_details(webPage, "h2"),
                              Campus = "Memorial University",
                              url = program_urls$url[url],
                              Description = content[1] %>% html_text())
  
  
  if(exists("programs")){
    programs = programs %>% 
      bind_rows(program)
  } else {
    programs = program
  }
}

programs <- programs %>% 
  mutate(Credential = case_when(grepl("bachelor", Program, ignore.case = T) ~ "Bachelor degree",
                                grepl("practical nursing", Program, ignore.case = T) ~ "Diploma",
                                grepl("continuing nursing", Program, ignore.case = T) ~ "Other"),
         Duration = case_when(grepl("bachelor", Program, ignore.case = T) ~ "4 years",
                              grepl("practical nursing", Program, ignore.case = T) ~ "16 months",
                              grepl("continuing nursing", Program, ignore.case = T) ~ ""))


save_out_standard_file(programs, Institution$institution_name, "Atlantic")

