
#############################
##### Aurora College
## Type of website:
Institution <- CIConnexName("Aurora College")
webPage <- read_webPage("http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsAtoZ.aspx")
program_urls <- tibble(Program = get_details(webPage %>% html_nodes("td.prg-list-text"), "a"),
                       url = paste0("http://www.auroracollege.nt.ca", get_details(webPage %>% html_nodes("td.prg-list-text"), "a", urlGrab = T)))


#function to get data using separate urls - doing this to separate credentials.
#http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsByCredentialType.aspx
url_combination <- function(url, xpath_id, credential){
  returnData = read_webPage(url)
  return(tibble(Program = get_details(returnData %>% html_nodes(xpath = paste0('//*[@id="', xpath_id, '"]/table')), "a"),
                url = paste0("www.auroracollege.nt.ca", get_details(returnData %>% html_nodes(xpath = paste0('//*[@id="', xpath_id, '"]/table')), "a", urlGrab = T)))) %>% 
    filter(grepl("[[:alpha:]]", Program)) %>% 
    mutate(Credential = credential)
}

url_df <- tibble(url = c("http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsDiploma.aspx", 
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsDegree.aspx",
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsApprenticeshipTrades.aspx",
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsOther.aspx",
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsAcademicUpgradingAndAccess.aspx",
                         "http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsCertificate.aspx"
),
xpath_id = c("CPH2_WPM_wp481640216_wp1279118871_divControlHolder",
             "CPH2_WPM_wp375978756_wp1611025563_divControlHolder",
             "CPH2_WPM_wp1049890377_wp1621563132_divControlHolder",
             "CPH2_WPM_wp1056227953_wp1808783534_divControlHolder",
             "CPH2_WPM_wp950191435_wp121123461_divControlHolder",
             "CPH2_WPM_wp1288918235_wp1396638403_divControlHolder"
),
credential = c("Diploma",
               "Bachelor",
               "Apprenticeship certificate",
               "Other",
               "Other",
               "Certificate"))

remove(program_urls)
for(i in 1:nrow(url_df)){
  credential_df = url_combination(url_df$url[i], url_df$xpath_id[i], url_df$credential[i])
  if(!exists("program_urls")){
    program_urls = credential_df
  } else {
    program_urls = program_urls %>% bind_rows(credential_df)
  }
}

url_combination(url_df$url[1], url_df$xpath_id[1], url_df$credential[1])

#### integrate data on campuses from http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsByLocation.aspx

Campus_details <- read_webPage("http://www.auroracollege.nt.ca/_live/pages/wpPages/ProgramsByLocation.aspx")

Campus_df <- get_details_table(Campus_details, xpath = '//*[@id="CPH2_WPM_wp752555833_wp1158879749_divControlHolder"]/table[2]') %>% 
  bind_rows() %>% 
  filter(X1 != "" & !grepl("Program", X1, ignore.case = T)) %>% 
  mutate(Campus = zoo::na.locf(ifelse(!grepl("^[[:digit:]]", X1), X1, NA))) %>% 
  filter(X1 != Campus) %>% 
  select(-X1) %>% 
  set_names("Program", "Campus") %>% 
  group_by(Program) %>% 
  summarize(Campus = paste(Campus, collapse = " AND "))


#Need to remove duplicates
program_urls <- program_urls %>% 
  left_join(Campus_df, by = "Program")


program_urls$WIL <- NA
for(url in 1:nrow(program_urls)){
  tryCatc({
#for(url in 3){
  #print(program_urls$Program[url])
  webPage = read_html(paste0("http://", program_urls$url[url]))
  courses = get_details_table(webPage, xpath = '//*[@id="CPH2_WPM_wp868700587_wp801226763_divControlHolder"]/table') %>% 
    bind_rows() %>% 
    filter(grepl("^[[:digit:]]{3}-[[:digit:]]{3}", X2)) %>% 
    select(X2,X4) %>% 
    set_names(c("Code", "Name")) %>% 
    filter(Name != "")
  
  #print(courses)
  
  course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url])
  program_urls$WIL[url] = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url])
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}
programs <- program_urls %>% 
  mutate(institution_name = Institution$institution_name,
         Duration = NA,
         Description = NA,
         prov_terr = Institution$prov_terr,
         record_id = Institution$record_id,
         member = Institution$member)



save_out_standard_file(programs, Institution = Institution$institution_name, "Territories")
