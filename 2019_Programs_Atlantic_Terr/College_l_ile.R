

#############################
##### Collège de l'Île
## Type of website:
Institution <- CIConnexName("Collège de l'Île")

webPage <- read_webPage("https://www.collegedelile.ca/fr/futurs-etudiants/programmes-d-etudes") %>% 
  html_nodes(xpath = '//*[@id="main_in_content"]/div[2]/div[3]')

program_urls <- tibble(Program = webPage %>% html_nodes(css = "a") %>% html_text(),
                       url = webPage %>% html_nodes(css = "a") %>% html_attr("href") %>% paste0("https://www.collegedelile.ca",.))


program_urls$WIL <- NA
for(url_row in 1:nrow(program_urls)){
  print(url_row)
  tryCatch({
    remove_old_programs(url)
    
    #for(url in 3){
    #print(program_urls$Program[url])
    webPage = read_webPage(program_urls$url[url_row])

    Duration = get_details_str_split(webPage %>% html_nodes("div.item-page"), searchString = "Durée:|Durée :", splitString = "\r\n", ignoreCase = T)
    Description = get_details_str_split(webPage %>% html_nodes("div.item-page"), searchString = "Durée:|Durée :", splitString = "\r\n", ignoreCase = T, offset = 1)
    Tuition = get_details_str_split(webPage %>% html_nodes("div.item-page"), searchString = "\\*Droits de scolarité :", splitString = "\r\n", ignoreCase = T) %>% paste(., collapse = " AND ")
    
    courses = webPage %>% html_nodes("strong") %>% html_text() %>% str_split("(?<=\\))(?=[A-Z])") %>% unlist()
    courses = courses[grepl("^[A-Z]{3}", courses)]
    courses = tibble(code = courses) %>% 
      separate(code, c("Code", "Name"), sep = " - ")
    print(head(courses))
    
    WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url_row], program_urls$url[url_row], noDescription = T)
    
    program_df = create_program_df(Institution, url = program_urls$url[url_row], Program = program_urls$Program[url_row], Credential = NA, Campus = NA, Duration = Duration, Description = Description, WIL = WIL)
    #print(head(program_df))
    
    Program_tuition = tibble(Program = program_urls$Program[url_row],
                             Program_url = program_urls$url[url_row],
                             Tuition = Tuition)
    
    
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
