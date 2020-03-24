
#############################
##### Collège Nordique Francophone
## Type of website:
Institution <- CIConnexName("Collège Nordique Francophone")


urls <- tibble(Program = c(get_details(read_webPage("https://college-nordique.com/el-introduction/?lang=en") %>% 
                                     html_nodes("label"),
                                   class = "a"),
                       get_details(read_webPage("https://college-nordique.com/ps-introduction/?lang=en") %>% 
                                     html_nodes("label"),
                                   class = "a"),
                       get_details(read_webPage("https://college-nordique.com/pp-introduction/?lang=en") %>% 
                                     html_nodes("label"),
                                   class = "a")),
               url = c(get_details(read_webPage("https://college-nordique.com/el-introduction/?lang=en") %>% 
                                     html_nodes("label"),
                                   class = "a",
                                   urlGrab = T),
                       get_details(read_webPage("https://college-nordique.com/ps-introduction/?lang=en") %>% 
                                     html_nodes("label"),
                                   class = "a",
                                   urlGrab = T),
                       get_details(read_webPage("https://college-nordique.com/pp-introduction/?lang=en") %>% 
                                     html_nodes("label"),
                                   class = "a",
                                   urlGrab = T)),
               index = c(rep.int(1, length(get_details(read_webPage("https://college-nordique.com/el-introduction/?lang=en") %>% 
                                                       html_nodes("label"),
                                                     class = "a"))),
                         rep.int(2, length(get_details(read_webPage("https://college-nordique.com/ps-introduction/?lang=en") %>% 
                                                         html_nodes("label"),
                                                       class = "a"))),
                         rep.int(3, length( get_details(read_webPage("https://college-nordique.com/pp-introduction/?lang=en") %>% 
                                                          html_nodes("label"),
                                                        class = "a")))))

remove(programs)
remove(Fees)
for(url in 1:nrow(urls)){
  Program_url = urls$url[url]
  webPage = read_webPage(Program_url)
  
  Program = if(urls$Program[url] != ""){
    urls$Program[url]
  } else {
    get_details(webPage, "h1")[1]
  }
  print(Program)
  
  if(urls$index[url] %in% c(1, 2)){
    Description = get_details_xpath(webPage, '/html/body/div[5]/div[1]/div[2]/p[2]/text()')
    Description = if(length(Description) <1){
      NA
    }else {
      Description %>% unlist()
    }
    
    Details = get_details_for_filtering(webPage,"div.cours-2colonnes", "\n")
    
    Duration = paste(Details[grepl("duration", Details, ignore.case = T)], collapse = " AND ")
    Tuition = tibble(Program = Program,
                     Program_url = Program_url,
                     Fees = paste(Details[grep("frais", Details, ignore.case = T) + 1], collapse = " AND "))
    Credential = "Other"
    Campus = NA
    WIL = NA
  }
  
  if(urls$index[url] == 2){
    Details_table = get_details_table(webPage %>% html_nodes("div.col-sm-4.no-padding.flexitem-right.section-table"), class = "table") %>% bind_rows()
    Duration = Details_table %>% filter(X1 == "Duration") %>% group_by(X1) %>% summarize(Duration = paste(X2, collapse = " OR ")) %>% ungroup() %>% select(Duration) %>% unlist()
    Campus = Details_table %>% filter(X1 == "Diffusion Mode") %>% group_by(X1) %>% summarize(Campus = paste(X2, collapse = " OR ")) %>% ungroup() %>% select(Campus) %>% unlist()
    Credential = Details_table %>% filter(!X1 %in% c("Duration", "Diffusion Mode")) %>% group_by(X2) %>% summarize(Credential = paste(X1, collapse = " OR ")) %>% ungroup() %>% select(Credential) %>% unlist()
    
    courses = tibble(Code = NA,
                     Name = get_details_id(webPage, id = "content2", class = "span", class_before = F))
    
    Tuition = tibble(Program = Program,
                     Program_url = Program_url,
                     Fees = get_details_id(webPage, id = "content3", class = "span", class_before = F)[grepl("\\$", get_details_id(webPage, id = "content3", class = "span", class_before = F))])
    
    if(nrow(courses > 0)){
      WIL = course_eval(courses, Institution$institution_name, Program, Program_url, noDescription = T)
    }
  }
  
  
  program = create_program_df(Institution, url = Program_url, Program = Program, Credential = Credential, Campus = Campus, Duration = Duration, Description = Description, WIL = WIL)
    

  if(!exists("programs")){
    programs = program
  } else {
    programs = programs %>% bind_rows(program)
  }
  
    if(!exists("Fees")){
      Fees = Tuition
    } else {
      Fees = Fees %>% bind_rows(Tuition)
    }
  
}

save_out_standard_file(programs, Institution$institution_name, "Territories")
