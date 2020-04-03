
#############################
##### Université Sainte-Anne - Collège de l'Acadie
## Type of website:
Institution <- CIConnexName("Université Sainte-Anne - Collège de l'Acadie")



url_df_nodes <- read_webPage("https://www.usainteanne.ca/les-etudes") %>% html_nodes("div.col-xs-12.col-sm-8")
remove(url_df)
for(node in url_df_nodes){
  d = tibble(Program = node %>% html_nodes("a") %>% html_text(),
         Program_url = node %>% html_nodes("a") %>% html_attr("href"),
         Credential = node %>% html_nodes('h2') %>% html_text())
  if(!exists("url_df")){
    url_df = d
  } else {
    url_df = url_df %>% bind_rows(d)
  }
  
}
url_df <- url_df %>% 
  mutate(Program_url = ifelse(!grepl("^http", Program_url), paste0("https://www.usainteanne.ca/", Program_url), Program_url))



for(url in 1:nrow(url_df)){
#for(url in 6:9){
  #Checking to see if programs exists
  remove_old_programs(url)

  tryCatch({
    Program_url = url_df$Program_url[url]
    Program = url_df$Program[url]
    Credential = url_df$Credential[url]
    webPage = read_webPage(Program_url)
    print(paste(url, ":", Program_url))
    
    #Need to avoid duplicates - listing existing program urls to check for duplicates
    program_url_collections = if(exists("programs")) { programs$url} else {"None"} 
    
    #Checking duplicates
    if(!Program_url %in% program_url_collections){
    
      #### specific search for university programs
      if(grepl("^Études universitaires", Credential)){
        credential_url_df = tibble(Program = get_details(webPage %>% html_nodes("div.col-xs-12.col-sm-8"), "a"),
                                   Program_url = paste0("https://www.usainteanne.ca/", get_details(webPage %>% html_nodes("div.col-xs-12.col-sm-8"), "a", urlGrab = T)))
        
        for(credential_url in 1:nrow(credential_url_df)){
          
          Program_url = credential_url_df$Program_url[credential_url]
          Program = credential_url_df$Program[credential_url]
          webPage = read_webPage(Program_url)
          print(paste(credential_url))
          
          #Need to avoid duplicates - listing existing program urls to check for duplicates
          program_url_collections = if(exists("programs")) { programs$url} else {"None"} 
          
            #Checking duplicates
            if(!Program_url %in% program_url_collections){
              details = get_details_table(webPage, "table.table.table-striped") %>% 
                bind_rows()
              
              Duration = get_detail_from_table(details, "X1", "X2", "Durée")
              Campus = get_detail_from_table(details, "X1", "X2", "Campus")
              Credential = get_detail_from_table(details, "X1", "X2", "Diplôme")
              Description = get_details(webPage, "p.lead") %>% paste(., collapse = " AND ")
              
              courses = tibble(code = get_details(webPage %>% html_nodes("div.list-group"), "a"))
              if(nrow(courses) > 0){
                courses = courses %>% 
                  separate(code, c("Code", "Name"), sep = " : ")
                
                WIL = course_eval(courses, Institution$institution_name, Program, Program_url)
                
              } else {
                WIL = NA
              }
              program = create_program_df(Institution, 
                                          url = Program_url, 
                                          Program = Program, 
                                          Credential = Credential,
                                          Campus = Campus,
                                          Duration = Duration,
                                          Description = Description,
                                          WIL = WIL)
              
              
              if(!exists("programs")){
                programs = program
              } else {
                programs = programs %>% bind_rows(program)
              }
              
            }
          
    
        }
        
        #### specific search for college programs
      } else if(grepl("Études collégiales", Credential)){
        
        credential_url_df = tibble(Program = get_details(webPage %>% html_nodes("div.col-xs-12.col-sm-9") %>% html_nodes('h3'), "a"),
                                   Program_url = paste0("https://etudescollegiales.ca", get_details(webPage %>% html_nodes("div.col-xs-12.col-sm-9") %>% html_nodes('h3'), "a", urlGrab = T)))
        
        if(nrow(credential_url_df > 0)){
          for(credential_url in 1:nrow(credential_url_df)){
            Program_url = credential_url_df$Program_url[credential_url]
            Program = credential_url_df$Program[credential_url]
            webPage = read_webPage(Program_url)
            print(paste(credential_url))
            

            
            #Need to avoid duplicates - listing existing program urls to check for duplicates
            program_url_collections = if(exists("programs")) { programs$url} else {"None"} 
            
            #Checking duplicates
            if(!Program_url %in% program_url_collections){
              if(grepl("Leadership en petite enfance", Program)){
                print(Program)
                #print(credential_url)
                print(url)
                print(Program_url)
                print(grepl(Program_url, program_url_collections))
                print(Program_url %in% program_url_collections)
              }
              
              details = get_details_table(webPage, "table.table.table-striped") %>% 
                bind_rows()
              
              Duration = get_detail_from_table(details, "X1", "X2", "Durée")
              Campus = get_detail_from_table(details, "X1", "X2", "Campus")
              Credential = paste("Diplôme", get_detail_from_table(details, "X1", "X2", "Diplôme"))
              if(Credential == "Diplôme "){
                Credential = paste("Certificat", get_detail_from_table(details, "X1", "X2", "Certificat"))
              }
              
              WIL = get_detail_from_table(details, "X1", "X2", "Stage")
              Description = get_details(webPage, "p.lead") %>% paste(., collapse = " AND ")
              
              
              
              courses = webPage %>% html_nodes(xpath = '//*[@id="structure-du-programme"]') %>% html_nodes("table") %>% html_table() %>% bind_rows()
              
              courses = courses[1] %>% set_names("code")
              if(nrow(courses) > 0){
                courses = courses %>% 
                  mutate(code = gsub("([[:digit:]]{2}-[[:digit:]]{2})", "\\1 : ", code)) %>% 
                  separate(code, c("Code", "Name"), sep = " : ")
                
                WIL_option = course_eval(courses, Institution$institution_name, Program, Program_url)
                WIL = paste(WIL, WIL_option, collapse = " AND ")
                
              }
              
              program = create_program_df(Institution, 
                                          url = Program_url, 
                                          Program = Program, 
                                          Credential = Credential,
                                          Campus = Campus,
                                          Duration = Duration,
                                          Description = Description,
                                          WIL = WIL)
              
              
              if(!exists("programs")){
                programs = program
              } else {
                programs = programs %>% bind_rows(program)
              }
            }
        }
        
    
          #### specific search for other programs  
        }
      } else {
          Duration = NA
          Campus = NA
          WIL = NA
          Description = NA
          
          if(!exists("programs")){
            programs = program
          } else {
            programs = programs %>% bind_rows(program)
          }
          
      }
    } else {
      print(paste("Skipped", url))
    }
        
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})

}

programs <- programs %>% 
  distinct()


#rselenium frais calculator

url = "https://www.usainteanne.ca/?option=com_chronoforms6&view=form&Itemid=2188&lang=fr"

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)
remDr$navigate(url)

Sys.sleep(5)
agreement = remDr$findElement(using = 'xpath', '/html/body/div/div[3]/div[2]/div/div/div/form/div/div[2]/div[3]/div/label')
agreement$clickElement()

enter = remDr$findElement(using = 'xpath', '//*[@id="button59"]')
enter$clickElement()
Sys.sleep(5)



remove(Fees)
for(item in 2:40){
#for(item in 2:4){
  print(item)
  tryCatch({
    Sys.sleep(1)
    dropdown = remDr$findElement(using = 'xpath', '/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[2]/div/div/i')
    dropdown$clickElement()
    
    
    
    program_select = remDr$findElement(using = 'xpath', paste0('/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[2]/div/div/div[2]/div[', item, ']'))
    program_select$clickElement()
    Sys.sleep(1)
    program = remDr$findElement(using = 'xpath', '/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[2]/div/div/div[1]')
    
    Program = program$getElementText() %>% unlist()
    
    campus_dropdown = remDr$findElement(using = 'xpath', '/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[4]/div[2]/div/div[7]/div/i')
    campus_dropdown$clickElement()
    
    
    
    
    
    campus_sel = get_element('/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[4]/div[2]/div/div[7]/div/div[2]/div[2]')
    if(length(campus_sel) < 1){
      campus_sel = remDr$findElement(using = 'xpath', '/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[4]/div[2]/div/div[7]/div/div[2]/div')
    }
    campus_sel$clickElement()
    Sys.sleep(1)
    
    
    res_dropdown = remDr$findElement(using = 'xpath', '/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[4]/div[2]/div/div[8]/div')
    res_dropdown$clickElement()
    
    res_sel = remDr$findElement(using = 'xpath', '/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[4]/div[2]/div/div[8]/div/div[2]/div[2]')
    res_sel$clickElement()
    Sys.sleep(1)
    
    if(item == 2) {
      show_more = remDr$findElement(using = 'xpath', '/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[3]/div[1]/div/label')
      show_more$clickElement()
      Sys.sleep(1)
    }


    show_fees = remDr$findElement(using = 'xpath', '//*[@id="button62"]')
    show_fees$clickElement()
    
    
    
    Sys.sleep(1)

    feeWebPage = read_html(remDr$getPageSource()[[1]])
    fees_table = feeWebPage %>% 
      html_nodes(xpath ='/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[3]/div[3]/table[1]') %>% 
      html_table() %>% 
      bind_rows() %>% 
      set_names(letters[1:ncol(.)]) %>% 
      mutate(semester = zoo::na.locf(ifelse(grepl("^Automne|Hiver", a, ignore.case = T), a, NA), na.rm = F)) %>% 
      filter(!is.na(semester)) %>% 
      filter(semester != a) %>% 
      mutate(Program = Program,
             Institution = Institution$institution_name)
    
    if(!exists("Fees")){
      Fees = fees_table
      remove(fees_table)
    } else {
      Fees = Fees %>% bind_rows(fees_table)
      remove(fees_table)
    }
    
    clear = remDr$findElement(using = 'xpath', paste0('/html/body/div/div[3]/div[2]/div/div/div/form/div/div[3]/div[1]/div[2]/div/div/div[2]/div[1]'))
    clear$clickElement()
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

#Find program in programs df

remDr$close()


save_out_standard_file(programs, Institution$institution_name, "Atlantic")
