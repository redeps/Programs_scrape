
#############################
##### Nunavut Arctic College
## Type of website:
Institution <- CIConnexName("Nunavut Arctic College")

url = "https://arcticcollege.ca/programs"

webPage = read_webPage(url) 
webPage %>% html_nodes("a")
url_df = tibble(url = webPage %>% html_nodes("div.intrinsic") %>% html_nodes('a') %>% html_attr('href') %>% paste0("https://arcticcollege.ca", .))

for(url in 1:nrow(url_df)){
  remove_old_programs(url)
  print(url)
  
  tryCatch({
    url = url_df$url[url]
    webPage = read_webPage(url)
    program_url_list = get_details_index(webPage, searchString = "Download course outline for this program", class_1 = "div.sqs-block-button-container--center", class_2 = "div.sqs-block-button-container--center", class_3 = "a", urlGrab = T)
    Program_list = webPage %>% html_nodes("h2") %>% html_text()

    program = tibble(Program = Program_list) %>% 
      mutate(Program = gsub("^[[:space:]]*", "", gsub("\n", "", Program)),
             #Using 4 first capital letters for matching with url
             Program_match = gsub("([A-Z]{4})(.*)", "\\1", gsub("[^A-Z]", "", gsub(" to | of | for", "", Program, ignore.case = T))))

    program_url = tibble(program_url = program_url_list,
                         #Using 4 first capital letters for matching with url
                         Program_match = gsub("([A-Z]{4})(.*)", "\\1", gsub("[^A-Z]", "", gsub("/s/[[:digit:]]*-*", "", program_url))))

    
    program = program %>% left_join(program_url) %>% 
      select(-Program_match)
    
    print(paste("Captured", length(program$program_url[!is.na(program$program_url)]), "urls from a total of", length(program_url_list)))
      
   if(!exists("programs")){
      programs = program
    } else {
      programs = programs %>% bind_rows(program)
    }
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}


programs$program_url = paste0("https://arcticcollege.ca", programs$program_url)

for(url in 1:nrow(programs)){
  print(url)
  
  tryCatch({
    url = programs$program_url[url]
    webPage = read_webPage(url)
    
    #url = "https://static1.squarespace.com/static/5b1954d75cfd798b94327249/t/5b479310f950b7444aaeb1b6/1531417364166/031+Management+Studies.pdf"
    #url = "https://static1.squarespace.com/static/5b1954d75cfd798b94327249/t/5b4649a11ae6cffb586868f5/1531333054653/Bachelor+Of+Science+In+Nursing+%28BScN%29+%28Arctic+Nursing%29.pdf"
    raw_text = pdftools::pdf_text(url)
    raw_text = raw_text %>% 
      str_split("\n") %>% 
      unlist()
    
    #if a certain structure: 
    if(!grepl("Program Number", head(raw_text))){
      Credential = raw_text[grep("Credential", raw_text, ignore.case = T) + 1]
      Duration = raw_text[grep("Length of Program", raw_text) + 1]
      Duration = Duration[grep("semester", Duration, ignore.case = T)]
      Campus = raw_text[grep("institution", raw_text, ignore.case = T) + 1]
      Description = raw_text[grep("^Description", raw_text, ignore.case = T) + 1:10] %>% gsub(" {2,}", "", .)
      Description = paste(Description[nchar(Description) > 30], collapse = " ")
    } else {
      
    }


    
    
     
    
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

html_nodes("a") %>% html_attr("href")
webPage %>% html_nodes("h2") %>% html_text()


#country <- c("lux", "fra", "deu", "usa", "prt", "gbr")
#url <- "http://www.who.int/diabetes/country-profiles/{country}_en.pdf?ua=1"
#(urls <- glue(url))
