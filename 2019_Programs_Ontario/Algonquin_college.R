#############################
##### Algonquin
## Type of website:


Institution <- CIConnexName("Algonquin")

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
#remDr$open(silent = T)
remDr$navigate("https://www.algonquincollege.com/future-students/programs/")
prgTable = remDr$findElement(using = 'xpath', '//*[@id="DataTables_Table_0"]')

doc = htmlParse(remDr$getPageSource()[[1]])
responseTable = readHTMLTable(doc)[[3]]
colnames(responseTable)[1] <- "Selection"
program_urls = responseTable %>% 
  as_tibble() %>% 
  filter(Selection != "") %>% 
  filter(!is.na(`Program Name`)) %>% 
  rename(Program = `Program Name`,
         Duration = Length)

urls = remDr$findElements(using = 'xpath', '//*[@id="DataTables_Table_0"]/tbody/tr/td/a')
program_urls$url = unlist(lapply(urls, function(x) {x$getElementAttribute('href')}))
program_urls$Program = gsub(" Duration.*$", "", program_urls$Program)
remDr$close()

program_urls$WIL <- NA
remove(details)
for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    Description = webPage %>% html_nodes("div.col-sm-8") %>% html_nodes("p") %>% html_text() %>% paste(., collapse = " ")
    
    courses = webPage %>% html_nodes("div.container.course-container") %>% html_nodes("div.row") %>% html_text() %>% gsub("\\t{1,}|\\r{1,}|\\n{1,}", "|", .) %>% gsub("\\|{1,}", "|", .)
    if(length(courses) > 0){
      courses = tibble(Code = courses %>% gsub("^\\||\\|$", "", .)) %>% 
        separate(Code, c("Code", "Name", "Description", "Hours"), sep = "\\|")
      
      #clean_string(program_urls$Program[url])
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
    } else {
      WIL = NA
    }
    
    Details = webPage %>% html_nodes("ul.list-no-left") %>% html_nodes("li")
    ACProgramCode = Details[grepl("Program Code", Details, ignore.case = T)]
    ACProgramCode = ACProgramCode %>% html_nodes("span") %>% html_text() %>% gsub("^[[:space:]]|[[:space:]]$", "", .)
    
    tryCatch({
      if(is.na(WIL)){
        WIL = Details[grep("Work Integrated", Details, ignore.case = T):length(Details)] %>% html_nodes("span") %>% html_text() %>% paste(., collapse = " AND ")
      }
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    
    
     tryCatch({
      remove("Program_tuition")
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
    feeUrl = paste0("https://www.algonquincollege.com/ro/pay/fee-estimator/?campus=ALL&startterm=2019F&residency=canadian&programFees=", ACProgramCode ,"#programDetail")
    tryCatch({
      website <- read_html(feeUrl)
      progUrl <- website %>% 
        html_nodes(xpath = '//*[@id="feeEstimator"]/div[2]/div[1]/h2/a') %>% 
        html_attr("href")
      d <- tibble(feeTitle = website %>% 
                    html_nodes("span.feeTitle") %>% 
                    html_text(),
                  feeValue = website %>% 
                    html_nodes("span.feeValue") %>% 
                    html_text(),
                  Program = program_urls$Program[url],
                  url = feeUrl,
                  ScrapeProgram = website %>% 
                    html_nodes("div.feeProgramTitle") %>% 
                    html_text(),
                  progUrl = program_urls$url[url],
                  studentType = "Canadian")
      if(!exists("Program_tuition")){
        Program_tuition <- d
      } else {
        Program_tuition <- rbind(Program_tuition, d)
      }
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    
    feeUrl = paste0("https://www.algonquincollege.com/ro/pay/fee-estimator/?campus=ALL&startterm=2019F&residency=international&programFees=", ACProgramCode ,"#programDetail")
    tryCatch({
      website <- read_html(feeUrl)
      progUrl <- website %>% 
        html_nodes(xpath = '//*[@id="feeEstimator"]/div[2]/div[1]/h2/a') %>% 
        html_attr("href")
      d <- tibble(feeTitle = website %>% 
                    html_nodes("span.feeTitle") %>% 
                    html_text(),
                  feeValue = website %>% 
                    html_nodes("span.feeValue") %>% 
                    html_text(),
                  Program = program_urls$Program[url],
                  url = feeUrl,
                  ScrapeProgram = website %>% 
                    html_nodes("div.feeProgramTitle") %>% 
                    html_text(),
                  progUrl = program_urls$url[url],
                  studentType = "International")
      if(!exists("Program_tuition")){
        Program_tuition <- d
      } else {
        Program_tuition <- rbind(Program_tuition, d)
      }
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = Description, WIL = WIL)
    
    
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





save_out_standard_file(programs, Institution$institution_name, "Ontario")
