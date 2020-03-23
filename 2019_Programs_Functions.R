##### Declaring Functions ########
strToDf <- function(string, columnsToProduce = 2, columnNames = c("Code", "Name")){
  stringDf = tibble(Values = string) %>% 
    mutate(Categories = rep(1:columnsToProduce, (nrow(.)/columnsToProduce))) %>% 
    mutate(Groupings = rep(1:(nrow(.)/columnsToProduce), each = columnsToProduce)) %>% 
    spread(Categories, Values ) %>% 
    select(-Groupings) %>% 
    set_names(columnNames)
  
  return(stringDf)
}

CIConnexName <- function(x){
  return(
    colleges %>% 
      filter(grepl(x, institution_name))
  )
}


### scraping helper functions

get_urls <- function(class, url){
  
  read_html(url) %>% 
    html_nodes(class) %>% 
    html_nodes("a") %>% 
    html_attr("href")
}


possibleError <- function(url){
  
  Error = tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  return(Error)
}

possibleSelError <- function(remDr, xpath){
  
  Error = tryCatch(
    remDr$findElement(using = "xpath", xpath),
    error = function(e) e
  )
  
  return(Error)
}

clean_tags <- function(stringVar){
  return(gsub("^[[:blank:]]+", "", gsub("[[:blank:]]{2,}", " ", gsub("\r|\n|\t", "", stringVar))))
}

# key workhorse - reads in html structure
read_webPage <- function(url){
  
  if(!inherits(possibleError(url), "error")){
    webPage = read_html(url)
    return(webPage)
  }
}

# key workhorse - gets text from specific class in html structure
get_details <- function(webPage, class, urlGrab = F){
  
  if(urlGrab == F){
    detail = webPage %>% 
      html_nodes(class) %>% 
      html_text()
    
    detail = sapply(detail, function(x) clean_tags(x))
  } else {
    detail = webPage %>% 
      html_nodes(class) %>% 
      html_attr("href")
  }
  
  return(detail)
  
}

# key workhorse - gets table from specific class in html structure
get_details_table <- function(webPage, class = NA, xpath = NA){
  if(is.na(xpath)){
    detail = webPage %>% 
      html_nodes(class) %>% 
      html_table(fill = T)
    return(detail)
  } else if(is.na(class)) {
    detail = webPage %>% 
      html_nodes(xpath = xpath) %>% 
      html_table(fill = T)
    return(detail)
  }
}

# key workhorse - gets table from specific class in html structure
get_details_table_xpath <- function(webPage, xpath){
  detail = webPage %>% 
    html_nodes(xpath = xpath) %>% 
    html_table(fill = T)
  return(detail)
}

get_details_id <- function(webPage, id, class = NA){
  if(is.na(class)){
    detail = webPage %>% 
      html_nodes(xpath = paste0('//*[@id="', id, '"]')) %>% 
      html_text()
  } else {
    detail = webPage %>% 
      html_nodes(class) %>% 
      html_nodes(xpath = paste0('//*[@id="', id, '"]')) %>% 
      html_text()
    
  }
  
  detail = sapply(detail, function(x) clean_tags(x))
  
  return(detail)
}

## using regex search through first set of texts to find a specific node within nodes
# Use only if there is no other tag to go by
# add 3rd class if you want to go one step deeper
get_details_index <- function(webPage, searchString, class_1, class_2, class_3 = NA, urlGrab = F){
  index = webPage %>% 
    html_nodes(class_1) %>% 
    html_text()
  
  index = grep(searchString, index, ignore.case = T)
  
  detail = webPage %>% 
    html_nodes(class_2)
  
  if(length(index) > 0){
    if(urlGrab == F) {
      if(is.na(class_3)){
        detail = detail[index] %>% 
          html_text()
        
        detail = sapply(detail, function(x) clean_tags(x))
        return(detail)
      } else {
        detail = detail[index] %>% 
          html_nodes(class_3) %>% 
          html_text()
        
        detail = sapply(detail, function(x) clean_tags(x))
        return(detail)
      }
    }
    
    if(urlGrab == T) {
      if(is.na(class_3)){
        detail = detail[index] %>% 
          html_attr("href")
        
        return(detail)
      } else {
        detail = detail[index] %>% 
          html_nodes(class_3) %>% 
          html_attr("href")
        
        return(detail)
      }
    }
    
  }
  
  return(NA)
}

#Function for finding button to click, then finding subsequently revealed data using RSelenium variable
find_and_click <- function(remDr, clickPath, dataPath, pause = .75){
  
  
  if(!inherits(possibleSelError(remDr, clicPath), "error")){
    webElem <- remDr$findElement(using = "xpath", clickPath)
    Sys.sleep(pause)
    webElem$clickElement()
    Sys.sleep(pause)
    
    if(!inherits(possibleSelError(remDr, dataPath), "error")){
      errorCheck = possibleSelError(remDr, dataPath)
      returnData <- remDr$findElement(using = "xpath", dataPath)
      return(returnData)
    } else {
      return(NA)
    }
    
  }else {
    return(NA)
  }
  
}

# key workhorse - combines class names and class content (if structured differently)
combine_details <- function(webPage, class_names, class_content){
  names = get_details(webPage, class_names)
  content = get_details(webPage, class_content)
  
  detailDf = tibble(names = names,
                    content = content) %>% 
    spread(names, content)
  
  return(detailDf)
}


create_program_df <- function(Institution, url = NA, Program = NA, Credential = NA, Campus = NA, Duration = NA, Description = NA, WIL = NA){
  program = Institution %>% 
    left_join(tibble(institution_name = Institution$institution_name,
                     url = url, 
                     Program = Program, 
                     Credential = Credential, 
                     Campus = Campus, 
                     Duration = Duration, 
                     Description = Description, 
                     WIL = WIL),
              by = "institution_name")
  
  return(program)
}

get_courses <- function(url, isTable = T, tableContClass){
  if(!is.na(url)){
    #reading in web page structure
    webPage = read_webPage(url)
    
    #checking to see if web structure includes the table tag
    includesTable = webPage %>% grepl("<table", .)
    #print(includesTable)
    if(isTable == T & includesTable == T){
      courses = webPage %>% 
        html_nodes(tableContClass) %>%
        html_nodes("table") %>% 
        html_table(fill = T) %>% 
        bind_rows() %>% 
        as_tibble() 
      
      #Returning data frame (tibble)
      return(courses)
    } else {
      courses = tibble()
    }
  } else {
    courses = tibble()
  }
}

clean_string <- function(stringVar){
  return(gsub(" |/", "_", gsub('\\"|\n|\\(|\\)|&|-', "", stringVar)))
}


##### This is the shakiest funciton - getting course data
course_eval <- function(courses, Institution, Program, Program_url, noDescription = T){
  
  if(noDescription == T){
    course_df = courses %>% 
      mutate(Institution = Institution,
             Program = Program,
             Program_url = Program_url,
             Description = NA) %>% 
      select(Institution, Program, Program_url, Code, Name, Description)
  } else {
    course_df = courses %>% 
      mutate(Institution = Institution,
             Program = Program,
             Program_url = Program_url) %>% 
      select(Institution, Program, Program_url, Code, Name, Description)
  }
  

  
  dir.create(paste0("courses/", clean_string(Institution)), showWarnings = FALSE)
  progFileName <- gsub(" |/|_2,}", "_", gsub('\\"|\n|\\(|\\)|&|-', "", Program))
  if(nchar(progFileName) > 50){
    progFileName <- substr(progFileName,1,50)
  }
  write.csv(course_df, paste0("courses/", clean_string(Institution), "/", progFileName, ".csv"))
  WIL <- course_df$Name[grepl("Pratique|Practicum|Field placement|Placement|Work experience|Co-op|coop|Apprentice|internship|field practice|clinical practice|clinical work|work term| stage ", course_df$Name, ignore.case = T)]
  
  if(length(WIL) > 0){
    #print(paste(WIL, collapse = " AND "))
    return(paste(WIL, collapse = " AND "))
  } else {
    return(NA)
  }
}



save_out_standard_file <- function(programs, Institution, region){
  region_check = grepl(region, paste(list.dirs("programs"), collapse = ","))
  if(region_check == F){
    dir.create(paste0("programs/", region))
    dir.create(paste0("programs/", region, "/Archive"))
  }
  
  programs = programs %>% 
    select(institution_name, prov_terr, record_id, member, url, Program, Credential, Campus, Duration, Description, WIL)
  
  old_files = list.files(paste0("programs/",region))
  old_files = if(length(old_files) > 1) old_files[grepl(clean_string(Institution), old_files)]
  
  if(length(old_files) > 0){
    file.copy(paste0("programs/",region, "/",old_files), paste0("programs/",region, "/Archive/",old_files))
    file.remove(paste0("programs/",region, "/",old_files))
  }
  
  write_rds(programs, paste0("programs/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_programs.rds"))
  write_csv(programs, paste0("programs/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_programs.csv"), na = "")
  if(exists("Fees")){
    write_rds(programs, paste0("programs/",region, "/", clean_string(Institution),"_", Sys.Date(),  "_fees.rds"))
    
  }
}

### End data handling helper functions
