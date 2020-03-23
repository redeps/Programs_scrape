#############################
##### Martime College of Forest tech
## Type of website: very untidy - needs a more manual approach
Institution <- CIConnexName("Maritime College of Forest Technology")

url <- "https://mcft.ca/en/explore/program"

## untidy website
webPage <- read_webPage(url)

programs = create_program_df(Institution = Institution,
                             Program = get_details(webPage, class = "h1"),
                             Campus = "Fredricton AND Bathurst",
                             url = url,
                             Duration = "Two years",
                             Description = webPage %>% html_nodes(xpath = '//*[@id="content"]/p[1]') %>% html_text(),
                             WIL = "10 week practicum") %>% 
  filter(Program != "") %>% 
  bind_rows(create_program_df(Institution = Institution,
                              Program = get_details(webPage, class = "h2"),
                              Campus = "Fredricton AND Bathurst",
                              url = url,
                              Duration = "Two years",
                              Description = webPage %>%  html_nodes(xpath = '//*[@id="content"]/p[7]') %>%   html_text(),
                              WIL = NA)
  )



courses = tibble(Code = NA,
                 Name = get_details(webPage %>% html_nodes("dl.twocolumn"), class = "li"))

course_eval(courses, Institution$institution_name, programs$Program[1], url)

tuition <- read_webPage("https://mcft.ca/en/explore/tuition-and-fees") %>% 
  html_nodes("table.tuitionTable") %>% 
  html_table()

tuition_ad_dip <- tuition[[5]] %>% 
  spread(X1, X2) %>% 
  janitor::clean_names() %>% 
  select(total_fish_wildlife) %>% 
  set_names("Totals") %>% 
  mutate(Program = programs$Program[2],
         url = url) 

Fees <- tuition[1:4] %>% 
  bind_rows() %>% 
  set_names(.[1,]) %>% 
  filter(!Totals %in% c("Totals")) %>% 
  separate(Totals, c("Totals", "Grad_fees"), sep = " \\+ ") %>% 
  filter(Single != "Double") %>% 
  mutate(Semester = as.character(c(rep.int(1, 3), rep.int(2, 3))),
         Res_type = rep(c("Single", "Double", "Single"), 2),
         Program = programs$Program[1],
         url = url) %>% 
  bind_rows(tuition_ad_dip %>% 
              mutate(Semester = "All"))

save_out_standard_file(programs, Institution$institution_name, "Atlantic")