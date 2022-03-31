# Goal: scrape Wikipedia infoboxes for each fire to get
# 1. Start / end dates
# 2. Number of acres 

library(googlesheets4)
library(httr)
library(xml2)
library(tidyverse)
library(rvest)

firenames <- c("August Complex", "Dixie", "Mendocino Complex", "SCU Lightning Complex", 
               "Creek", "LNU Lightning Complex", "North Complex", "Thomas", 
               "Rush", "Rim", "Zaca", "Carr", "Monument", "Caldor", "River Complex", 
               "Witch", "Camp", "Redwood Valley", "Tubbs", "Valley", "Woolsey", 
               "Glass", "CZU Lightning Complex", "Nuns", "Butte")
v <- sapply(firenames, function(x){
  query <- paste0('//a[text()="', x, '"]')
  "https://en.wikipedia.org/wiki/List_of_California_wildfires" %>%
    GET() %>%
    content() %>%
    xml_find_all(xpath = query) %>%
    xml_attr("href") %>%
    unique() %>%
    `[`(1)
})

costs <- sapply(v, function(x){
  print(x)
  x %>% 
    paste0("https://en.wikipedia.org/", .) %>%
    read_html() %>%
    html_table() %>%
    pluck(1) %>%
    `[`(c(1:2)) %>%
     set_names("var", "val") %>%
    filter(var=="Cost") %>%
    pull(val)
}) %>%
  as.character() %>%
  data.frame() %>%
  set_names("fire") %>%
  mutate(order=str_extract(fire, c("million|billion"))) %>%
  mutate(order=case_when(
    order=="million" ~ 1000000, 
    order=="billion" ~ 1000000000)
  ) %>%
  mutate(val=as.numeric(str_extract(fire, "(?<=\\$)\\S+"))) %>%
  mutate(val=val*order)
writeClipboard(as.character(costs$val))




county_data <- readxl::read_xlsx("~/../Downloads/co-est2019-annres.xlsx") %>%
  set_names("county", 2010:2019) %>%
  mutate(county=str_replace(county, "^\\.", ""))
maxpop <- sapply(v, function(x){
  print(x)
  counties <- x %>% 
    paste0("https://en.wikipedia.org/", .) %>% 
    read_xml() %>%
    xml_find_all('//td[@class="infobox-data location"]//a[contains(text(), "County")]') %>%
    xml_attr("href") %>%
    str_replace_all("_", " ") %>%
    str_replace("/wiki/", "")
  county_data %>%
    filter(county%in%counties) %>%
    arrange(desc(`2019`)) %>%
    slice(1) %>%
    pull(`2019`)
})
writeClipboard(as.character(as.character(maxpop)))

counties <- c("Shasta", "Butte", "Mendocino", "Alameda", "Fresno", "Sonoma", 
              "Butte", "Ventura", "Washoe", "Tuolumne", "Santa Barbara", "Shasta", 
              "Trinity", "El Dorado", "Siskyou", "San Diego", "Butte", "Solano", 
              "Sonoma", "Lake", "Los Angeles", "Sonoma", "San Mateo", "Sonoma", "Amador") %>%
  paste("County, California")
maxpop <- sapply(counties, function(x){
  county_data %>%
    filter(county==x) %>%
    arrange(desc(`2019`)) %>%
    slice(1) %>%
    pull(`2019`)
})


wealth <- read.csv("~/../Downloads/ACSST5Y2020.S1901-2022-03-30T234601.csv")
names(wealth)[1] <- "label"
county_income <- wealth %>%
  filter(label=="Median income (dollars)") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(str_detect(rowname, "Households..Estimate")) %>%
  mutate(rowname=str_extract(rowname, ".*(?=\\.County)")) %>%
  mutate(rowname=paste(rowname, "County, California")) %>%
  mutate(rowname=str_replace(rowname, "\\.", " ")) %>%
  mutate(V1=str_replace(V1, ",", ""))
count_income <- sapply(counties, function(x){
  county_income %>% filter(rowname==x) %>% pull("V1")
}) %>% as.character()
writeClipboard(count_income)

counties %>%
  data.frame(rowname=.) %>%
  fuzzyjoin::stringdist_left_join(county_income) %>%
  # filter(rowname.x==rowname.y) %>%
  pull("V1") %>%
  str_replace(",", "") %>%
  writeClipboard() 
