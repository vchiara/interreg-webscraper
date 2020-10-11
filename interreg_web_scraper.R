library(tidyverse)
library(rvest)
library(stringr)
library(purrr)

extract_bp_data <- function(url){
  
  #read the page
  web <- read_html(url)
  
  #extract summary
  type <- web %>%
    html_nodes('.thematic-icons > i') %>% 
    html_attr("class") %>%
    as.character() %>% 
    str_squish()
  
  type <- type[1]
  
  title <- web %>%
    html_nodes('.dk-banner-title') %>% 
    html_text() %>%
    str_squish() %>%
    str_to_title(locale = "en")
    
  institution <- web %>%
    html_nodes('.person-details-info__get-in-touch:nth-child(1) > div:nth-child(3) > div:nth-child(2)') %>% 
    html_text() %>%
    str_squish()
  
  contact <- web %>%
    html_nodes('.person-details-info__get-in-touch__details') %>% 
    html_text() %>%
    str_squish()
    
  summary <- web %>%
    html_nodes('.good-practice-text.dk-article-summary') %>% 
    html_text() %>%
    str_squish()
  
  text <- web %>%
    html_nodes('.good-practice-text:nth-child(3)') %>% 
    html_text() %>%
    str_squish()
  
  #extract data from pages with different structure
  resources <- web %>%
    html_nodes('.good-practice-text:nth-child(6)') %>%
    html_text() %>%
    str_squish()
  
  results <- web %>%
    html_nodes('.good-practice-text:nth-child(8)') %>%
    html_text() %>%
    str_squish()
  
  theme <- web %>% 
    html_nodes('h2')
  
  if (length(theme) > 3){
  difficulties <- web %>%
    html_nodes('.good-practice-text:nth-child(10)') %>%
    html_text() %>%
    str_squish()
  
  transferability <- web %>%
    html_nodes('.good-practice-text:nth-child(12)') %>%
    html_text() %>%
    str_squish()
  } else {
    
    difficulties <- c("Not available")
    
    transferability <- web %>%
      html_nodes('.good-practice-text:nth-child(10)') %>%
      html_text() %>%
      str_squish()
  }
  
  dates <- web %>%
    html_nodes('.person-details-info__get-in-touch:nth-child(1) > div:nth-child(5) > div:nth-child(2)') %>%
    html_text() %>%
    str_squish() %>%
    str_extract('\\d.\\d+') %>% 
    paste("-01-01", sep = "")
  
  ongoing <- web %>%
    html_nodes('.person-details-info__get-in-touch:nth-child(1) > div:nth-child(6) > div:nth-child(2)') %>%
    html_text() %>%
    str_squish()
    
  if (ongoing == "Ongoing"){
    ongoing <- "Yes"
  } else{
      ongoing <- "No"
  }
  
  link <- web %>%
    html_nodes('.data-further-information > div:nth-child(1) > a:nth-child(1)') %>%
    html_attr('href') %>%
    str_squish() 
  
  if(length(link)==0){
    project <- web %>%
      html_nodes('.data-acronym > a') %>% 
      html_attr("href") %>%
      str_squish()
    link <- paste("https://www.interregeurope.eu/", project, sep = "")}
  
  tags <- web %>%
    html_nodes('.dk-tag-enumeration') %>%
    html_text() %>%
    str_squish()
  
  if(length(tags)==0){
    tags <- c("Not available")}
  
  #create the tibble
  bp_data <- tibble(type, title, institution, contact, summary, text, resources, results, difficulties, 
                    transferability, dates, ongoing, link, tags)
  
  return(bp_data)
}

#using the function on a single url
url <-"https://www.interregeurope.eu/policylearning/good-practices/item/2912/sowaccess-ecosystem/"

bp_data <- extract_bp_data(url)

#using the function on a list of urls
bp <- read_csv("gp_interreg.csv")

urls <- bp$link

bp_data_df <- tibble(url = urls,
                     results = map(url, extract_bp_data))

bp_data_df2 <- bp_data_df %>% unnest(cols = c(results))

bp_data_df2 <- cbind(bp_data_df2, bp$location, bp$country, bp$country_code)

write.csv(bp_data_df2, "gp_interreg_data.csv")