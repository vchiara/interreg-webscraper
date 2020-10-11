library(rvest)
library(data.table)
library(tidyverse)
library(stringr)
library(countrycode)
library(RCurl)
library(xlsx)

scrape <- function(url) {
  site <- read_html(url) 
  
  page <- site %>%
    html_nodes('nav > ul > li.active') %>% 
    html_text()
  
  bp_type <- site %>%
    html_nodes('#keyword') %>% 
    html_attr('value') %>%
    str_squish()
  
  title <- site %>%
    html_nodes('.search-result__item__title > a') %>% 
    html_attr('title') %>%
    str_squish()
  
  link <- site %>%
    html_nodes('.search-result__item__title > a') %>% 
    html_attr('href') %>%
    str_squish()
  
  description <- site %>% 
    html_nodes('.search-result__item__description') %>%  
    html_text() %>%
    str_squish()
  
  summary <- description[c(TRUE, FALSE)]
  summary <- gsub("Summary:", "", summary)
  
  location <- description[c(FALSE, TRUE)]
  location <- gsub("Location:", "", location)
  
  df <- tibble(page, bp_type, title, link, summary, location)
  return(df)
}

mobility1_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=0&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Mobility%20%28tourism%20regions%29&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D%5Bcrdate%5D=DESC&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bpage%5D=0&tx_emgoodpractices_goodpracticessearch%5Baction%5D=index&tx_emgoodpractices_goodpracticessearch%5Bcontroller%5D=Search'
mobility2_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=0&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Mobility%20%28tourism%20regions%29&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D%5Bcrdate%5D=DESC&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Baction%5D=index&tx_emgoodpractices_goodpracticessearch%5Bcontroller%5D=Search'

mobility1 <- scrape(mobility1_url)
mobility2 <- scrape(mobility2_url)

rural1_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40extension%5D=EmGoodPractices&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40vendor%5D=EuropaMedia&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40controller%5D=Search&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40action%5D=index&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5Barguments%5D=YTo2OntzOjk6ImZpbHRlck1hcCI7czowOiIiO3M6NjoiZmlsdGVyIjthOjU6e3M6NDoicGFnZSI7czoxOiIwIjtzOjU6ImxpbWl0IjtzOjI6IjEwIjtzOjc6ImtleXdvcmQiO3M6MjY6Ik1vYmlsaXR5ICh0b3VyaXNtIHJlZ2lvbnMpIjtzOjk6ImludGVyZXN0cyI7czowOiIiO3M6OToib3JkZXJpbmdzIjthOjE6e3M6NjoiY3JkYXRlIjtzOjQ6IkRFU0MiO319czo3OiJjb3VudHJ5IjtzOjA6IiI7czo0OiJwYWdlIjtzOjE6IjAiO3M6NjoiYWN0aW9uIjtzOjU6ImluZGV4IjtzOjEwOiJjb250cm9sbGVyIjtzOjY6IlNlYXJjaCI7fQ%3D%3D519b47c900a51cc9bf7d0210c658469115b84309&tx_emgoodpractices_goodpracticessearch%5B__trustedProperties%5D=a%3A2%3A%7Bs%3A9%3A%22filterMap%22%3Bi%3A1%3Bs%3A6%3A%22filter%22%3Ba%3A5%3A%7Bs%3A4%3A%22page%22%3Bi%3A1%3Bs%3A5%3A%22limit%22%3Bi%3A1%3Bs%3A7%3A%22keyword%22%3Bi%3A1%3Bs%3A9%3A%22interests%22%3Ba%3A4%3A%7Bi%3A0%3Bi%3A1%3Bi%3A1%3Bi%3A1%3Bi%3A2%3Bi%3A1%3Bi%3A3%3Bi%3A1%3B%7Ds%3A9%3A%22orderings%22%3Bi%3A1%3B%7D%7Dcc9e3b9c29edc9ed3ec30674b098637dbf6d3d8b&tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=0&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Support+to+sector+%28tourism+%26+rural+SMEs%29&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D=crdate_desc'
rural2_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=0&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Support%20to%20sector%20%28tourism%20%26%20rural%20SMEs%29&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D%5Bcrdate%5D=DESC&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Baction%5D=index&tx_emgoodpractices_goodpracticessearch%5Bcontroller%5D=Search'
rural1 <- scrape(rural1_url)
rural2 <- scrape(rural2_url)

sector1_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40extension%5D=EmGoodPractices&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40vendor%5D=EuropaMedia&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40controller%5D=Search&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40action%5D=index&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5Barguments%5D=YTo2OntzOjk6ImZpbHRlck1hcCI7czowOiIiO3M6NjoiZmlsdGVyIjthOjU6e3M6NDoicGFnZSI7czoxOiIwIjtzOjU6ImxpbWl0IjtzOjI6IjEwIjtzOjc6ImtleXdvcmQiO3M6NDA6IlN1cHBvcnQgdG8gc2VjdG9yICh0b3VyaXNtICYgcnVyYWwgU01FcykiO3M6OToiaW50ZXJlc3RzIjtzOjA6IiI7czo5OiJvcmRlcmluZ3MiO2E6MTp7czo2OiJjcmRhdGUiO3M6NDoiREVTQyI7fX1zOjc6ImNvdW50cnkiO3M6MDoiIjtzOjQ6InBhZ2UiO3M6MToiMSI7czo2OiJhY3Rpb24iO3M6NToiaW5kZXgiO3M6MTA6ImNvbnRyb2xsZXIiO3M6NjoiU2VhcmNoIjt9a29f0804811ef7330b775ce06cf794e751cd2230&tx_emgoodpractices_goodpracticessearch%5B__trustedProperties%5D=a%3A2%3A%7Bs%3A9%3A%22filterMap%22%3Bi%3A1%3Bs%3A6%3A%22filter%22%3Ba%3A5%3A%7Bs%3A4%3A%22page%22%3Bi%3A1%3Bs%3A5%3A%22limit%22%3Bi%3A1%3Bs%3A7%3A%22keyword%22%3Bi%3A1%3Bs%3A9%3A%22interests%22%3Ba%3A4%3A%7Bi%3A0%3Bi%3A1%3Bi%3A1%3Bi%3A1%3Bi%3A2%3Bi%3A1%3Bi%3A3%3Bi%3A1%3B%7Ds%3A9%3A%22orderings%22%3Bi%3A1%3B%7D%7Dcc9e3b9c29edc9ed3ec30674b098637dbf6d3d8b&tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Support+to+sector+%28tourism%29&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D=crdate_desc'
sector2_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Support%20to%20sector%20%28tourism%29&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D%5Bcrdate%5D=DESC&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Baction%5D=index&tx_emgoodpractices_goodpracticessearch%5Bcontroller%5D=Search'
sector1 <- scrape(sector1_url)
sector2 <- scrape(sector2_url)

sme1_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40extension%5D=EmGoodPractices&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40vendor%5D=EuropaMedia&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40controller%5D=Search&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40action%5D=index&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5Barguments%5D=YTo2OntzOjk6ImZpbHRlck1hcCI7czowOiIiO3M6NjoiZmlsdGVyIjthOjU6e3M6NDoicGFnZSI7czoxOiIxIjtzOjU6ImxpbWl0IjtzOjI6IjEwIjtzOjc6ImtleXdvcmQiO3M6Mjc6IlN1cHBvcnQgdG8gc2VjdG9yICh0b3VyaXNtKSI7czo5OiJpbnRlcmVzdHMiO3M6MDoiIjtzOjk6Im9yZGVyaW5ncyI7YToxOntzOjY6ImNyZGF0ZSI7czo0OiJERVNDIjt9fXM6NzoiY291bnRyeSI7czowOiIiO3M6NDoicGFnZSI7czoxOiIxIjtzOjY6ImFjdGlvbiI7czo1OiJpbmRleCI7czoxMDoiY29udHJvbGxlciI7czo2OiJTZWFyY2giO30%3D1e8d2f482a1c4b8b9046004af07c0184422050ef&tx_emgoodpractices_goodpracticessearch%5B__trustedProperties%5D=a%3A2%3A%7Bs%3A9%3A%22filterMap%22%3Bi%3A1%3Bs%3A6%3A%22filter%22%3Ba%3A5%3A%7Bs%3A4%3A%22page%22%3Bi%3A1%3Bs%3A5%3A%22limit%22%3Bi%3A1%3Bs%3A7%3A%22keyword%22%3Bi%3A1%3Bs%3A9%3A%22interests%22%3Ba%3A4%3A%7Bi%3A0%3Bi%3A1%3Bi%3A1%3Bi%3A1%3Bi%3A2%3Bi%3A1%3Bi%3A3%3Bi%3A1%3B%7Ds%3A9%3A%22orderings%22%3Bi%3A1%3B%7D%7Dcc9e3b9c29edc9ed3ec30674b098637dbf6d3d8b&tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Support+to+SMEs+%28tourism+%26+destination+management%29&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D=crdate_desc'
sme2_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Support%20to%20SMEs%20%28tourism%20%26%20destination%20management%29&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D%5Bcrdate%5D=DESC&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Baction%5D=index&tx_emgoodpractices_goodpracticessearch%5Bcontroller%5D=Search'
sme1 <- scrape(sme1_url)
sme2 <- scrape(sme2_url)

sust1_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40extension%5D=EmGoodPractices&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40vendor%5D=EuropaMedia&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40controller%5D=Search&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5B%40action%5D=index&tx_emgoodpractices_goodpracticessearch%5B__referrer%5D%5Barguments%5D=YTo2OntzOjk6ImZpbHRlck1hcCI7czowOiIiO3M6NjoiZmlsdGVyIjthOjU6e3M6NDoicGFnZSI7czoxOiIxIjtzOjU6ImxpbWl0IjtzOjI6IjEwIjtzOjc6ImtleXdvcmQiO3M6NTA6IlN1cHBvcnQgdG8gU01FcyAodG91cmlzbSAmIGRlc3RpbmF0aW9uIG1hbmFnZW1lbnQpIjtzOjk6ImludGVyZXN0cyI7czowOiIiO3M6OToib3JkZXJpbmdzIjthOjE6e3M6NjoiY3JkYXRlIjtzOjQ6IkRFU0MiO319czo3OiJjb3VudHJ5IjtzOjA6IiI7czo0OiJwYWdlIjtzOjE6IjEiO3M6NjoiYWN0aW9uIjtzOjU6ImluZGV4IjtzOjEwOiJjb250cm9sbGVyIjtzOjY6IlNlYXJjaCI7fQ%3D%3D19a9d3edc429f1e356fb36dbf6c193c9c5dd8079&tx_emgoodpractices_goodpracticessearch%5B__trustedProperties%5D=a%3A2%3A%7Bs%3A9%3A%22filterMap%22%3Bi%3A1%3Bs%3A6%3A%22filter%22%3Ba%3A5%3A%7Bs%3A4%3A%22page%22%3Bi%3A1%3Bs%3A5%3A%22limit%22%3Bi%3A1%3Bs%3A7%3A%22keyword%22%3Bi%3A1%3Bs%3A9%3A%22interests%22%3Ba%3A4%3A%7Bi%3A0%3Bi%3A1%3Bi%3A1%3Bi%3A1%3Bi%3A2%3Bi%3A1%3Bi%3A3%3Bi%3A1%3B%7Ds%3A9%3A%22orderings%22%3Bi%3A1%3B%7D%7Dcc9e3b9c29edc9ed3ec30674b098637dbf6d3d8b&tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Sustainable+tourism&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D=crdate_desc'
sust2_url <- 'https://www.interregeurope.eu/policylearning/good-practices/?tx_emgoodpractices_goodpracticessearch%5BfilterMap%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Blimit%5D=10&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Bkeyword%5D=Sustainable%20tourism&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Binterests%5D=&tx_emgoodpractices_goodpracticessearch%5Bfilter%5D%5Borderings%5D%5Bcrdate%5D=DESC&tx_emgoodpractices_goodpracticessearch%5Bcountry%5D=&tx_emgoodpractices_goodpracticessearch%5Bpage%5D=1&tx_emgoodpractices_goodpracticessearch%5Baction%5D=index&tx_emgoodpractices_goodpracticessearch%5Bcontroller%5D=Search'
sust1 <- scrape(sust1_url)
sust2 <- scrape(sust2_url)

gp <- rbind(mobility1, mobility2, rural1, rural2, sector1, sector2, sme1, sme2, sust1, sust2)
gp <- distinct(gp, link, .keep_all = TRUE)
gp <- separate(data = gp, col = location, into = c("location", "country"), sep = ",")
gp$country <- str_replace(gp$country, " \\(.*\\)", "")
gp[40,7] <- "United Kingdom"
gp[57,7] <- "Ireland"
gp[58,7] <- "Spain"
gp[59,7] <- "Italy"
gp$country <- gp$country %>% str_squish()
gp$location <- gp$location %>% str_squish()
country_code <- countrycode(gp$country, origin ='country.name', destination ='iso3c')

gp <- cbind(gp, country_code)

write.csv(gp, "gp_interreg.csv")
