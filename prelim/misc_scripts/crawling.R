# Schweinberger, Martin. 2020. 
# Web Crawling and Scraping using R. Brisbane: 
# The University of Queensland. 
# url: https://slcladal.github.io/webcrawling.html 
# (Version 2020.12.03).

library(readtext)
library(rvest)

url <- "http:... "
html_document <- read_html(url)

links <- html_document %>%
  html_nodes(xpath = "//div[contains(@class, 'fc-item__inner')]/a") %>%
  html_attr(name = "href")



sessionInfo()