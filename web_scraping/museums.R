# ----- Connecting the necessary libraries -----
library(rvest)

url <- "https://www.sputnik8.com/ru/moscow"
filename <- "guided_tours.html"
download.file(url, destfile = filename, quiet = TRUE)

content <- read_html(filename, encoding = "UTF-8")

items <- content %>% 
  html_nodes(xpath = '//div[contains(@class, "items-grid_us0X") and contains(@class, "items-grid_size_3") and contains(@class, "gtm_main-block_listing")]') %>%
  html_children()


for (item in items) {
  # Getting tour name and link
  link_container <- item %>% html_node(xpath = './/a[@role="link"]')

  title <- link_container %>%
    html_node(xpath = './/div[contains(@class, "heading_")]') %>%
    html_text(trim = TRUE)

  href <- paste0("https://www.sputnik8.com/", link_node %>% html_attr("href"))
  
  # Getting tour rating
  rating <- item %>%
    html_node(xpath = './/div[contains(@class, "ui-text_size_s_9H8Q")]') %>%
    html_text(trim = TRUE)
  
  description <- item %>%
    html_node(xpath = './/div/div/div[
      contains(@class, "ui-text_size_s_9H8Q") and 
      contains(@class, "ui-text_color_99_JRt1") and 
      contains(@class, "description_phiK")
    ]') %>%
    html_text(trim = TRUE)
  
  # Getting tour date
  events_container <- item %>% html_node(xpath = './/div[contains(@class, "events_UOUT")]')
  
  dates <- events_container %>%
    html_nodes(xpath = './/div[contains(@class, "event_Opi1")]//div[contains(@class, "ui-text_size_s_9H8Q")]') %>%
    html_text(trim = TRUE)
  
  first_date <- ifelse(length(dates) >= 1, dates[1], NA)
  second_date <- ifelse(length(dates) >= 2, dates[2], NA)
  
  # Getting tour time
  details_container <- item %>% 
    html_node(xpath = './/div[contains(@class, "details_ZBWU")]')
  
  details <- details_container %>%
    html_nodes(xpath = './/div[contains(@class, "detail_QQhb")]') %>%
    html_text(trim = TRUE)
  
  duration <- details[grep("ч\\.", details)]
  
  print(title)
  print(description)
  print(rating)
  print(first_date)
  print(second_date)
  print(duration)
  print(href)
}