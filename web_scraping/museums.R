# ----- Setting up working directory -----
setwd("C:/Users/rexandel/Desktop/GitHub/big_data_research/web_scraping/")
getwd()

# ----- Connecting the necessary libraries -----
library(rvest)
library(openxlsx)

# ----- Downloading page from the Internet -----
url <- "https://www.sputnik8.com/ru/moscow"
filename <- "guided_tours.html"
download.file(url, destfile = filename, quiet = TRUE)

# ----- Getting downloaded page -----
content <- read_html(filename, encoding = "UTF-8")

# ----- Getting all containers responsible for tours -----
items <- content %>% 
  html_nodes(xpath = '//div[contains(@class, "items-grid_us0X") and contains(@class, "items-grid_size_3") and contains(@class, "gtm_main-block_listing")]') %>%
  html_children()

# ----- Initialize empty data frame -----
tours <- data.frame(
  Title = character(),
  Description = character(),
  Rating = character(),
  First_Date = character(),
  Second_Date = character(),
  Duration = character(),
  Link = character(),
  stringsAsFactors = FALSE
)

# ----- Getting data about specific tour -----
for (item in items) {
  # Getting tour name and link
  link_container <- item %>% html_node(xpath = './/a[@role="link"]')

  title <- link_container %>%
    html_node(xpath = './/div[contains(@class, "heading_")]') %>%
    html_text(trim = TRUE)

  href <- paste0("https://www.sputnik8.com", link_container %>% html_attr("href"))
  
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
  
  tours <- rbind(tours,
                 data.frame(
                   Title = title,
                   Description = description,
                   Rating = rating,
                   First_Date = first_date,
                   Second_Date = second_date,
                   Duration = duration,
                   Link = href,
                   stringsAsFactors = FALSE
                   )
                 )
}

# ----- Deleting a loaded page -----
file.remove(filename)

# ----- Viewing information about received tours -----
tours
View(tours)

# ----- Exporting information about received tours -----
write.xlsx(tours, "moscow_tours.xlsx", 
           sheetName = "Tours",
           colNames = TRUE,
           rowNames = FALSE, 
           append = FALSE)