library(rvest)

target_countries <- c("Brazil", "India", "Lebanon", "Turkey", "Denmark")
years <- 2014:2021
results_list <- list()

for (year in years) {
  # Getting all links
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  
  # Creating HTML files
  filename <- paste0("quality_of_life_", year, ".html")
  
  # Downloading HTML files
  download.file(url, destfile = filename, quiet = TRUE)
  
  # Getting the HTML code of the page
  content <- read_html(filename)
  
  # Getting the desired table
  data <- content %>%
    html_element("#t2") %>%
    html_table(fill = TRUE)

  # Explicit conversion to data.frame
  data <- as.data.frame(data)
  
  # Deleting the first column
  data <- data[, -1]
  
  colnames(data) <- c("Country", "Quality_of_Life_Index",
                      "Purchasing_Power_Index", "Safety_Index",
                      "Health_Care_Index", "Cost_of_Living_Index",
                      "Property_Price_to_Income_Ratio",
                      "Traffic_Commute_Time_Index",
                      "Pollution_Index", "Climate_Index")
  
  # Filter for target countries
  data_filtered <- data[data$Country %in% target_countries, ]
  
  # Add year column
  data_filtered$Year <- year
  
  # Store in results list
  results_list[[as.character(year)]] <- data_filtered
  
  # Remove temporary file
  file.remove(filename)
}

for (year in years) {
  assign(paste0("data_", year), results_list[[as.character(year)]])
}

