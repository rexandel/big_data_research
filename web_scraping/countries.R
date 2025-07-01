# ----- Connecting the necessary libraries -----
library(rvest)

# ----- Initial data -----
target_countries <- c("Brazil", "India", "Lebanon", "Turkey", "Denmark")
years <- 2014:2021
results_list <- list()

# ----- Getting data from the Internet -----
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
  
  # Store in results list
  results_list[[as.character(year)]] <- data_filtered
  
  # Remove temporary file
  file.remove(filename)
}

# ----- Getting data frames by year -----
for (year in years) {
  # Creating separate data frames by year
  assign(paste0("data_", year), results_list[[as.character(year)]])
}

# ----- Getting data frames by country ----- 
data_brazil <- data.frame()
data_india <- data.frame()
data_lebanon <- data.frame()
data_turkey <- data.frame()
data_denmark <- data.frame()

for (year in years) {
  year_data <- results_list[[as.character(year)]]
  
  year_data$Year <- year
  
  if ("Brazil" %in% year_data$Country) {
    data_brazil <- rbind(data_brazil, year_data[year_data$Country == "Brazil", ])
  }
  
  if ("India" %in% year_data$Country) {
    data_india <- rbind(data_india, year_data[year_data$Country == "India", ])
  }
  
  if ("Lebanon" %in% year_data$Country) {
    data_lebanon <- rbind(data_lebanon, year_data[year_data$Country == "Lebanon", ])
  }
  
  if ("Turkey" %in% year_data$Country) {
    data_turkey <- rbind(data_turkey, year_data[year_data$Country == "Turkey", ])
  }
  
  if ("Denmark" %in% year_data$Country) {
    data_denmark <- rbind(data_denmark, year_data[year_data$Country == "Denmark", ])
  }
}

# Deleting an unnecessary column
data_brazil <- data_brazil[, -1]
data_india <- data_india[, -1]
data_lebanon <- data_lebanon[, -1]
data_turkey <- data_turkey[, -1]
data_denmark <- data_denmark[, -1]

# ----- Working with missing values -----
# By countries
data_brazil[data_brazil == "-"] <- NA
data_india[data_india == "-"] <- NA
data_lebanon[data_lebanon == "-"] <- NA
data_turkey[data_turkey == "-"] <- NA
data_denmark[data_denmark == "-"] <- NA

# List of numeric columns
numeric_cols <- c("Quality_of_Life_Index", "Purchasing_Power_Index",
                  "Safety_Index", "Health_Care_Index", "Cost_of_Living_Index", 
                  "Property_Price_to_Income_Ratio", "Traffic_Commute_Time_Index",
                  "Pollution_Index", "Climate_Index")

# Converting values to numeric
data_brazil[numeric_cols] <- lapply(data_brazil[numeric_cols], as.numeric)
data_india[numeric_cols] <- lapply(data_india[numeric_cols], as.numeric)
data_lebanon[numeric_cols] <- lapply(data_lebanon[numeric_cols], as.numeric)
data_turkey[numeric_cols] <- lapply(data_turkey[numeric_cols], as.numeric)
data_denmark[numeric_cols] <- lapply(data_denmark[numeric_cols], as.numeric)

# Function for filling in missing values
fill_climate_na_first_valid <- function(df) {
  first_valid <- na.omit(df$Climate_Index)[1]
  df$Climate_Index[is.na(df$Climate_Index)] <- first_valid
  return(df)
}

# Using function to fill in missing values 
data_brazil <- fill_climate_na_first_valid(data_brazil)
data_india <- fill_climate_na_first_valid(data_india)
data_lebanon <- fill_climate_na_first_valid(data_lebanon)
data_turkey <- fill_climate_na_first_valid(data_turkey)
data_denmark <- fill_climate_na_first_valid(data_denmark)

# Displaying information about all countries in the console
data_brazil
data_india
data_lebanon
data_turkey
data_denmark
