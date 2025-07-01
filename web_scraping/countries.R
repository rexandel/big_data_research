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

# ----- Charting the dynamics of the Quality Of Life Index -----
dev.off()
dev.new()

ylim_data_qoli <- c(min(data_brazil$Quality_of_Life_Index,
                        data_india$Quality_of_Life_Index,
                        data_lebanon$Quality_of_Life_Index,
                        data_turkey$Quality_of_Life_Index,
                        data_denmark$Quality_of_Life_Index) - 5,
                    max(data_brazil$Quality_of_Life_Index,
                        data_india$Quality_of_Life_Index,
                        data_lebanon$Quality_of_Life_Index,
                        data_turkey$Quality_of_Life_Index,
                        data_denmark$Quality_of_Life_Index) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Quality_of_Life_Index,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Quality of Life Index",
     main = "Brazil: Quality of Life Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_qoli)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Quality_of_Life_Index,
     labels = data_brazil$Quality_of_Life_Index,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Quality_of_Life_Index,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Quality of Life Index",
     main = "India: Quality of Life Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_qoli)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Quality_of_Life_Index,
     labels = data_india$Quality_of_Life_Index,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Quality_of_Life_Index,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Quality of Life Index",
     main = "Lebanon: Quality of Life Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_qoli)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Quality_of_Life_Index,
     labels = data_lebanon$Quality_of_Life_Index,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Quality_of_Life_Index,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Quality of Life Index",
     main = "Turkey: Quality of Life Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_qoli)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Quality_of_Life_Index,
     labels = data_turkey$Quality_of_Life_Index,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Quality_of_Life_Index,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Quality of Life Index",
     main = "Denmark: Quality of Life Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_qoli)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Quality_of_Life_Index,
     labels = data_denmark$Quality_of_Life_Index,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_qoli,
     xlab = "Year", 
     ylab = "Quality of Life Index",
     main = "Quality of Life Index by Country (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Quality_of_Life_Index, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Quality_of_Life_Index, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Quality_of_Life_Index, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Quality_of_Life_Index, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Quality_of_Life_Index, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

# ----- Charting the dynamics of the Purchasing Power Index -----
dev.off()
dev.new()

ylim_data_ppi <- c(min(data_brazil$Purchasing_Power_Index,
                       data_india$Purchasing_Power_Index,
                       data_lebanon$Purchasing_Power_Index,
                       data_turkey$Purchasing_Power_Index,
                       data_denmark$Purchasing_Power_Index) - 5,
                   max(data_brazil$Purchasing_Power_Index,
                       data_india$Purchasing_Power_Index,
                       data_lebanon$Purchasing_Power_Index,
                       data_turkey$Purchasing_Power_Index,
                       data_denmark$Purchasing_Power_Index) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Purchasing_Power_Index,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Purchasing Power Index",
     main = "Brazil: Purchasing Power Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ppi)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Purchasing_Power_Index,
     labels = data_brazil$Purchasing_Power_Index,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Purchasing_Power_Index,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Purchasing Power Index",
     main = "India: Purchasing Power Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ppi)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Purchasing_Power_Index,
     labels = data_india$Purchasing_Power_Index,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Purchasing_Power_Index,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Purchasing Power Index",
     main = "Lebanon: Purchasing Power Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ppi)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Purchasing_Power_Index,
     labels = data_lebanon$Purchasing_Power_Index,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Purchasing_Power_Index,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Purchasing Power Index",
     main = "Turkey: Purchasing Power Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ppi)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Purchasing_Power_Index,
     labels = data_turkey$Purchasing_Power_Index,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Purchasing_Power_Index,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Purchasing Power Index",
     main = "Denmark: Purchasing Power Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ppi)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Purchasing_Power_Index,
     labels = data_denmark$Purchasing_Power_Index,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_ppi,
     xlab = "Year", 
     ylab = "Purchasing Power Index",
     main = "Purchasing Power Index by Country (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Purchasing_Power_Index, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Purchasing_Power_Index, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Purchasing_Power_Index, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Purchasing_Power_Index, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Purchasing_Power_Index, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

# ----- Charting the dynamics of the Safety Index -----
dev.off()
dev.new()

ylim_data_si <- c(min(data_brazil$Safety_Index,
                       data_india$Safety_Index,
                       data_lebanon$Safety_Index,
                       data_turkey$Safety_Index,
                       data_denmark$Safety_Index) - 5,
                   max(data_brazil$Safety_Index,
                       data_india$Safety_Index,
                       data_lebanon$Safety_Index,
                       data_turkey$Safety_Index,
                       data_denmark$Safety_Index) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Safety_Index,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Safety Index",
     main = "Brazil: Safety Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_si)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Safety_Index,
     labels = data_brazil$Safety_Index,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Safety_Index,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Safety Index",
     main = "India: Safety Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_si)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Safety_Index,
     labels = data_india$Safety_Index,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Safety_Index,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Safety Index",
     main = "Lebanon: Safety Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_si)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Safety_Index,
     labels = data_lebanon$Safety_Index,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Safety_Index,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Safety Index",
     main = "Turkey: Safety Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_si)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Safety_Index,
     labels = data_turkey$Safety_Index,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Safety_Index,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Safety Index",
     main = "Denmark: Safety Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_si)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Safety_Index,
     labels = data_denmark$Safety_Index,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_si,
     xlab = "Year", 
     ylab = "Safety Index",
     main = "Safety Index by Country (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Safety_Index, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Safety_Index, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Safety_Index, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Safety_Index, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Safety_Index, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

# ----- Charting the dynamics of the Health Care Index -----
dev.off()
dev.new()

ylim_data_hci <- c(min(data_brazil$Health_Care_Index,
                      data_india$Health_Care_Index,
                      data_lebanon$Health_Care_Index,
                      data_turkey$Health_Care_Index,
                      data_denmark$Health_Care_Index) - 5,
                  max(data_brazil$Health_Care_Index,
                      data_india$Health_Care_Index,
                      data_lebanon$Health_Care_Index,
                      data_turkey$Health_Care_Index,
                      data_denmark$Health_Care_Index) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Health_Care_Index,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Health Care Index",
     main = "Brazil: Health Care Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_hci)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Health_Care_Index,
     labels = data_brazil$Health_Care_Index,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Health_Care_Index,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Health Care Index",
     main = "India: Health Care Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_hci)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Health_Care_Index,
     labels = data_india$Health_Care_Index,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Health_Care_Index,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Health Care Index",
     main = "Lebanon: Health Care Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_hci)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Health_Care_Index,
     labels = data_lebanon$Health_Care_Index,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Health_Care_Index,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Health Care Index",
     main = "Turkey: Health Care Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_hci)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Health_Care_Index,
     labels = data_turkey$Health_Care_Index,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Health_Care_Index,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Health Care Index",
     main = "Denmark: Health Care Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_hci)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Health_Care_Index,
     labels = data_denmark$Health_Care_Index,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_hci,
     xlab = "Year", 
     ylab = "Health Care Index",
     main = "Health Care Index by Country (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Health_Care_Index, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Health_Care_Index, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Health_Care_Index, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Health_Care_Index, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Health_Care_Index, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

# ----- Charting the dynamics of the Cost Of Living Index -----
dev.off()
dev.new()

ylim_data_coli <- c(min(data_brazil$Cost_of_Living_Index,
                       data_india$Cost_of_Living_Index,
                       data_lebanon$Cost_of_Living_Index,
                       data_turkey$Cost_of_Living_Index,
                       data_denmark$Cost_of_Living_Index) - 5,
                   max(data_brazil$Cost_of_Living_Index,
                       data_india$Cost_of_Living_Index,
                       data_lebanon$Cost_of_Living_Index,
                       data_turkey$Cost_of_Living_Index,
                       data_denmark$Cost_of_Living_Index) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Cost_of_Living_Index,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Cost Of Living Index",
     main = "Brazil: Cost Of Living Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_coli)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Cost_of_Living_Index,
     labels = data_brazil$Cost_of_Living_Index,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Cost_of_Living_Index,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Cost Of Living Index",
     main = "India: Cost Of Living Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_coli)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Cost_of_Living_Index,
     labels = data_india$Cost_of_Living_Index,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Cost_of_Living_Index,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Cost Of Living Index",
     main = "Lebanon: Cost Of Living Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_coli)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Cost_of_Living_Index,
     labels = data_lebanon$Cost_of_Living_Index,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Cost_of_Living_Index,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Cost Of Living Index",
     main = "Turkey: Cost Of Living Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_coli)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Cost_of_Living_Index,
     labels = data_turkey$Cost_of_Living_Index,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Cost_of_Living_Index,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Cost Of Living Index",
     main = "Denmark: Cost Of Living Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_coli)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Cost_of_Living_Index,
     labels = data_denmark$Cost_of_Living_Index,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_coli,
     xlab = "Year", 
     ylab = "Cost Of Living Index",
     main = "Cost Of Living Index by Country (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Cost_of_Living_Index, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Cost_of_Living_Index, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Cost_of_Living_Index, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Cost_of_Living_Index, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Cost_of_Living_Index, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

# ----- Charting the dynamics of the Property Price To Income Ratio -----
dev.off()
dev.new()

ylim_data_pptir <- c(min(data_brazil$Property_Price_to_Income_Ratio,
                       data_india$Property_Price_to_Income_Ratio,
                       data_lebanon$Property_Price_to_Income_Ratio,
                       data_turkey$Property_Price_to_Income_Ratio,
                       data_denmark$Property_Price_to_Income_Ratio) - 5,
                   max(data_brazil$Property_Price_to_Income_Ratio,
                       data_india$Property_Price_to_Income_Ratio,
                       data_lebanon$Property_Price_to_Income_Ratio,
                       data_turkey$Property_Price_to_Income_Ratio,
                       data_denmark$Property_Price_to_Income_Ratio) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Property_Price_to_Income_Ratio,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Property Price To Income Ratio",
     main = "Brazil: Property Price To Income Ratio (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pptir)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Property_Price_to_Income_Ratio,
     labels = data_brazil$Property_Price_to_Income_Ratio,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Property_Price_to_Income_Ratio,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Property Price To Income Ratio",
     main = "India: Property Price To Income Ratio (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pptir)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Property_Price_to_Income_Ratio,
     labels = data_india$Property_Price_to_Income_Ratio,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Property_Price_to_Income_Ratio,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Property Price To Income Ratio",
     main = "Lebanon: Property Price To Income Ratio (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pptir)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Property_Price_to_Income_Ratio,
     labels = data_lebanon$Property_Price_to_Income_Ratio,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Property_Price_to_Income_Ratio,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Property Price To Income Ratio",
     main = "Turkey: Property Price To Income Ratio (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pptir)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Property_Price_to_Income_Ratio,
     labels = data_turkey$Property_Price_to_Income_Ratio,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Property_Price_to_Income_Ratio,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Property Price To Income Ratio",
     main = "Denmark: Property Price To Income Ratio (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pptir)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Property_Price_to_Income_Ratio,
     labels = data_denmark$Property_Price_to_Income_Ratio,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_pptir,
     xlab = "Year", 
     ylab = "Property Price To Income Ratio",
     main = "Property Price To Income Ratio (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Property_Price_to_Income_Ratio, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Property_Price_to_Income_Ratio, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Property_Price_to_Income_Ratio, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Property_Price_to_Income_Ratio, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Property_Price_to_Income_Ratio, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

# ----- Charting the dynamics of the Traffic Commute Time Index -----
dev.off()
dev.new()

ylim_data_tcti <- c(min(data_brazil$Traffic_Commute_Time_Index,
                         data_india$Traffic_Commute_Time_Index,
                         data_lebanon$Traffic_Commute_Time_Index,
                         data_turkey$Traffic_Commute_Time_Index,
                         data_denmark$Traffic_Commute_Time_Index) - 5,
                     max(data_brazil$Traffic_Commute_Time_Index,
                         data_india$Traffic_Commute_Time_Index,
                         data_lebanon$Traffic_Commute_Time_Index,
                         data_turkey$Traffic_Commute_Time_Index,
                         data_denmark$Traffic_Commute_Time_Index) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Traffic_Commute_Time_Index,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Traffic Commute Time Index",
     main = "Brazil: Traffic Commute Time Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_tcti)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Traffic_Commute_Time_Index,
     labels = data_brazil$Traffic_Commute_Time_Index,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Traffic_Commute_Time_Index,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Traffic Commute Time Index",
     main = "India: Traffic Commute Time Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_tcti)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Traffic_Commute_Time_Index,
     labels = data_india$Traffic_Commute_Time_Index,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Traffic_Commute_Time_Index,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Traffic Commute Time Index",
     main = "Lebanon: Traffic Commute Time Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_tcti)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Traffic_Commute_Time_Index,
     labels = data_lebanon$Traffic_Commute_Time_Index,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Traffic_Commute_Time_Index,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Traffic Commute Time Index",
     main = "Turkey: Traffic Commute Time Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_tcti)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Traffic_Commute_Time_Index,
     labels = data_turkey$Traffic_Commute_Time_Index,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Traffic_Commute_Time_Index,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Traffic Commute Time Index",
     main = "Denmark: Traffic Commute Time Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_tcti)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Traffic_Commute_Time_Index,
     labels = data_denmark$Traffic_Commute_Time_Index,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_tcti,
     xlab = "Year", 
     ylab = "Traffic Commute Time Index",
     main = "Traffic Commute Time Index (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Traffic_Commute_Time_Index, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Traffic_Commute_Time_Index, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Traffic_Commute_Time_Index, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Traffic_Commute_Time_Index, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Traffic_Commute_Time_Index, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

# ----- Charting the dynamics of the Pollution Index  -----
dev.off()
dev.new()

ylim_data_pi <- c(min(data_brazil$Pollution_Index,
                        data_india$Pollution_Index,
                        data_lebanon$Pollution_Index,
                        data_turkey$Pollution_Index,
                        data_denmark$Pollution_Index) - 5,
                    max(data_brazil$Pollution_Index,
                        data_india$Pollution_Index,
                        data_lebanon$Pollution_Index,
                        data_turkey$Pollution_Index,
                        data_denmark$Pollution_Index) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Pollution_Index,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Pollution Index",
     main = "Brazil: Pollution Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pi)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Pollution_Index,
     labels = data_brazil$Pollution_Index,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Pollution_Index,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Pollution Index",
     main = "India: Pollution Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pi)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Pollution_Index,
     labels = data_india$Pollution_Index,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Pollution_Index,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Pollution Index",
     main = "Lebanon: Pollution Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pi)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Pollution_Index,
     labels = data_lebanon$Pollution_Index,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Pollution_Index,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Pollution Index",
     main = "Turkey: Pollution Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pi)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Pollution_Index,
     labels = data_turkey$Pollution_Index,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Pollution_Index,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Pollution Index",
     main = "Denmark: Pollution Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_pi)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Pollution_Index,
     labels = data_denmark$Pollution_Index,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_pi,
     xlab = "Year", 
     ylab = "Pollution Index",
     main = "Pollution Index (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Pollution_Index, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Pollution_Index, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Pollution_Index, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Pollution_Index, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Pollution_Index, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

# ----- Charting the dynamics of the Climate Index  -----
dev.off()
dev.new()

ylim_data_ci <- c(min(data_brazil$Climate_Index,
                      data_india$Climate_Index,
                      data_lebanon$Climate_Index,
                      data_turkey$Climate_Index,
                      data_denmark$Climate_Index) - 5,
                  max(data_brazil$Climate_Index,
                      data_india$Climate_Index,
                      data_lebanon$Climate_Index,
                      data_turkey$Climate_Index,
                      data_denmark$Climate_Index) + 10)

# Brazil
plot(data_brazil$Year,
     data_brazil$Climate_Index,
     type = "o",
     col = "darkgreen",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Climate Index",
     main = "Brazil: Climate Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ci)

axis(1, at = data_brazil$Year, labels = data_brazil$Year)
text(data_brazil$Year,
     data_brazil$Climate_Index,
     labels = data_brazil$Climate_Index,
     pos = 3, cex = 0.8)

# India
plot(data_india$Year,
     data_india$Climate_Index,
     type = "o",
     col = "darkred",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Climate Index",
     main = "India: Climate Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ci)

axis(1, at = data_india$Year, labels = data_india$Year)
text(data_india$Year,
     data_india$Climate_Index,
     labels = data_india$Climate_Index,
     pos = 3, cex = 0.8)

# Lebanon
plot(data_lebanon$Year,
     data_lebanon$Climate_Index,
     type = "o",
     col = "darkblue",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Climate Index",
     main = "Lebanon: Climate Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ci)

axis(1, at = data_lebanon$Year, labels = data_lebanon$Year)
text(data_lebanon$Year,
     data_lebanon$Climate_Index,
     labels = data_lebanon$Climate_Index,
     pos = 3, cex = 0.8)

# Turkey
plot(data_turkey$Year,
     data_turkey$Climate_Index,
     type = "o",
     col = "orange2",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Climate Index",
     main = "Turkey: Climate Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ci)

axis(1, at = data_turkey$Year, labels = data_turkey$Year)
text(data_turkey$Year,
     data_turkey$Climate_Index,
     labels = data_turkey$Climate_Index,
     pos = 3, cex = 0.8)

# Denmark
plot(data_denmark$Year,
     data_denmark$Climate_Index,
     type = "o",
     col = "purple3",
     lwd = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Climate Index",
     main = "Denmark: Climate Index (2014-2021)",
     xaxt = "n",
     ylim = ylim_data_ci)

axis(1, at = data_denmark$Year, labels = data_denmark$Year)
text(data_denmark$Year,
     data_denmark$Climate_Index,
     labels = data_denmark$Climate_Index,
     pos = 3, cex = 0.8)

# All Countries
par(mar = c(5, 5, 5, 8) + 0.1) # c(bottom, left, top, right)

plot(NA, 
     xlim = range(years), 
     ylim = ylim_data_ci,
     xlab = "Year", 
     ylab = "Climate Index",
     main = "Climate Index (2014-2021)",
     xaxt = "n",
     las = 1)

axis(1, at = years, labels = years)

lines(data_brazil$Year, data_brazil$Climate_Index, 
      type = "o", col = "darkgreen", lwd = 2, pch = 19)

lines(data_india$Year, data_india$Climate_Index, 
      type = "o", col = "darkred", lwd = 2, pch = 19)

lines(data_lebanon$Year, data_lebanon$Climate_Index, 
      type = "o", col = "darkblue", lwd = 2, pch = 19)

lines(data_turkey$Year, data_turkey$Climate_Index, 
      type = "o", col = "orange2", lwd = 2, pch = 19)

lines(data_denmark$Year, data_denmark$Climate_Index, 
      type = "o", col = "purple3", lwd = 2, pch = 19)

legend("topright",
       inset = c(-0.25, 0),
       legend = c("Brazil", "India", "Lebanon", "Turkey", "Denmark"),
       col = c("darkgreen", "darkred", "darkblue", "orange2", "purple3"),
       lwd = 2, 
       pch = 19,
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")

