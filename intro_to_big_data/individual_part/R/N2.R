countries <- c("Germany", "Australia", "New Zealand", "Denmark",
               "Norway", "Iceland", "USA", "Canada", "Russia",
               "Belarus", "North Korea", "China", "Chile", "Argentina",
               "Brazil", "Switzerland", "Montenegro", "Armenia",
               "Israel", "South Korea", "Slovenia", "Serbia",
               "Portugal", "Poland", "Moldova", "Mexico", "Kazakhstan",
               "Japan")
countries


country_rank <- c(1 : length(countries))
country_rank


education_index <- c(0.946, 0.923, 0.923, 0.92, 0.919, 0.918, 0.899, 0.891, rep(NA, 19), 0.850)
education_index


expenses <- c(4.6, 5.1, 7.2, 8.7, 7.3, 7.8, 5.4, 4.8, rep(NA, 19), 3.8)
expenses


data <- data.frame(country_rank, countries, education_index, expenses)
data


data$continents <- c("Eurasia", "Australia", "Australia", "Eurasia",
                     "Eurasia", "Eurasia", "North America",
                     "North America", "Eurasia", "Eurasia",
                     "Eurasia", "Eurasia", "South America", "South America",
                     "South America", "Eurasia", "Eurasia", "Eurasia",
                     "Eurasia", "Eurasia", "Eurasia", "Eurasia", "Eurasia",
                     "Eurasia", "Eurasia", "South America", "Eurasia",
                     "Eurasia")
data


avg_edu_index <- round(mean(education_index[!is.na(education_index)]), digits = 3)
avg_edu_index

avg_expenses <- round(mean(expenses[!is.na(expenses)]), digits = 1)
avg_expenses


data$education_index[is.na(education_index)] <- avg_edu_index
data

data$expenses[is.na(expenses)] <- avg_expenses
data


colnames(data) <- c("Rang", "Country", "Education index", "Expenses, % of GDP", "Continent")
data


sorted_data <- data[order(data$`Expenses, % of GDP`, decreasing = FALSE), ]
sorted_data


data_eurasia <- data[data$Continent == "Eurasia", ]
data_eurasia


data_america <- data[grepl("America", data$Continent, fixed = TRUE), ]
data_america
