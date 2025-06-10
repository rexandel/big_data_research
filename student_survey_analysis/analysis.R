library(readxl)
survey <- read_excel("C:/Users/rexandel/Desktop/GitHub/big_data_research/student_survey_analysis/survey.xlsx")

data <- data.frame(survey)
data

# --- Getting surnames and genres ---
surnames <- data[, 1]
surnames

genres <- names(data[, -1])
genres

# --- Normalization of data ---
data[, sapply(data, is.numeric)] <- data[, sapply(data, is.numeric)] / 10
data

# --- Calculating metrics ---
min_values <- sapply(data[, sapply(data, is.numeric)], min, na.rm = TRUE)
min_values

max_values <- sapply(data[, sapply(data, is.numeric)], max, na.rm = TRUE)
max_values

mean_values <- sapply(data[, sapply(data, is.numeric)], mean, na.rm = TRUE)
mean_values

# --- Analysis of distribution of respondents by preference level ---
count_of_high_ratings <- colSums(data[, sapply(data, is.numeric)] > 0.7, na.rm = TRUE)
count_of_high_ratings

count_of_low_ratings <- colSums(data[, sapply(data, is.numeric)] < 0.3, na.rm = TRUE)
count_of_low_ratings

# --- Working with missing data ---
data_mean_input <- data

numeric_cols <- sapply(data_mean_input, is.numeric)
data_mean_input[, numeric_cols] <- lapply(data_mean_input[, numeric_cols], function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  round(x, 1)
})
data_mean_input

data_zero <- data
numeric_cols <- sapply(data_zero, is.numeric)
data_zero[, numeric_cols] <- lapply(data_zero[, numeric_cols], function(x) {
  x[is.na(x)] <- 0
  x
})
data_zero

data_delete <- na.omit(data)
data_delete

data <- data_mean_input

# --- Sorting data set ---
sorted_genres_asc <- sort(mean_values, decreasing = FALSE)
sorted_genres_asc

sorted_genres_desc <- sort(mean_values, decreasing = TRUE)
sorted_genres_desc

# --- Creating subsets of data ---
sci_fi_high <- data[data$Sci.Fi > 0.7, ]
sci_fi_high

students_sci_fi_high <- sci_fi_high[, 1]
students_sci_fi_high

musical_low <- data[data$Musical > 0.7, ]
musical_low

students_musical_low <- musical_low[, 1]
students_musical_low


