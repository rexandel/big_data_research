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
