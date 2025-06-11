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

sorted_by_detective_asc <- data[order(data$Detective, decreasing = FALSE), ]
sorted_by_detective_asc

sorted_by_detective_desc <- data[order(data$Detective, decreasing = TRUE), ]
sorted_by_detective_desc

# --- Creating subsets of data ---
sci_fi_high <- data[data$Sci.Fi > 0.7, ]
sci_fi_high

students_sci_fi_high <- sci_fi_high[, 1]
students_sci_fi_high

musical_low <- data[data$Musical < 0.3, ]
musical_low

students_musical_low <- musical_low[, 1]
students_musical_low

# --- Setting up margins ---
par(mar = c(6, 4, 2, 1) + 0.1) # c(bottom, left, top, right)
# dev.off()

# --- Plotting min, max, mean genre ratings barplots ---
barplot(min_values,
        main = "Minimum genre ratings",
        xlab = "",
        ylab = "Minimum rating",
        ylim = c(0, 1),
        col = rgb(0.9,0.2,0.5,0.6),
        las = 2)

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
title(xlab = "Genre", line = 4)

barplot(max_values,
        main = "Maximum genre ratings",
        xlab = "",
        ylab = "Maximum rating",
        ylim = c(0, 1),
        col = rgb(0.5,0.6,0.8,0.9),
        las = 2)

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
title(xlab = "Genre", line = 4)

barplot(mean_values,
        main = "Average genre ratings",
        xlab = "",
        ylab = "Average rating",
        ylim = c(0, 1),
        col = rgb(0.376, 0.808, 0.902),
        las = 2)

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
title(xlab = "Genre", line = 4)

# --- Plotting rating barplot by genre  ---
barplot(height = data$Horror,
        names.arg = data$Surname,
        main = "Horror Ratings",
        xlab = "",
        ylab = "Rating",
        ylim = c(0, 1),
        col = rgb(0.682, 0.396, 0.831),
        las = 2,
        cex.names = 0.8)

title(xlab = "Surname", line = 5)

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

# --- Plotting barplot of respondent's ratings ---
barplot(as.numeric(data[data$Surname == "Vavakin", sapply(data, is.numeric)]),
        names.arg = genres,
        main = "Respondent's ratings",
        xlab = "",
        ylab = "Rating",
        ylim = c(0, 1),
        col = rgb(0.749, 0.263, 0.412),
        las = 2)

title(xlab = "Genre", line = 5)

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

# --- Plotting average ratings of each respondent ---
barplot(height = rowMeans(data[-1]),
        names.arg = data$Surname,
        main = "Average ratings of respondents",
        ylab = "Average score",
        ylim = c(0, 1),
        col = hcl.colors(nrow(data)),
        las = 2,
        cex.names = 0.7)
        
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

# --- Setting up margins ---
dev.off()

# --- Plotting boxplots ---
boxplot(data$Action,
        main = "Distribution of ratings: Action",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.831, 0.475, 0.584))

boxplot(data$Sci.Fi,
        main = "Distribution of ratings: Sci-Fi",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.561, 0.42, 0.463))

boxplot(data$Thriller,
        main = "Distribution of ratings: Thriller",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.22, 0.42, 0.463))

boxplot(data$Comedy,
        main = "Distribution of ratings: Comedy",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.63, 0.82, 0.463))

boxplot(data$Horror,
        main = "Distribution of ratings: Horror",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.77, 0.651, 0.106))

boxplot(data$Drama,
        main = "Distribution of ratings: Drama",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.77, 0.651, 0.85))

boxplot(data$Musical,
        main = "Distribution of ratings: Musical",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.427, 0.63, 0.541))

boxplot(data$Detective,
        main = "Distribution of ratings: Detective",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.82, 0.361, 0.298))

boxplot(data$Cartoon,
        main = "Distribution of ratings: Cartoon",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.82, 0.42, 0.063))

boxplot(data$Adventure,
        main = "Distribution of ratings: Adventure",
        ylab = "Rating",
        ylim = c(0,1),
        col = rgb(0.365, 0.769, 0.251))