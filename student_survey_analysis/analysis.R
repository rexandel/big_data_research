# ----- Connecting libraries -----
library("psych")
library("readxl")

# ----- Getting data from Excel -----
setwd("C:/Users/rexandel/Desktop/GitHub/big_data_research/student_survey_analysis")

survey <- read_excel("survey.xlsx")

data <- data.frame(survey)
data

# ----- Getting surnames and genres -----
surnames <- data[, 1]
surnames

genres <- names(data[, -1])
genres

# ----- Normalization of data -----
data[, sapply(data, is.numeric)] <- data[, sapply(data, is.numeric)] / 10
data

# ----- Calculating metrics -----
# sapply(data, is.numeric) - named logical vector, where TRUE = numeric
min_values <- sapply(data[, sapply(data, is.numeric)], min, na.rm = TRUE)
min_values

max_values <- sapply(data[, sapply(data, is.numeric)], max, na.rm = TRUE)
max_values

mean_values <- sapply(data[, sapply(data, is.numeric)], mean, na.rm = TRUE)
mean_values

median_values <- sapply(data[, sapply(data, is.numeric)], median, na.rm = TRUE)
median_values

mode_values <- sapply(data[, sapply(data, is.numeric)], function(x) as.numeric(names(which.max(table(x, useNA = "no")))))
mode_values

var_values <- sapply(data[, sapply(data, is.numeric)], var, na.rm = TRUE)
var_values

sd_values <- sapply(data[, sapply(data, is.numeric)], sd, na.rm = TRUE)
sd_values

skew_values <- sapply(data[, sapply(data, is.numeric)], skew, na.rm = TRUE)
skew_values

kurtosi_values <- sapply(data[, sapply(data, is.numeric)], kurtosi, na.rm = TRUE)
kurtosi_values

metrics <- data.frame(
                      min = min_values,
                      max = max_values,
                      scope = max_values - min_values,
                      mean = round(mean_values, 2),
                      median = median_values,
                      mode = mode_values,
                      var = round(var_values, 3),
                      sd = round(sd_values, 3),
                      skew = round(skew_values, 3),
                      kurtosi = round(kurtosi_values, 3)
                     )
metrics

# ----- Analysis of distribution of respondents by preference level -----
count_of_high_ratings <- colSums(data[, sapply(data, is.numeric)] > 0.7, na.rm = TRUE)
count_of_high_ratings

count_of_low_ratings <- colSums(data[, sapply(data, is.numeric)] < 0.3, na.rm = TRUE)
count_of_low_ratings

# ----- Working with missing data -----
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

# ----- Sorting data set -----

# Not necessary
# sorted_genres_asc <- sort(mean_values, decreasing = FALSE)
# sorted_genres_asc

# sorted_genres_desc <- sort(mean_values, decreasing = TRUE)
# sorted_genres_desc

# Detective
sorted_by_detective_asc <- data[order(data$Detective, decreasing = FALSE), ]
sorted_by_detective_asc

sorted_by_detective_desc <- data[order(data$Detective, decreasing = TRUE), ]
sorted_by_detective_desc

# Horror
sorted_by_horror_asc <- data[order(data$Horror, decreasing = FALSE), ]
sorted_by_horror_asc

sorted_by_horror_desc <- data[order(data$Horror, decreasing = TRUE), ]
sorted_by_horror_desc

# ----- Creating subsets of data -----
adventure_high <- data[data$Adventure > 0.8, ]
adventure_high
str(adventure_high)
summary(adventure_high)

horror_low <- subset(data, data$Horror < 0.4)
horror_low
str(horror_low)
summary(horror_low)

# ----- Setting up margins -----
par(mar = c(6, 4, 2, 1) + 0.1) # c(bottom, left, top, right)
# dev.off()

# ----- Plotting min, max, mean, median genre ratings barplots -----
barplot(height = min_values,
        main = "Minimum genre ratings",
        xlab = "",
        ylab = "Minimum rating",
        ylim = c(0, 1),
        yaxt = "n",
        col = rgb(0.9,0.2,0.5,0.6),
        las = 2)

title(xlab = "Genre", line = 4)
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
grid(nx = NA, ny = 10, col = "gray", lty = "dotted")

barplot(height = max_values,
        main = "Maximum genre ratings",
        xlab = "",
        ylab = "Maximum rating",
        ylim = c(0, 1),
        yaxt = "n",
        col = rgb(0.5,0.6,0.8,0.9),
        las = 2)

title(xlab = "Genre", line = 4)
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
grid(nx = NA, ny = 10, col = "gray", lty = "dotted")

barplot(height = mean_values,
        main = "Average genre ratings",
        xlab = "",
        ylab = "Average rating",
        ylim = c(0, 1),
        yaxt = "n",
        col = rgb(0.376, 0.808, 0.902),
        las = 2)

title(xlab = "Genre", line = 4)
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
grid(nx = NA, ny = 10, col = "gray", lty = "dotted")

barplot(height = median_values,
        main = "Median genre ratings",
        xlab = "",
        ylab = "Average rating",
        ylim = c(0, 1),
        yaxt = "n",
        col = rgb(0.44, 0.7, 0),
        las = 2)

title(xlab = "Genre", line = 4)
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
grid(nx = NA, ny = 10, col = "gray", lty = "dotted")

barplot(height = mode_values,
        main = "Mode genre ratings",
        xlab = "",
        ylab = "Average rating",
        ylim = c(0, 1),
        yaxt = "n",
        col = rgb(1, 0.7, 0),
        las = 2)

title(xlab = "Genre", line = 4)
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
grid(nx = NA, ny = 10, col = "gray", lty = "dotted")


# ----- Plotting barplot of rating by genre -----
barplot(height = data$Horror,
        names.arg = data$Surname,
        main = "Horror Ratings",
        xlab = "",
        ylab = "Rating",
        ylim = c(0, 1),
        yaxt = "n",
        col = rgb(0.682, 0.396, 0.831),
        las = 2,
        cex.names = 0.8)

title(xlab = "Surname", line = 4)
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
grid(nx = NA, ny = 10, col = "gray", lty = "dotted")

# ----- Plotting barplot of respondent's ratings -----
barplot(height = as.numeric(data[data$Surname == "Vavakin", sapply(data, is.numeric)]),
        names.arg = genres,
        main = "Respondent's ratings",
        xlab = "",
        ylab = "Rating",
        yaxt = "n",
        ylim = c(0, 1),
        col = rgb(0.749, 0.263, 0.412),
        las = 2)

title(xlab = "Genre", line = 4)
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
grid(nx = NA, ny = 10, col = "gray", lty = "dotted")

# ----- Plotting barplot of average ratings of each respondent -----
barplot(height = rowMeans(data[-1]),
        names.arg = data$Surname,
        main = "Average ratings of respondents",
        ylab = "Average score",
        ylim = c(0, 1),
        yaxt = "n",
        col = hcl.colors(nrow(data)),
        las = 2,
        cex.names = 0.8)

title(xlab = "Surname", line = 4)  
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
grid(nx = NA, ny = 10, col = "gray", lty = "dotted")

# ----- Plotting histogram of distribution of ratings by genre -----
hist(data$Action, 
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Action",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.831, 0.475, 0.584),
     border = "black")

lines(density(data$Action), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Sci.Fi, 
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Sci-Fi",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.561, 0.42, 0.463),
     border = "black")

lines(density(data$Sci.Fi), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Thriller, 
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Thriller",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.22, 0.42, 0.463),
     border = "black")

lines(density(data$Thriller), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Comedy, 
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Comedy",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.63, 0.82, 0.463),
     border = "black")

lines(density(data$Comedy), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Horror, 
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Horror",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.77, 0.651, 0.106),
     border = "black")

lines(density(data$Horror), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Drama, 
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Drama",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.77, 0.651, 0.85),
     border = "black")

lines(density(data$Drama, bw = 0.1), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Musical,
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Musical",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.427, 0.63, 0.541),
     border = "black")

lines(density(data$Musical), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Detective,
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Detective",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.82, 0.361, 0.298),
     border = "black")

lines(density(data$Detective), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Cartoon,
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Cartoon",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.82, 0.42, 0.063),
     border = "black")

lines(density(data$Cartoon), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

hist(data$Adventure,
     breaks = seq(0, 1, by = 0.1),
     xaxt = "n",
     main = "Distribution of ratings for Adventure",
     xlab = "Rating", 
     ylab = "Count of ratings",
     col = rgb(0.365, 0.769, 0.251),
     border = "black")

lines(density(data$Adventure), col = "black")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.1, 1, 0.1), cex.axis = 0.8)

# ----- Zeroing out margins -----
dev.off()

# ----- Plotting boxplots -----
boxplot(data$Action,
        main = "Distribution of ratings: Action",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.831, 0.475, 0.584))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Action)
IQR(data$Action)

boxplot(data$Sci.Fi,
        main = "Distribution of ratings: Sci-Fi",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.561, 0.42, 0.463))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Sci.Fi)
IQR(data$Sci.Fi)

boxplot(data$Thriller,
        main = "Distribution of ratings: Thriller",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.22, 0.42, 0.463))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Thriller)
IQR(data$Thriller)

boxplot(data$Comedy,
        main = "Distribution of ratings: Comedy",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.63, 0.82, 0.463))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Comedy)
IQR(data$Comedy)

boxplot(data$Horror,
        main = "Distribution of ratings: Horror",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.77, 0.651, 0.106))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Horror)
IQR(data$Horror)

boxplot(data$Drama,
        main = "Distribution of ratings: Drama",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.77, 0.651, 0.85))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Drama)
IQR(data$Drama)

boxplot(data$Musical,
        main = "Distribution of ratings: Musical",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.427, 0.63, 0.541))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Musical)
IQR(data$Musical)

boxplot(data$Detective,
        main = "Distribution of ratings: Detective",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.82, 0.361, 0.298))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Detective)
IQR(data$Detective)

boxplot(data$Cartoon,
        main = "Distribution of ratings: Cartoon",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.82, 0.42, 0.063))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Cartoon)
IQR(data$Cartoon)

boxplot(data$Adventure,
        main = "Distribution of ratings: Adventure",
        ylab = "Rating",
        ylim = c(0,1),
        yaxt = "n",
        col = rgb(0.365, 0.769, 0.251))

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
summary(data$Adventure)
IQR(data$Adventure)

# ----- Zeroing out margins -----
dev.off()
par(mar = c(8, 4, 4, 2))

# ----- Mega boxplot -----
genre_data <- list(
  Action = data$Action,
  Sci.Fi = data$Sci.Fi,
  Thriller = data$Thriller,
  Comedy = data$Comedy,
  Horror = data$Horror,
  Drama = data$Drama,
  Musical = data$Musical,
  Detective = data$Detective,
  Cartoon = data$Cartoon,
  Adventure = data$Adventure
)

genre_colors <- c(
  rgb(0.831, 0.475, 0.584),    # Action
  rgb(0.561, 0.42, 0.463),     # Sci.Fi
  rgb(0.22, 0.42, 0.463),      # Thriller
  rgb(0.63, 0.82, 0.463),      # Comedy
  rgb(0.77, 0.651, 0.106),     # Horror
  rgb(0.77, 0.651, 0.85),      # Drama
  rgb(0.427, 0.63, 0.541),     # Musical
  rgb(0.82, 0.361, 0.298),     # Detective
  rgb(0.82, 0.42, 0.063),      # Cartoon
  rgb(0.365, 0.769, 0.251)     # Adventure
)

boxplot(genre_data, 
        main = "Distribution of ratings by Genre",
        ylab = "Rating",
        ylim = c(0, 1),
        las = 2,
        col = genre_colors,
        yaxt = "n")

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
axis(2, at = seq(0.0, 1.0, 0.1), cex.axis = 0.8, las = 2)
