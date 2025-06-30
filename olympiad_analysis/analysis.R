# ----- Connecting necessary libraries -----
import(readxl)

# ----- Setting up working directory -----
setwd("C:/Users/rexandel/Desktop/GitHub/big_data_research/olympiad_analysis")
getwd()

# ----- Importing data from Excel -----
norway_beatlon <- read_excel("norway_beatlon.xlsx") 
norway_beatlon <- data.frame(norway_beatlon)
names(norway_beatlon) <- c("Olympiad", 
                 "1", "2", "3", "4",
                 "5", "6", "7", "8")
norway_beatlon

norway_beatlon_genders <- read_excel("norway_beatlon_genders.xlsx")
norway_beatlon_genders <- data.frame(norway_beatlon_genders)
names(norway_beatlon_genders) <- c("Olympiad", 
                           "1 (M)", "2 (M)", "3 (M)", "4 (M)",
                           "5 (M)", "6 (M)", "7 (M)", "8 (M)",
                           "1 (F)", "2 (F)", "3 (F)", "4 (F)",
                           "5 (F)", "6 (F)", "7 (F)", "8 (F)",
                           "Top Male", "Top Female",
                           "Male", "Female")
norway_beatlon_genders

gold_medals <- read_excel("gold_medals.xlsx")
gold_medals <- data.frame(gold_medals)
gold_medals

medal_places <- read_excel("medal_places.xlsx")
medal_places <- data.frame(medal_places)
medal_places

# ----- Setting up margins -----
par(mar = c(8, 4, 2, 1) + 0.1) # c(bottom, left, top, right)

# ----- Building barplot (Number of places in biathlon Norway) -----
plot_data <- as.matrix(norway_beatlon[, -1])
rownames(plot_data) <- norway_beatlon$Olympiad

barplot(t(plot_data),
        beside = TRUE,
        col = rainbow(8),
        main = "Number of places 1-8 for each Olympiad",
        ylab = "Number of places",
        xlab = "",
        legend.text = paste("Place", 1:8),
        args.legend = list(x = "topright", bty = "n", ncol = 2),
        las = 2,
        cex.names = 0.8)

title(xlab = "Olympiad", line = 5)
grid(nx = NA, ny = NULL, lty = 2, col = "gray")

# ----- Setting up margins -----
dev.off()

# ----- Building piechart (Number of first places in each Olympiad) -----
first_places <- norway_beatlon$`1`
olympiads <- norway_beatlon$Olympiad

nonzero_indices <- first_places > 0
filtered_olympiads <- olympiads[nonzero_indices]
filtered_counts <- first_places[nonzero_indices]

pie(filtered_counts, 
    labels = filtered_counts,
    col = rainbow(length(filtered_counts)),
    main = "Norway's First Place Finishes in Biathlon by Olympiad",
    cex.main = 1.2)

legend(x = "right", 
       legend = filtered_olympiads, 
       fill = rainbow(length(filtered_counts)),
       title = "Olympic Games",
       cex = 0.8,
       bty = "n")

# ----- Setting up margins -----
par(mar = c(10, 4, 2, 8) + 0.1) # c(bottom, left, top, right)

# ----- Building plot (Trends in number of prizes for men and women) -----
norway_beatlon_genders
norway_beatlon_genders_numeric <- norway_beatlon_genders[sapply(norway_beatlon_genders, is.numeric)]
norway_beatlon_genders_numeric

olympiads <- rev(norway_beatlon_genders$Olympiad)
male_counts <- rev(norway_beatlon_genders_numeric$`Top Male`)
female_counts <- rev(norway_beatlon_genders_numeric$`Top Female`)

plot(1, type = "n", 
     xlim = c(1, length(olympiads)), 
     ylim = range(c(male_counts, female_counts)),
     xlab = "",
     ylab = "Number of top-three finishes",
     main = "Trend of Norway's Biathlon Results by Gender (1992-2022)",
     xaxt = "n")

title(xlab = "Olympic Games", line = 8)
axis(1, at = 1:length(olympiads), labels = olympiads, las = 2, cex.axis = 0.8)

lines(male_counts, type = "o", col = "blue", pch = 16, lwd = 2)
lines(female_counts, type = "o", col = "red", pch = 16, lwd = 2)

legend(x = "topright", inset = c(-0.25, 0),
       legend = c("Male", "Female"),
       col = c("blue", "red"),
       lty = 1, pch = 16,
       title = "Gender",
       bty = "n",
       xpd = TRUE)

grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")