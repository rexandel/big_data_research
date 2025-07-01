# ----- Connecting necessary libraries -----
library(readxl)

# ----- Setting up working directory -----
setwd("C:/Users/rexandel/Desktop/GitHub/big_data_research/olympiad_analysis")
getwd()

# ----- Importing data from Excel -----
# norway_beatlon - data on Norway's biathlon victories without gender division
norway_beatlon <- read_excel("norway_beatlon.xlsx") 
norway_beatlon <- data.frame(norway_beatlon)
names(norway_beatlon) <- c("Olympiad", 
                 "1", "2", "3", "4",
                 "5", "6", "7", "8")
norway_beatlon

# norway_beatlon_genders - data on Norway's biathlon victories with gender division
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

# gold_medals - data on gold medals of top 7 countries at last 6 Olympic Games
gold_medals <- read_excel("gold_medals.xlsx")
gold_medals <- data.frame(gold_medals)
gold_medals

# medal_places - data on medals of top 7 countries at last 6 Olympic Games
medal_places <- read_excel("medal_places.xlsx")
medal_places <- data.frame(medal_places)
medal_places

# ----- Setting up margins -----
par(mar = c(8, 4, 2, 6) + 0.1) # c(bottom, left, top, right)

# ----- Building Barplot (Number of places in biathlon Norway) -----
norway_beatlon_barplot <- as.matrix(norway_beatlon[, -1])
rownames(norway_beatlon_barplot) <- norway_beatlon$Olympiad

barplot(t(norway_beatlon_barplot),
        beside = TRUE,
        col = rainbow(8),
        ylim = c(0, max(norway_beatlon_barplot) + 1),
        yaxt = "n",
        main = "Number of places 1-8 for each Olympiad",
        ylab = "Number of places",
        xlab = "",
        las = 2,
        cex.names = 0.8)

legend("topright",
       inset = c(-0.15, 0),
       legend = paste("Place", 1:8),
       fill = rainbow(8),
       title = "Place",
       bty = "n",
       xpd = TRUE,
       cex = 0.8)

axis(2, at = -1:max(norway_beatlon_barplot) + 1, las = 2)
title(xlab = "Olympiad", line = 5)
grid(nx = NA, ny = NULL, lty = 2, col = "gray")

# ----- Setting up margins -----
par(mar = c(2, 2, 2, 2) + 0.1) # c(bottom, left, top, right)

# ----- Building Piechart (Number of first places in each Olympiad) -----
norway_beatlon
first_places <- norway_beatlon$`1`
olympiads <- norway_beatlon$Olympiad

nonzero_indices <- first_places > 0
filtered_olympiads <- olympiads[nonzero_indices]
filtered_counts <- first_places[nonzero_indices]

pie(filtered_counts, 
    labels = filtered_counts,
    col = rainbow(length(filtered_counts)),
    main = "Norway's First Place Finishes in Biathlon by Olympiad")

legend(x = "right",
       legend = filtered_olympiads, 
       fill = rainbow(length(filtered_counts)),
       title = "Olympic Games",
       bty = "n",
       cex = 0.8)

# ----- Setting up margins -----
par(mar = c(10, 4, 2, 8) + 0.1) # c(bottom, left, top, right)

# ----- Building Plot (Trends in number of prizes for men and women) -----
norway_beatlon_genders

olympiads <- norway_beatlon_genders$Olympiad
male_counts <- norway_beatlon_genders$`Top Male`
female_counts <- norway_beatlon_genders$`Top Female`

plot(1,
     type = "n", 
     xlim = c(1, length(olympiads)), 
     ylim = range(c(male_counts, female_counts)),
     xlab = "",
     ylab = "Number of top-three finishes",
     main = "Trend of Norway's Biathlon Results by Gender (1992-2022)",
     xaxt = "n")

lines(male_counts, type = "o", col = "blue", pch = 16, lwd = 2)
lines(female_counts, type = "o", col = "red", pch = 16, lwd = 2)

legend(x = "topright",
       inset = c(-0.25, 0),
       legend = c("Male", "Female"),
       col = c("blue", "red"),
       lty = 1, pch = 16,
       title = "Gender",
       bty = "n",
       xpd = TRUE)

title(xlab = "Olympic Games", line = 8)
axis(1, at = 1:length(olympiads), labels = olympiads, las = 2, cex.axis = 0.8)
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# ----- Setting up margins -----
par(mar = c(10, 4, 2, 9) + 0.1) # c(bottom, left, top, right)

# ----- Building Plot (Trends in sports achievements by gold medals) -----
gold_medals

colors <- c("Norway" = "blue", 
            "Germany" = "cadetblue",
            "USA" = "red",
            "Canada" = "purple",
            "Netherlands" = "orange",
            "Austria" = "darkgreen",
            "Switzerland" = "red4")

olympiads <- gold_medals$Olympiad
norway_counts <- gold_medals$Norway
germany_counts <- gold_medals$Germany
usa_counts <- gold_medals$USA
canada_counts <- gold_medals$Canada
netherlands_counts <- gold_medals$Netherlands
austia_counts <- gold_medals$Austria
switzerland_counts <- gold_medals$Switzerland

plot(1,
     type = "n", 
     xlim = c(1, length(olympiads)),
     ylim = c(0, max(gold_medals[, -1]) + 1),
     yaxt = "n",
     xaxt = "n",
     xlab = "",
     ylab = "Total number of gold medals",
     main = "Trends in sports achievements by gold medals (2002-2022)"
     )

lines(norway_counts, type = "o", col = colors["Norway"], pch = 16, lwd = 2)
lines(germany_counts, type = "o", col = colors["Germany"], pch = 16, lwd = 2)
lines(usa_counts, type = "o", col = colors["USA"], pch = 16, lwd = 2)
lines(canada_counts, type = "o", col = colors["Canada"], pch = 16, lwd = 2)
lines(netherlands_counts, type = "o", col = colors["Netherlands"], pch = 16, lwd = 2)
lines(austia_counts, type = "o", col = colors["Austria"], pch = 16, lwd = 2)
lines(switzerland_counts, type = "o", col = colors["Switzerland"], pch = 16, lwd = 2)

legend("right", 
       inset = c(-0.3, 0),
       legend = names(colors),
       col = colors,
       lty = 1,
       pch = 16,
       title = "Countries",
       bty = "n",
       xpd = TRUE,
       cex = 0.8)

title(xlab = "Olympic Games", line = 8)
axis(2, at = 0:max(gold_medals[, -1]) + 1, las = 2, cex.axis = 0.8)
axis(1, at = 1:length(olympiads), labels = olympiads, las = 2, cex.axis = 0.8)
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# ----- Building Plot (Trends in sports achievements by medals) -----
medal_places

colors <- c("Norway" = "blue", 
            "Germany" = "cadetblue",
            "USA" = "red",
            "Canada" = "purple",
            "Netherlands" = "orange",
            "Austria" = "darkgreen",
            "Switzerland" = "red4")

olympiads <- medal_places$Olympiad
norway_counts <- medal_places$Norway
germany_counts <- medal_places$Germany
usa_counts <- medal_places$USA
canada_counts <- medal_places$Canada
netherlands_counts <- medal_places$Netherlands
austia_counts <- medal_places$Austria
switzerland_counts <- medal_places$Switzerland

plot(1,
     type = "n", 
     xlim = c(1, length(olympiads)), 
     ylim = c(min(medal_places[, -1]), max(medal_places[, -1]) + 1),
     yaxt = "n",
     xaxt = "n",
     xlab = "",
     ylab = "Total number of medals",
     main = "Trends in sports achievements by medals (2002-2022)"
     )

lines(norway_counts, type = "o", col = colors["Norway"], pch = 16, lwd = 2)
lines(germany_counts, type = "o", col = colors["Germany"], pch = 16, lwd = 2)
lines(usa_counts, type = "o", col = colors["USA"], pch = 16, lwd = 2)
lines(canada_counts, type = "o", col = colors["Canada"], pch = 16, lwd = 2)
lines(netherlands_counts, type = "o", col = colors["Netherlands"], pch = 16, lwd = 2)
lines(austia_counts, type = "o", col = colors["Austria"], pch = 16, lwd = 2)
lines(switzerland_counts, type = "o", col = colors["Switzerland"], pch = 16, lwd = 2)

legend("right", 
       inset = c(-0.3, 0),
       legend = names(colors),
       col = colors,
       lty = 1,
       pch = 16,
       title = "Countries",
       bty = "n",
       xpd = TRUE,
       cex = 0.8)

title(xlab = "Olympic Games", line = 8)
axis(2, at = (min(medal_places[, -1]) - 1):max(medal_places[, -1]) + 1, las = 2, cex.axis = 0.8)
axis(1, at = 1:length(olympiads), labels = olympiads, las = 2, cex.axis = 0.8)
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# ----- Setting up graph layout & margins -----
dev.off()
par(mar = c(10, 2, 2, 8) + 0.1) # c(bottom, left, top, right)
par(mfrow = c(1, 2))

# ----- Building Barplots (Number of places in biathlon Norway by gender) -----
norway_beatlon_genders

# Male
norway_beatlon_genders_male <- as.matrix(cbind(norway_beatlon_genders$`1 (M)`,
                                               norway_beatlon_genders$`2 (M)`,
                                               norway_beatlon_genders$`3 (M)`))

rownames(norway_beatlon_genders_male) <- norway_beatlon_genders$Olympiad
colnames(norway_beatlon_genders_male) <- c(1, 2, 3)
norway_beatlon_genders_male

# Female
norway_beatlon_genders_female <- as.matrix(cbind(norway_beatlon_genders$`1 (F)`,
                                                 norway_beatlon_genders$`2 (F)`,
                                                 norway_beatlon_genders$`3 (F)`))

rownames(norway_beatlon_genders_female) <- norway_beatlon_genders$Olympiad
colnames(norway_beatlon_genders_female) <- c(1, 2, 3)
norway_beatlon_genders_female

# First Barplot (Male)
barplot(t(norway_beatlon_genders_male),
        beside = TRUE,
        col = rainbow(3),
        ylim = c(0, max(norway_beatlon_genders_female, norway_beatlon_genders_male) + 1),
        yaxt = "n",
        main = "Number of places 1-3 for each Olympiad (Male)",
        ylab = "Number of places",
        xlab = "",
        las = 2,
        cex.names = 0.8,
        cex.main = 0.9)

axis(2, at = -1:max(norway_beatlon_barplot) + 1, las = 2)
title(xlab = "Olympiad", line = 8)
grid(nx = NA, ny = NULL, lty = 2, col = "gray")


# Second Barplot (Female)
barplot(t(norway_beatlon_genders_female),
        beside = TRUE,
        col = rainbow(3),
        ylim = c(0, max(norway_beatlon_genders_female, norway_beatlon_genders_male) + 1),
        yaxt = "n",
        main = "Number of places 1-3 for each Olympiad (Female)",
        ylab = "Number of places",
        xlab = "",
        las = 2,
        cex.names = 0.8,
        cex.main = 0.9)

legend("topright",
       inset = c(-0.35, 0),
       legend = paste("Place", 1:3),
       fill = rainbow(3),
       title = "Place",
       bty = "n",
       xpd = TRUE,
       cex = 0.8)

axis(2, at = -1:max(norway_beatlon_barplot) + 1, las = 2)
title(xlab = "Olympiad", line = 8)
grid(nx = NA, ny = NULL, lty = 2, col = "gray")

# ----- Setting up graph layout & margins -----
dev.off()
par(mar = c(5, 2, 2, 8) + 0.1) # c(bottom, left, top, right)
par(mfrow = c(1, 2))

# ----- Building Piecharts (Norway's Price Place Finishes in Biathlon by Olympiad by gender -----
norway_beatlon_genders

olympiads <- norway_beatlon_genders$Olympiad

# Male
norway_beatlon_genders_top_male <- norway_beatlon_genders$`Top Male`
nonzero_indices_male <- norway_beatlon_genders_top_male > 0
filtered_olympiads_male <- olympiads[nonzero_indices_male]
filtered_counts_male <- norway_beatlon_genders_top_male[nonzero_indices_male]

# Female
norway_beatlon_genders_top_female <- norway_beatlon_genders$`Top Female`
nonzero_indices_female <- norway_beatlon_genders_top_female > 0
filtered_olympiads_female <- olympiads[nonzero_indices_female]
filtered_counts_female <- norway_beatlon_genders_top_female[nonzero_indices_female]

# Male PieChart
pie(filtered_counts_male, 
    labels = filtered_counts_male,
    col = rainbow(length(olympiads))[nonzero_indices_male],
    main = "Norway's Price Place Finishes in Biathlon by Olympiad (Male)",
    cex.main = 0.8)

# Female PieChart
pie(filtered_counts_female,
    labels = filtered_counts_female,
    col = rainbow(length(olympiads))[nonzero_indices_female],
    main = "Norway's Price Place Finishes in Biathlon by Olympiad (Female)",
    cex.main = 0.8)

# Legend (Female)
legend("right",
       inset = c(-0.3, 0),
       legend = olympiads,
       fill = rainbow(length(olympiads)),
       title = "Olympic Games",
       bty = "n",
       cex = 0.7,
       ncol = 1,
       xpd = TRUE)