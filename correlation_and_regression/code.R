# ----- Install packages -----
install.packages("GGally", dependencies = TRUE)
install.packages("car", dependencies = TRUE)

# ----- Setting up working directory -----
setwd("C:/Users/rexandel/Desktop/GitHub/big_data_research/correlation_and_regression/")
getwd()

# ----- Connecting the necessary libraries -----
library(car)
library(GGally)
library(readr)

# ----- Initial data -----
data <- read_delim("serbia.csv", delim = ",", na = c("..", ""), locale = locale(encoding = "latin1"))

# Чтение данных
data <- read.csv("serbia.csv", sep = ",",  header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
years <- 1989:2023

# 1. График прироста ВВП
gdp_growth <- data[data$Series.Code == "NY.GDP.MKTP.KD.ZG", ]

gdp_values <- as.numeric(gdp_growth[5:39]) 

valid_years <- years[!is.na(gdp_values)]
valid_values <- gdp_values[!is.na(gdp_values)]

year_range <- range(valid_years)  # автоматически определим диапазон
axis_ticks <- seq(floor(year_range[1]/2)*2, ceiling(year_range[2]/2)*2, by = 2)

plot(valid_years, valid_values, 
     type = "o",
     col = "blue", 
     lwd = 2,
     main = "Рост ВВП Сербии (годовой %, 1996–2023)",
     xlab = "Год",
     ylab = "Рост ВВП (%)",
     ylim = c(min(valid_values, na.rm = TRUE) - 2, max(valid_values, na.rm = TRUE) + 2),
     xaxt = "n")
axis(1, at = axis_ticks, las = 2, cex.axis = 0.8)
abline(h = 0, col = "red", lty = 2)

# 2. График прироста населения
# Filter data for population growth
pop_growth <- data[data$Series.Code == "SP.DYN.CBRT.IN", ]

# Extract population growth values
pop_values <- as.numeric(pop_growth[5:39])

# Filter out NA values and corresponding years
valid_years <- years[!is.na(pop_values)]
valid_pop_values <- pop_values[!is.na(pop_values)]

year_range <- range(valid_years)  # автоматически определим диапазон
axis_ticks <- seq(floor(year_range[1]/2)*2, ceiling(year_range[2]/2)*2, by = 2)

# Create the population growth plot
plot(valid_years, valid_pop_values, 
     type = "o",
     col = "green", 
     lwd = 2,
     main = "Рост населения Сербии (рождаемость на 1000 человек, 1995–2023)",
     xlab = "Год",
     ylab = "Рождаемость (на 1000 человек)",
     ylim = c(min(valid_pop_values, na.rm = TRUE) - 2, max(valid_pop_values, na.rm = TRUE) + 2),
     xaxt = "n")
axis(1, at = axis_ticks, las = 2, cex.axis = 0.8)
abline(h = mean(valid_pop_values, na.rm = TRUE), col = "red", lty = 2)

# 3. График прироста ВВА и график прироста населения вместе
# Filter data for GDP growth
gdp_growth <- data[data$Series.Code == "NY.GDP.MKTP.KD.ZG", ]
gdp_values <- as.numeric(gdp_growth[5:39])
valid_years_gdp <- years[!is.na(gdp_values)]
valid_gdp_values <- gdp_values[!is.na(gdp_values)]

# Filter data for population growth
pop_growth <- data[data$Series.Code == "SP.DYN.CBRT.IN", ]
pop_values <- as.numeric(pop_growth[5:39])
valid_years_pop <- years[!is.na(pop_values)]
valid_pop_values <- pop_values[!is.na(pop_values)]

# Ensure the years align
common_years <- intersect(valid_years_gdp, valid_years_pop)
gdp_subset <- valid_gdp_values[valid_years_gdp %in% common_years]
pop_subset <- valid_pop_values[valid_years_pop %in% common_years]

# Create the plot with dual axes
par(mar = c(5, 4, 4, 4) + 0.1)
plot(common_years, gdp_subset, 
     type = "o",
     col = "blue", 
     lwd = 2,
     main = "Рост ВВП и рождаемость в Сербии (1995–2023)",
     xlab = "Год",
     ylab = "Рост ВВП (%)",
     ylim = c(min(gdp_subset, na.rm = TRUE) - 2, max(gdp_subset, na.rm = TRUE) + 2),
     xaxt = "n")
axis(1, at = seq(1995, 2025, by = 2), las = 2)
abline(h = 0, col = "red", lty = 2)

# Add second y-axis for population growth
par(new = TRUE)
plot(common_years, pop_subset, 
     type = "o",
     col = "green", 
     lwd = 2,
     xaxt = "n", 
     yaxt = "n", 
     xlab = "", 
     ylab = "",
     ylim = c(min(pop_subset, na.rm = TRUE) - 2, max(pop_subset, na.rm = TRUE) + 2))
axis(4, at = seq(floor(min(pop_subset)), ceiling(max(pop_subset)), by = 1), las = 2)
mtext("Рождаемость (на 1000 человек)", side = 4, line = 3)

# Add legend
legend("topleft", 
       legend = c("Рост ВВП (%)", "Рождаемость (на 1000 чел.)"),
       col = c("blue", "green"),
       lwd = 2,
       pch = 1,
       bty = "n")

# GDP_Growth
# Годовой процент роста ВВП по рыночным ценам в постоянной местной валюте. Данные приведены в долларах США 2010 года.

# Pop_Growth
# Годовой прирост населения.

# Unemployment_Basic
# Уровень безработицы в % по всем гражданам, имеющим базовое образование.

# Health_Expenditure
# Государственные расходы на здравоохранение (% от ВВП).

# Life_Expectancy
# Средняя продолжительность жизни.

# Death_Rate
# Уровень смертности на 1000 человек.

# Higher_Education
# Доля населения с высшим образованием (степень бакалавра или эквивалент)

# Export_Growth
# Экспорт товаров и услуг – ежегодный прирост в %.

# Education_Expenditure
# Расходы на образование в % от ВВП.

# Female_Bachelors
# Кумулятивный прирост бакалавров среди женщин в % (накопительный показатель).

# Scientific_Articles
# Статей  в научных и технических журналах.

# 4. Визуализация корреляции
library(ellipse)
df <- data.frame(
  GDP_Growth = as.numeric(data[data$Series.Code == "NY.GDP.MKTP.KD.ZG", 5:39]),
  Pop_Growth = as.numeric(data[data$Series.Code == "SP.POP.GROW", 5:39]),
  Unemployment_Basic = as.numeric(data[data$Series.Code == "SL.UEM.BASC.ZS", 5:39]),
  Health_Expenditure = as.numeric(data[data$Series.Code == "SH.XPD.CHEX.GD.ZS", 5:39]),
  Life_Expectancy = as.numeric(data[data$Series.Code == "SP.DYN.LE00.IN", 5:39]),
  Death_Rate = as.numeric(data[data$Series.Code == "SP.DYN.CDRT.IN", 5:39]),
  Higher_Education = as.numeric(data[data$Series.Code == "SE.TER.CUAT.BA.ZS", 5:39]),
  Export_Growth = as.numeric(data[data$Series.Code == "NE.EXP.GNFS.KD.ZG", 5:39]),
  Education_Expenditure = as.numeric(data[data$Series.Code == "SE.XPD.TOTL.GD.ZS", 5:39]),
  Female_Bachelors = as.numeric(data[data$Series.Code == "SE.TER.CUAT.BA.FE.ZS", 5:39]),
  Scientific_Articles = as.numeric(data[data$Series.Code == "IP.JRN.ARTC.SC", 5:39])
)

# ----- Working with NA values (Interpolation) -----
data_inter <- df
# Функция для интерполяции NA значений
interpolate_na <- function(x) {
  if(all(is.na(x))) return(x)
  if (sum(!is.na(x)) < 2) {
    warning(paste("Слишком мало данных для интерполяции в столбце. Пропуск."))
    return(x)
  }
  
  # Линейная интерполяция
  approx(1:length(x), x, xout = 1:length(x), method = "linear", rule = 2)$y
}

# Применяем интерполяцию ко всем столбцам
df_interpolate <- as.data.frame(lapply(data_inter, interpolate_na))

# ----- Working with NA values (Lazy) -----
data_lazy <- df
# Замена средним по столбцу
df_filled <- as.data.frame(lapply(data_lazy, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}))

# Или медианой
df_filled <- as.data.frame(lapply(data_lazy, function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
}))

# --- Interpolate ---
# Вычисление корреляционной матрицы (метод Спирмена)
cor_matrix <- cor(df_interpolate, method = "spearman")
# Построение графика корреляции
plotcorr(cor_matrix)

# --- Lazy ---
# Вычисление корреляционной матрицы (метод Спирмена)
cor_matrix <- cor(df_filled, method = "spearman")
# Построение графика корреляции
plotcorr(cor_matrix)

# ==============================================================================
# Scatter plot для связи роста ВВП и прироста населения 
# --- Interpolate ---
plot(df_interpolate$GDP_Growth, df_filled$Pop_Growth,
     main = "Связь роста ВВП и прироста населения",
     xlab = "Рост ВВП (%)",
     ylab = "Прирост населения (%)",
     pch = 19,
     col = "blue")
# Линия тренда
abline(lm(Pop_Growth ~ GDP_Growth, data = df_filled), col = "red")

# --- Lazy ---
plot(df_filled$GDP_Growth, df_filled$Pop_Growth,
     main = "Связь роста ВВП и прироста населения",
     xlab = "Рост ВВП (%)",
     ylab = "Прирост населения (%)",
     pch = 19,
     col = "blue")
# Линия тренда
abline(lm(Pop_Growth ~ GDP_Growth, data = df_filled), col = "red")


# Scatter plot для связи высшего образования и научных статей
# --- Interpolate ---
plot(df_interpolate$Higher_Education, df_interpolate$Scientific_Articles,
     main = "Связь доли населения с высшим образованием\nи количества научных статей",
     xlab = "Доля населения с высшим образованием, 25+ лет (%)",
     ylab = "Количество научных статей",
     pch = 19,
     col = "darkgreen",
     cex = 1.2)
# Линия тренда
abline(lm(Scientific_Articles ~ Higher_Education, data = df_interpolate), 
       col = "red", lwd = 2)
# --- Lazy ---
plot(df_filled$Higher_Education, df_interpolate$Scientific_Articles,
     main = "Связь доли населения с высшим образованием\nи количества научных статей",
     xlab = "Доля населения с высшим образованием, 25+ лет (%)",
     ylab = "Количество научных статей",
     pch = 19,
     col = "darkgreen",
     cex = 1.2)
# Линия тренда
abline(lm(Scientific_Articles ~ Higher_Education, data = df_interpolate), 
       col = "red", lwd = 2)

# ==============================================================================

# 1. Корреляция роста ВВП и прироста населения
# --- Interpolate ---
cor(df_interpolate$GDP_Growth, df_interpolate$Pop_Growth, method = "spearman")
cor.test(df_interpolate$GDP_Growth, df_interpolate$Pop_Growth, method = "spearman")
# --- Lazy ---
cor(df_filled$GDP_Growth, df_filled$Pop_Growth, method = "spearman")
cor.test(df_filled$GDP_Growth, df_filled$Pop_Growth, method = "spearman")

# ==============================================================================

# 2. Прирост населения и динамика безработицы (базовое образование)
# --- Interpolate ---
cor(df_interpolate$Unemployment_Basic, df_interpolate$Pop_Growth, method = "spearman")
cor.test(df_interpolate$Unemployment_Basic, df_interpolate$Pop_Growth, method = "spearman")
# --- Lazy ---
cor(df_filled$Unemployment_Basic, df_filled$Pop_Growth, method = "spearman")
cor.test(df_filled$Unemployment_Basic, df_filled$Pop_Growth, method = "spearman")

# ==============================================================================

# 3. Расходы на медицину и продолжительность жизни/смертность
# 3.1. Медицина и продолжительность жизни
# --- Interpolate ---
cor(df_interpolate$Health_Expenditure, df_interpolate$Life_Expectancy, method = "spearman")
cor.test(df_interpolate$Health_Expenditure, df_interpolate$Life_Expectancy, method = "spearman")
# --- Lazy ---
cor(df_filled$Health_Expenditure, df_filled$Life_Expectancy, method = "spearman")
cor.test(df_filled$Health_Expenditure, df_filled$Life_Expectancy, method = "spearman")

# 3.2. Медицина и смертность
# --- Interpolate ---
cor(df_interpolate$Health_Expenditure, df_interpolate$Death_Rate, method = "spearman")
cor.test(df_interpolate$Health_Expenditure, df_interpolate$Death_Rate, method = "spearman")
# --- Lazy ---
cor(df_filled$Health_Expenditure, df_filled$Death_Rate, method = "spearman")
cor.test(df_filled$Health_Expenditure, df_filled$Death_Rate, method = "spearman")

# ==============================================================================

# 4. Высшее образование и экспорт/высокотехнологичное производство
# 4.1. Образование и экспорт
# --- Interpolate ---
cor(df_interpolate$Higher_Education, df_interpolate$Export_Growth, method = "spearman")
cor.test(df_interpolate$Higher_Education, df_interpolate$Export_Growth, method = "spearman")
# --- Lazy ---
cor(df_filled$Higher_Education, df_filled$Export_Growth, method = "spearman")
cor.test(df_filled$Higher_Education, df_filled$Export_Growth, method = "spearman")

# 4.2. Образование и высокие технологии
# --- Interpolate ---
cor(df_interpolate$Higher_Education, df_interpolate$High_Tech_Exports, method = "spearman")
cor.test(df_interpolate$Higher_Education, df_interpolate$High_Tech_Exports, method = "spearman")
# --- Lazy ---
cor(df_filled$Higher_Education, df_filled$High_Tech_Exports, method = "spearman")
cor.test(df_filled$Higher_Education, df_filled$High_Tech_Exports, method = "spearman")

# ==============================================================================

# 5. Расходы на образование и бакалавры среди женщин
# --- Interpolate ---
cor(df_interpolate$Education_Expenditure, df_interpolate$Female_Bachelors, method = "spearman")
cor.test(df_interpolate$Education_Expenditure, df_interpolate$Female_Bachelors, method = "spearman")
# --- Lazy ---
cor(df_filled$Education_Expenditure, df_filled$Female_Bachelors, method = "spearman")
cor.test(df_filled$Education_Expenditure, df_filled$Female_Bachelors, method = "spearman")

# ==============================================================================

# 6. Высшее образование и научные статьи в журналах
# --- Interpolate ---
cor(df_interpolate$Higher_Education, df_interpolate$Scientific_Articles, method = "spearman")
cor.test(df_interpolate$Higher_Education, df_interpolate$Scientific_Articles, method = "spearman")
# --- Lazy ---
cor(df_filled$Higher_Education, df_filled$Scientific_Articles, method = "spearman")
cor.test(df_filled$Higher_Education, df_filled$Scientific_Articles, method = "spearman")

# ==============================================================================

# 5. Построение матрицы корреляций 
# --- Interpolate ---
ggpairs(df_interpolate,
        upper = list(continuous = wrap("cor", method = "spearman")),
        title = "Матрица корреляций (метод Спирмена) (Интерполяция)"
)
# --- Lazy ---
ggpairs(df_filled,
        upper = list(continuous = wrap("cor", method = "spearman")),
        title = "Матрица корреляций (метод Спирмена) (Замена на среднее/медиану)"
)

# ==============================================================================

# 6. Регрессия
# lm - множественная линейная регрессия

# Зависимая переменная (выходная):
#  Export_Growth – ежегодный прирост экспорта товаров и услуг (%).

# Независимые переменные (входные):
#  GDP_Growth – годовой процент роста ВВП (%);
#  Health_Expenditure – госрасходы на здравоохранение (% от ВВП).

# --- Interpolate ---
fit_export_int <- lm(Export_Growth ~ GDP_Growth + Health_Expenditure, data=df_interpolate)
fit_export_int
summary(fit_export_int)

# --- Lazy ---
fit_export <- lm(Export_Growth ~ GDP_Growth + Health_Expenditure, data=df_filled)
fit_export
summary(fit_export)

# ==============================================================================

# 7. Предсказание модели
# --- Interpolate ---
predictions <- predict(fit_export_int)
df_interpolate$Predicted_Export <- predict(fit_export_int, newdata = df_interpolate)

years <- 1995:(1995 + nrow(df_interpolate) - 1)

plot(years, df_interpolate$Export_Growth, 
     col = "blue", pch = 19, 
     xlab = "Год", ylab = "Прирост экспорта",
     main = "Фактический и предсказанный прирост экспорта")
points(years, df_interpolate$Predicted_Export, 
       col = "red", pch = 19)
lines(years, df_interpolate$Export_Growth, 
      col = "blue")
lines(years, df_interpolate$Predicted_Export, 
      col = "red")
legend("bottomright", 
       legend = c("Фактические", "Предсказанные"),
       col = c("blue", "red"), pch = c(19, 19))

# -- Lazy ---
predictions <- predict(fit_export)
df_filled$Predicted_Export <- predict(fit_export, newdata = df_filled)

years <- 1995:(1995 + nrow(df_interpolate) - 1)

plot(years, df_filled$Export_Growth, 
     col = "blue", pch = 19, 
     xlab = "Год", ylab = "Прирост экспорта",
     main = "Фактический и предсказанный прирост экспорта")
points(years, df_filled$Predicted_Export, 
       col = "red", pch = 19)
lines(years, df_filled$Export_Growth, 
      col = "blue")
lines(years, df_filled$Predicted_Export, 
      col = "red")
legend("bottomright", 
       legend = c("Фактические", "Предсказанные"),
       col = c("blue", "red"), pch = c(19, 19))
