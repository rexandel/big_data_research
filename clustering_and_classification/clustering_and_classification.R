# ----- Connecting the necessary libraries -----
library(tidyverse)
library(ggplot2)
library(psych)
library(gridExtra)
library(factoextra)
library(cluster)
library(parameters)
library(dendextend)
library(scatterplot3d)
library(ClusterR)
library(plotly)

# ----- Setting up working directory -----
setwd("C:/Users/rexandel/Desktop/GitHub/big_data_research/clustering_and_classification//")
getwd()

# ----- Loading the initial data set -----
students <- read.csv("students_data.csv", sep = ",")

# ----- Getting metrics for descriptive data analysis -----
str(students)
summary(students)
head(students)

describe(students %>% select(age, Medu, Fedu, traveltime, studytime, failures, 
                             famrel, freetime, goout, Dalc, Walc, health, 
                             absences, G1, G2, G3))

# ----- Gaining basic knowledge about data distribution -----
table(students$school)
table(students$sex)
table(students$address)
table(students$famsize)
table(students$Pstatus)
table(students$Mjob)
table(students$Fjob)
table(students$reason)
table(students$guardian)
table(students$schoolsup)
table(students$famsup)
table(students$paid)
table(students$activities)
table(students$nursery)
table(students$higher)
table(students$internet)
table(students$romantic)

# ----- Boxplots -----

# 1. Боксплот оценок G1–G3
grades_long <- students %>% 
  select(G1, G2, G3) %>% 
  gather(key = "Period", value = "Grade")

boxplot(Grade ~ Period, data = grades_long,
        main = "Распределение оценок по периодам",
        xlab = "Период", ylab = "Оценка",
        col  = c("grey90", "grey70", "grey50"))

# 2. Боксплот оценок по полу
grades_sex_long <- students %>% 
  select(sex, G1, G2, G3) %>% 
  gather(key = "Period", value = "Grade", -sex)

boxplot(Grade ~ interaction(Period, sex), data = grades_sex_long,
        main = "Распределение оценок по полу",
        ylab = "Оценка",
        xlab = "Период:Пол",
        xaxt = "n",
        col  = rep(c("lightblue", "pink"), times = 3))
axis(1, at = 1:6,
     labels = c("G1:M", "G1:F", "G2:M", "G2:F", "G3:M", "G3:F"))

# 3. Боксплот социальных характеристик
social_long <- students %>% 
  select(famrel, Dalc, Walc) %>% 
  gather(key = "Variable", value = "Value")

boxplot(Value ~ Variable, data = social_long,
        main = "Социальные характеристики",
        xlab = "Переменная", ylab = "Значение",
        col  = "lightgreen")

# 4. Боксплот временных характеристик
demo_long <- students %>% 
  select(traveltime, studytime, freetime, goout) %>% 
  gather(key = "Variable", value = "Value")

boxplot(Value ~ Variable, data = demo_long,
        main = "Временные характеристики",
        xlab = "Переменная", ylab = "Значение",
        col  = "lightyellow")

# 5. Анализ корреляции между оценками
cor(students %>% select(G1, G2, G3), method = "pearson")

# ----- Preparing for clusterization -----
# Standardization of values
numeric_data_NO_SCALE <- students %>% 
  select(age, Medu, Fedu, traveltime, studytime, failures, 
         famrel, freetime, goout, Dalc, Walc, health, 
         absences, G1, G2, G3)

numeric_data <- students %>% 
  select(age, Medu, Fedu, traveltime, studytime, failures, 
         famrel, freetime, goout, Dalc, Walc, health, 
         absences, G1, G2, G3) %>%
  scale()

head(numeric_data)

# ----- Calculating the WSS -----
set.seed(42)

wss <- function(k) {
  kmeans(numeric_data, k, nstart = 10)$tot.withinss
}
# $tot.withinss - Суммарная внутрикластерная сумма квадратов

# WSS для k от 1 до 10
k.values <- 1:10
wss_values <- map_dbl(k.values, wss)

# ----- Calculating the required number of clusters -----

# Метод локтя
elbow_plot <- fviz_nbclust(numeric_data, kmeans, method = "wss", k.max = 10) + theme_minimal()

print(elbow_plot)

# Метод силуэта
silhouette_plot <- fviz_nbclust(numeric_data, kmeans, method = "silhouette", k.max = 10) + theme_minimal()

print(silhouette_plot)

# Статистика разрыва
gap_stat <- clusGap(numeric_data, FUN = kmeans, nstart = 25, K.max = 10, B = 100)
# nstart = 25 — при запуске k-means пробуем 25 случайных начальных центров
# B = 100 — генерируем 100 случайных (референсных) наборов данных для сравнения

print(gap_stat)

gap_plot <- fviz_gap_stat(gap_stat) + theme_minimal()

print(gap_plot)

# Алгоритм на основе консенсуса
n_clust <- n_clusters(numeric_data_NO_SCALE, package = c("easystats", "NbClust", "mclust"), fast = TRUE)

plot(n_clust)

# ----- Building a dendrogram -----
# Построение матрицы расстояний
dist_matrix <- dist(numeric_data_NO_SCALE, method = "euclidean")

# Иерархическая кластеризация (метод Варда)
hc <- hclust(dist_matrix, method = "ward.D2")

# Перевод в формат дендрограммы
dend <- as.dendrogram(hc)

# Настройка окна для двух графиков (2 строки, 1 колонка)
par(mfrow = c(2, 1), mar = c(4, 4, 4, 4))

# Дендрограмма с 2 кластерами
dend_2 <- color_branches(dend, k = 2, groupLabels = TRUE)
plot(dend_2,
     main = "Дендрограмма: разбиение на 2 кластера",
     xlab = "Объекты (наблюдения)", 
     ylab = "Высота",
     leaflab = "none")
rect.hclust(hc, k = 2, border = "red")

# Дендрограмма с 3 кластерами
dend_3 <- color_branches(dend, k = 3, groupLabels = TRUE)
plot(dend_3,
     main = "Дендрограмма: разбиение на 3 кластера",
     xlab = "Объекты (наблюдения)", 
     ylab = "Высота",
     leaflab = "none")
rect.hclust(hc, k = 3, border = c("red", "blue", "green"))

# Возвращаем стандартный режим
par(mfrow = c(1, 1))

# ----- Working with individual clusters -----
# Построение средних значений в каждом кластере
# GGPLOT
cluster_means_long <- cluster_means %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "mean_value")

ggplot(cluster_means_long, aes(x = variable, y = mean_value, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Средние значения переменных по кластерам",
       x = "Переменная",
       y = "Среднее значение") +
  theme_minimal()

# BASIC
# Преобразуем данные: убираем колонку с номером кластера и транспонируем
cluster_means_matrix <- as.matrix(t(cluster_means[,-1]))

# Подписи: строки — переменные, столбцы — кластеры
rownames(cluster_means_matrix) <- colnames(cluster_means)[-1]
colnames(cluster_means_matrix) <- paste("Кластер", cluster_means$cluster)

barplot(cluster_means_matrix,
        beside = TRUE,
        col = c("skyblue", "orange", "green"),
        legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n"),
        xlab = "Среднее значение",
        main = "Средние значения переменных по кластерам",
        cex.names = 0.8)

# Построение boxplot'ов
# 1. Итоговые оценки (G3) по кластерам
boxplot(G3 ~ cluster, data = students_clustered, 
        main = "Итоговые оценки (G3) по кластерам",
        xlab = "Кластер", ylab = "Оценка G3",
        col = rainbow(length(unique(students_clustered$cluster))))

# 2. Пропуски по кластерам
boxplot(absences ~ cluster, data = students_clustered,
        main = "Пропуски по кластерам",
        xlab = "Кластер", ylab = "Количество пропусков",
        col = rainbow(length(unique(students_clustered$cluster))))

# 3. Время учебы по кластерам
boxplot(studytime ~ cluster, data = students_clustered,
        main = "Время учебы по кластерам",
        xlab = "Кластер", ylab = "Время учебы",
        col = rainbow(length(unique(students_clustered$cluster))))

# 4. Время с друзьями по кластерам
boxplot(goout ~ cluster, data = students_clustered,
        main = "Время с друзьями по кластерам",
        xlab = "Кластер", ylab = "Время с друзьями",
        col = rainbow(length(unique(students_clustered$cluster))))

# 5. Употребление алкоголя в выходные по кластерам
boxplot(Walc ~ cluster, data = students_clustered,
        main = "Употребление алкоголя в выходные по кластерам",
        xlab = "Кластер", ylab = "Уровень потребления",
        col = rainbow(length(unique(students_clustered$cluster))))

# 6. Употребление алкоголя в будние дни по кластерам
boxplot(Dalc ~ cluster, data = students_clustered,
        main = "Употребление алкоголя в будние дни по кластерам",
        xlab = "Кластер", ylab = "Уровень потребления",
        col = rainbow(length(unique(students_clustered$cluster))))

# ----- Clustering a dataset using K-Means -----
### 1. Стандартный K-Means ###
set.seed(123)
km_standard <- kmeans(numeric_data, centers = 3, nstart = 25)

# Визуализация стандартного K-Means
p1 <- fviz_cluster(km_standard, data = numeric_data,
                   palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
                   geom = "point",
                   ellipse.type = "convex",
                   ggtheme = theme_minimal(),
                   main = "Стандартный K-Means")

# Добавляем кластеры к данным
students_standard <- students %>%
  mutate(cluster = as.factor(km_standard$cluster))

# Средние значения по кластерам
cluster_means_standard <- students_standard %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

### 2. K-Means++ ###
set.seed(123)
km_plusplus <- KMeans_rcpp(numeric_data, clusters = 3,
                           num_init = 25, initializer = "kmeans++")

# Визуализация K-Means++
p2 <- fviz_cluster(list(data = numeric_data, cluster = km_plusplus$clusters),
                   palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
                   geom = "point",
                   ellipse.type = "convex",
                   ggtheme = theme_minimal(),
                   main = "K-Means++")

# Добавляем кластеры к данным
students_plusplus <- students %>%
  mutate(cluster = as.factor(km_plusplus$clusters))

# Средние значения по кластерам
cluster_means_plusplus <- students_plusplus %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

### Сравнение результатов ###
# Выводим графики рядом
grid.arrange(p1, p2, ncol = 2)

# Сравниваем средние значения кластеров
cat("Стандартный K-Means - средние значения:\n")
print(cluster_means_standard)

cat("\nK-Means++ - средние значения:\n")
print(cluster_means_plusplus)

# ----- Building a scattering diagram -----
dev.off()
key_vars <- students_clustered %>%
  select(G1, G2, G3, absences, studytime, cluster)

pairs(key_vars[,1:5], col = key_vars$cluster, 
      pch = 19, cex = 0.6,
      main = "Матрица диаграмм рассеяния по кластерам")

# ----- Building a 3D scattering diagram -----
scatterplot3d(students_clustered$G1, 
              students_clustered$G2, 
              students_clustered$G3,
              color = as.numeric(students_clustered$cluster),
              pch = 19,
              main = "3D визуализация кластеров по оценкам",
              xlab = "G1",
              ylab = "G2",
              zlab = "G3")
legend("topright", legend = levels(students_clustered$cluster),
       col = 1:3, pch = 19)

# Создание интерактивного графика
plot_ly(students_clustered, 
        x = ~G1, y = ~G2, z = ~G3, 
        color = ~cluster, 
        colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 5, opacity = 0.8)) %>%
  layout(title = "3D визуализация кластеров по оценкам",
         scene = list(xaxis = list(title = "G1"),
                      yaxis = list(title = "G2"),
                      zaxis = list(title = "G3")))