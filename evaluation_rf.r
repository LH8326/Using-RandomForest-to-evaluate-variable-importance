data <- read.csv(file.choose("data_name"))
summary(data)
str(data)
data <- subset(data, select = -c(employstatus_1, employstatus_2, employstatus_3, employstatus_4, employstatus_5, employstatus_6, employstatus_7, employstatus_8, employstatus_9, employstatus_10, rankOrdLife_1, rankOrdLife_2, rankOrdLife_3, rankOrdLife_4, rankOrdLife_5, rankOrdLife_6, coded_country, c19ProSo02, c19ProSo03, c19ProSo04))
str(data)
summary(data)
library(randomForest)
set.seed(71)
# replace NAs with column medians
for (i in 1:ncol(data)) {
  data[, i][is.na(data[, i])] <- median(data[, i], na.rm = TRUE)
}
rf <- randomForest(data$c19ProSo01 ~ ., data = data, ntree = 500)
print(rf)
floor(sqrt(ncol(data) - 1))
mtry <- tuneRF(data[-1], data$c19ProSo01,
  ntreeTry = 500,
  stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE
)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
set.seed(71)
rf <- randomForest(data$c19ProSo01 ~ ., data = data, mtry = best.m, importance = TRUE, ntree = 500)
print(rf)
# Evaluate variable importance
importance(rf)
varImpPlot(rf)
## 2nd c19proso02
data <- subset(data, select = -c(employstatus_1, employstatus_2, employstatus_3, employstatus_4, employstatus_5, employstatus_6, employstatus_7, employstatus_8, employstatus_9, employstatus_10, rankOrdLife_1, rankOrdLife_2, rankOrdLife_3, rankOrdLife_4, rankOrdLife_5, rankOrdLife_6, coded_country, c19ProSo01, c19ProSo03, c19ProSo04))
str(data)
library(randomForest)
set.seed(71)
# replace NAs with column medians
for (i in 1:ncol(data)) {
  data[, i][is.na(data[, i])] <- median(data[, i], na.rm = TRUE)
}
rf <- randomForest(data$c19ProSo02 ~ ., data = data, ntree = 500)
print(rf)
floor(sqrt(ncol(data) - 1))
mtry <- tuneRF(data[-1], data$c19ProSo02,
  ntreeTry = 500,
  stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE
)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
set.seed(71)
rf <- randomForest(data$c19ProSo02 ~ ., data = data, mtry = best.m, importance = TRUE, ntree = 500)
print(rf)
# Evaluate variable importance
importance(rf)
varImpPlot(rf)
## c19proSo03
data <- subset(data, select = -c(employstatus_1, employstatus_2, employstatus_3, employstatus_4, employstatus_5, employstatus_6, employstatus_7, employstatus_8, employstatus_9, employstatus_10, rankOrdLife_1, rankOrdLife_2, rankOrdLife_3, rankOrdLife_4, rankOrdLife_5, rankOrdLife_6, coded_country, c19ProSo02, c19ProSo01, c19ProSo04))
str(data)
library(randomForest)
set.seed(71)
# replace NAs with column medians
for (i in 1:ncol(data)) {
  data[, i][is.na(data[, i])] <- median(data[, i], na.rm = TRUE)
}
rf <- randomForest(data$c19ProSo03 ~ ., data = data, ntree = 500)
print(rf)
floor(sqrt(ncol(data) - 1))
mtry <- tuneRF(data[-1], data$c19ProSo03,
  ntreeTry = 500,
  stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE
)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
set.seed(71)
rf <- randomForest(data$c19ProSo03 ~ ., data = data, mtry = best.m, importance = TRUE, ntree = 500)
print(rf)
# Evaluate variable importance
importance(rf)
varImpPlot(rf)
## c19proSo04
data <- subset(data, select = -c(employstatus_1, employstatus_2, employstatus_3, employstatus_4, employstatus_5, employstatus_6, employstatus_7, employstatus_8, employstatus_9, employstatus_10, rankOrdLife_1, rankOrdLife_2, rankOrdLife_3, rankOrdLife_4, rankOrdLife_5, rankOrdLife_6, coded_country, c19ProSo02, c19ProSo03, c19ProSo01))
str(data)
library(randomForest)
set.seed(71)
# replace NAs with column medians
for (i in 1:ncol(data)) {
  data[, i][is.na(data[, i])] <- median(data[, i], na.rm = TRUE)
}
rf <- randomForest(data$c19ProSo04 ~ ., data = data, ntree = 500)
print(rf)
floor(sqrt(ncol(data) - 1))
mtry <- tuneRF(data[-1], data$c19ProSo04,
  ntreeTry = 500,
  stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE
)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
set.seed(71)
rf <- randomForest(data$c19ProSo04 ~ ., data = data, mtry = best.m, importance = TRUE, ntree = 500)
print(rf)
# Evaluate variable importance
importance(rf)
varImpPlot(rf)
