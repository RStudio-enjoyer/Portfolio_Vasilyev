####Загрузка библиотек----------------------------------------------------------
library(lmtest)
library(ggplot2)
library(readxl)

#загружаем данные
data <- read_excel("C://Данные_ЦК.xlsx")

#создаем нужный фрейм данных для анализа 
new_data <- data.frame(Y1 = data$Y1, X1 = data$X1, X8 = data$X8, X9 = data$X9)
head(new_data)

###1 задание. Строим корр.матрицу--------------------------------------
corr_matrix <- cor(new_data)
print(corr_matrix)

###2 задание. Строим диаграммы рассеивания для анализа данных-------------------
ggplot(new_data, aes(x = X1, y = Y1)) + 
  geom_point() + 
  labs(title = "Зависимость производительности от трудоемкости единицы продукции") +
  geom_smooth(method = "lm")

ggplot(new_data, aes(x = X8, y = Y1)) + 
  geom_point() + 
  labs(title = "Зависимость производительности от удельного веса покупных изделий") +
  geom_smooth(method = "lm")

ggplot(new_data, aes(x = X9, y = Y1)) + 
  geom_point() + 
  labs(title = "Зависимость производительности от коэффициента сменности оборудования") +
    geom_smooth(method = "lm")

###3-6 задания. Регрессионная модель и другие показатели------------------------
model <- lm(Y1 ~ X1 + X8 + X9, data = new_data)
summary(model)

confint(model)

###7 Задание. Увеличение переменных и прогноз новой переменной------------------

#создаем модель
new_model <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = data)
summary(new_model)

#5% от среднего изменяемых переменных
new_X1 <- mean(data$X1) * 1.05
new_X2 <- mean(data$X2) * 1.05
new_X3 <- mean(data$X3) * 1.05
new_X4 <- mean(data$X4) * 1.05
new_X5 <- mean(data$X5) * 1.05
new_X6 <- mean(data$X6) * 1.05
new_X7 <- mean(data$X7) * 1.05
new_X8 <- mean(data$X8) * 1.05
new_X9 <- mean(data$X9) * 1.05


#создаем таблицу для всех переменных, увеличенных на 5%
new_variables = data.frame(X1 = new_X1, X2 = new_X2, X3 = new_X3, X4 = new_X4,
                           X5 = new_X5, X6 = new_X6, X7 = new_X7, X8 = new_X8,
                           X9 = new_X9)


#Расчет прогнозного значения результата
newresult <- predict(new_model, new_variables)


#округляем
newresult1 <- round(newresult, 3)
print(newresult1)
