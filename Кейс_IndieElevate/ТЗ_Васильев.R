####Загружаем библиотеки
library(readxl)
library(tidyverse)
library(ggplot2)
library(DescTools)


####Читаем файл
new_data <- read.csv('C://data.csv')
new_data

###статистика датасета
summary(new_data)

###строим ящик с усами
boxplot(new_data$Age)

###строим гистограмму
hist(new_data$Age,
     xlab = "возраст",
     main = "Гистограмма возраста")


###выносим выбросы в новую переменную
ind <- which(new_data$Age %in% boxplot.stats(new_data$Age)$out)

new_data[ind, ]


###просматриваем интервалы (нижний и верхний)
lower_bound <- quantile(new_data$Age, 0.01, na.rm = T)
lower_bound

upper_bound <- quantile(new_data$Age, 0.99, na.rm = T)
upper_bound


###311 ошибка, которая может повлиять на окончательный график
new_data1 <- new_data[-311, ]
new_data1

###есть ли значения NA
is.na(new_data1)

###Один из способов замены значений NA - подстановка среднего всех данных
new_data1$Age[is.na(new_data1$Age)] <- mean(new_data1$Age, na.rm = T)
new_data1$Age <- round(new_data1$Age, digits = 0)
new_data1


###статистика измененного датасета
summary(new_data1)

###Вычисляем основные характеристики данных без разделения на группу
Age_stat <- sapply(new_data1[, c("Age", "applicant_name")], function(x) {
  if (is.numeric(x)) {
    c(
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      var = var(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      range = diff(range(x, na.rm = TRUE))
    )
  } else {
    list()
  }
})
##удаляем имя кандидата
Age_filtered <- Filter(function(x) length(x) > 0, Age_stat)

##формируем датасет из основных характеристик и выводим результат
Age_num <- as.data.frame(Age_filtered)
print(Age_num)

###Вычисляем основные характеристики данных с разделением на группу
Cont_stat_grouped <- sapply(new_data1[, c("Age", "applicant_name")], function(x) {
  if (is.numeric(x)) {
    tapply(x, new_data1$Ответ.на.вопрос, function(y) {
      c(
        mean = mean(y, na.rm = TRUE),
        median = median(y, na.rm = TRUE),
        sd = sd(y, na.rm = TRUE),
        var = var(y, na.rm = TRUE),
        min = min(y, na.rm = TRUE),
        max = max(y, na.rm = TRUE),
        range = diff(range(y, na.rm = TRUE))
      )
    })
  } else {
    list()
  }
})

##соединяем все подсписки возраста в таблицу
Cont_stat_grouped_df <- do.call(rbind, Cont_stat_grouped$Age)

##Преобразуем в data.frame и транспонируем
Cont_stat_grouped_df <- as.data.frame(t(Cont_stat_grouped_df))

## Устанавливаем названия строк как имена столбцов и выводим результат
colnames(Cont_stat_grouped_df) <- names(Cont_stat_grouped$Age)

print(Cont_stat_grouped_df)


####Строим ящик с усами: возраст ~ ответ на вопрос
age_plot <- ggplot(new_data1, aes(x = `Ответ.на.вопрос`, y = Age)) +
  geom_boxplot(fill = 'orange') +
  labs(title = 'Ящик с усами для возраста по ответу на вопрос',
       x = 'Ответ на вопрос', y = 'Возраст')
age_plot


###Рассчитаем средний возраст и доверительные интервалы
data_summary <- new_data1 %>%
  group_by(`Ответ.на.вопрос`) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    ymin = mean_age - qt(0.975, df = n() - 1) * sd(Age, na.rm = TRUE) / sqrt(n()),
    ymax = mean_age + qt(0.975, df = n() - 1) * sd(Age, na.rm = TRUE) / sqrt(n())
  )

##Строим график с доверительными интервалами
age_geom <- ggplot(data_summary, aes(x = `Ответ.на.вопрос`, y = mean_age)) +
  geom_point(size = 3, position = position_dodge(0.9)) +  # Точки для среднего значения
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, position = position_dodge(0.9)) +
  labs(
    title = 'Доверительные интервалы для возраста по ответам на вопрос',
    x = 'Ответ на вопрос', 
    y = 'Средний возраст'
  ) +
  theme_minimal()

age_geom
