#устанавливаем библиотеки, которые будут нужны при дальнейшей работе
library(dplyr)
library(data.table)
library(ggplot2)
library(readr)

#читаем csv файл
IQ <- fread('C:\\avgIQpercountry.csv')

#для простоты использования данных прикрепляем столбики файла
attach(IQ)

#График 1
ggplot(IQ, aes(x = `Average IQ`, y = `Literacy Rate`, color = Continent)) +
  geom_point() +
  labs(title = "Связь между средним IQ и уровнем грамотности",
       x = "Средний IQ",
       y = "Уровень грамотности")
#График 2
ggplot(IQ, aes(x = Continent, y = `Nobel Prices`, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Число нобелевских призов по континентам",
       x = "Континент",
       y = "Число нобелевских призов")

#График 3
ggplot(IQ, aes(x = `Mean years of schooling - 2021`, y = `GNI - 2021`, color = Continent)) +
  geom_point() +
  labs(title = "Связь между уровнем образования и ВВП",
       x = "Среднее время обучения",
       y = "ВВП")

#График 4
ggplot(IQ, aes(x = Continent, y = `Mean years of schooling - 2021`)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Распределение среднего времени обучения по континентам",
       x = "Континент",
       y = "Среднее время обучения")

#График 5
IQ_grouped <- IQ %>%
  mutate(`Population - 2023` = as.numeric(`Population - 2023`)) %>%
  group_by(Continent) %>%
  summarise(`Population - 2023` = sum(`Population - 2023`)) %>%
  mutate(Continent = factor(Continent, levels = unique(Continent)))

ggplot(IQ_grouped, aes(x = "", y = `Population - 2023`, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(`Population - 2023` / 1e6), "M")), 
            position = position_stack(vjust = 0.5), size = 3.5) +
  coord_polar(theta = "y") +
  labs(title = "Соотношение населения по континентам",
       x = NULL,
       y = NULL) +
  theme_void()

#Вычисляем основные характеристики данных без разделения на группу

IQ_stat <- sapply(IQ[, c("Population - 2023", "Mean years of schooling - 2021",
                        "Literacy Rate")], function(x) {
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
    c(
      mean = NA,
      median = NA,
      sd = NA,
      var = NA,
      min = NA,
      max = NA,
      range = NA
    )
  }
})

IQ_num <- as.data.frame(IQ_stat)
print(IQ_num)

#Вычисляем основные характеристики данных с разделением на группу

IQ_stat_grouped <- sapply(IQ[, c("Nobel Prices", "Mean years of schooling - 2021",
                        "Average IQ")], function(x) {
  if (is.numeric(x)) {
    tapply(x, Continent, function(y) {
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

IQ_grouped_stat1 <- as.data.frame(IQ_stat_grouped)
print(IQ_grouped_stat1)


#Делаем группировку по одной качественной данной
grouped_data <- IQ %>%
  group_by(Continent) %>%
  summarize(mean(`Average IQ`))
grouped_data

#Делаем группировку по двум качественным данным
grouped_data1 <- IQ %>%
  group_by(Continent, Country) %>%
  summarize(mean(`Average IQ`))
grouped_data1