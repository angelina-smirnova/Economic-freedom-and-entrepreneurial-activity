library("AER") 
library("lmtest")
library("car")
#library("openintro")
#library("OIdata")
#library("gdata")
#library("doBy")
#library("ivreg") 

library(nnet)
library(oglmx)
library(erer)

library(DescTools)
library(caret)
library(lares)
library(pscl)

library(lmtest)
library(effects)
library(dplyr)
library(lava)
library("plm")
library("stargazer") 
library("ggplot2")
library("sandwich")
library(corrplot)
library(sjPlot)
library(FactoMineR)
library(factoextra)
library(readxl)
library(sandwich)
library(modelsummary)
library(tidyverse)
library(gt)
library (lmtest)
library (MASS)


#загружаем данные
freedom.scores <- read.csv("C:/Users/gelia/Desktop/freedom-scores.csv")
#View(freedom.scores)

gem <- read.csv2("C:/Users/gelia/Desktop/gem.csv")
#View(gem)
c <-  unique(gem$economy)
length(c)

#убираем лишние переменныe
gem_all <- data.frame(gem$`economy`, gem$`year`, gem$`Entrepreneurial.intentions`, gem$`Total.early.stage.Entrepreneurial.Activity..TEA.`, gem$`Established.Business.Ownership`)
names(gem_all) <- c("economy", "year", "Entrepreneurial.intentions",
                      "Total.early.stage.Entrepreneurial.Activity", "Established.Business.Ownership")
#View(gem_all)

#объединяем данные в один датасет
data <- merge(freedom.scores, gem_all, by=c("economy", "year"))
#View(data)

#убираем столбцы ненужные для анализа
data_all <- dplyr::select(data, -c(Id, Short.Name, ISO.Code))
#View(data_all)

#убираем пропуски
data_clean <- na.omit(data_all)
View(data_clean)
#было 892 наблюдения, стало 866

#2001
names(data_clean)
data2001 <- dplyr::select(data_clean, - Judicial.Effectiveness, -Labor.Freedom, -Fiscal.Health)
names(data2001)                          
data2001_cor <- data2001

#данные с 2005 года с labor freedom
data2005<- dplyr::filter(data_clean, Labor.Freedom!='NULL')
data2005_cor <- data2005
#View(data2005)

#данные с 2017 года с labor freedom, Judicial.Effectiveness, Fiscal.Health
data2017<- dplyr::filter(data_clean, Labor.Freedom!='NULL', Judicial.Effectiveness >0, Fiscal.Health > 0)
data2017_cor <- data2017
View(data2017)


#Меняем структуру данных
chars <- sapply(data2017_cor, is.character)
data2017_cor[, chars] <- as.data.frame(apply(
     data2017_cor[, chars], 2, as.numeric))
sapply(data2017_cor, class)

chars5 <- sapply(data2005_cor, is.character)
data2005_cor[, chars5] <- as.data.frame(apply(
  data2005_cor[, chars5], 2, as.numeric))
sapply(data2005_cor, class)

#2001
chars1 <- sapply(data2001_cor, is.character)
data2001_cor[, chars1] <- as.data.frame(apply(
  data2001_cor[, chars1], 2, as.numeric))
sapply(data2001_cor, class)
#View(data2001_cor)
#data_all$values <- row.names(data_all)
#data_all$values <- as.numeric(data_all$values)
#data_clean <- data_all$values

#data_frame_mod <- transform(
#  data2017,data2017[,2:16] = as.numeric(data2017[,2:16]))

#Строим корреляционную матрицу
data2017_cor <- dplyr::select(data2017_cor, -economy)
corrplot(cor(data2017_cor))

names(data2005_cor)
data2005_cor <- dplyr::select(data2005_cor, -economy, -Judicial.Effectiveness, - Fiscal.Health)
data2005_cor_clean <- na.omit(data2005_cor)
#View(data2005_cor)

data2001_cor <- dplyr::select(data2001_cor, -economy)
data2001_cor_clean <- na.omit(data2001_cor)
#View(data2001_cor_clean)
corrplot(cor(data2001_cor_clean))

data2001_cor_cleany <- dplyr::select(data2001_cor_clean, -year)
#  табличку с описательными статистиками
stargazer(data2017_cor, type="text", median=TRUE,
          digits=2, title="Traffic Fatalities Data")
#красивая табличка описательных статистик
datasummary( All(data2017_cor) ~  SD + Mean + Median, data = data2017_cor) 
datasummary( All(data2017_cor) ~  SD + Mean + Median + Min + Max, data = data2017_cor) 
datasummary( All(data2005_cor_clean) ~  SD + Mean + Median + Min + Max, data = data2005_cor_clean) 
datasummary( All(data2001_cor_cleany) ~  SD + Mean + Median + Min + Max, data = data2001_cor_cleany) 

#### Графики ####
# Построим диаграмму для 1982 года
names(data2017)
qplot(Overall.Score, Entrepreneurial.intentions, 
      data = subset(data2017, year==2018), main = "data2017")

# Наложим на нее линию регрессии
#qplot(beertax, fatality_rate, 
#      data = subset(Fatalities, year==1982), main = "Fatalities")+stat_smooth(method = "lm")
# Аналогично для 1988 года
#qplot(beertax, fatality_rate, 
#      data = subset(Fatalities, year==1988), main = "Fatalities")+stat_smooth(method = "lm")

#### Спецификация ####
names(freedom.scores)
names(gem)
#View(data2017)

data2017_m <- data.frame(data2017$economy, data2017_cor)
#View(data2017_m)
str(data2017_m)

data2005_m <- data.frame(data2005$economy, data2005_cor)
#View(data2005_m)
str(data2005_m)

data2001_m <- data.frame(data2001$economy, data2001_cor)
#View(data2001_m)
str(data2001_m)

#посмотреть а впринципе свобода влияет ли через овер ол скор

#по 2017, т.к. в нем все переменные уже есть
mod_sp <- lm(Established.Business.Ownership ~ . - Overall.Score - Total.early.stage.Entrepreneurial.Activity 
             - Entrepreneurial.intentions, data = data2017_cor)
summary(mod_sp)

#2)Проверка на наличие мультиколлинеарности и удаление некоторых регрессоров в случае ее наличия
vif(mod_sp)
#все меньше 10, мультиколлинеарности нет

#3)Проверка остатков на нормальность через QQ-plot
plot(mod_sp, which = 2)
#остатки ненормальны: завышены у наибольших значений

#4)Тест Бокса-Кокса и логарифмирование зависимой переменной при необходимости
boxCox(mod_sp)
#лямба почти 0 
mod_sp1 <- update(mod_sp, log(Established.Business.Ownership) ~ .)
summary(mod_sp1)
#меньше значимости
boxCox(mod_sp1)
plot(mod_sp1, which = 2)
#остатки стали сильно ненормальными у наименьших значений

#5)Тест Рамсея 
resettest(mod_sp)
#H0: степени не пропущены
#p-value=0.0.29073 > 0.05 => гипотеза принимается => не пропущены

names(data2001)


#6)Подбор спецификации при помощи crPlots
crPlots(mod_sp)
crPlots(mod_sp1)

mod_sp2 <- update(mod_sp, log(Established.Business.Ownership) ~ . - Monetary.Freedom + log(Monetary.Freedom))
crPlots(mod_sp2)

mod_sp3 <- update(mod_sp,  ~ . - Monetary.Freedom + log(Monetary.Freedom))
crPlots(mod_sp3)
#визуально логарифмы не улучшают картину


#7)Удаление незначимых переменных (вручную или через stepAIC)
mod2_sp <- stepAIC(mod_sp)
mod3_sp <- stepAIC(mod_sp3)
#удаляем ROCE, growth,   ROE, DSI,  Yield 

#8)Сравнение модели до удаления и после удаления переменных через тест «Короткая против длинной» (тест Вальда)
waldtest(mod2_sp, mod_sp)
#p-value = 0.9155 > 0.05 - нулевая гипотеза принимается
#коэффциенты длинной равны нулю

#9)Сравнение короткой и длинной модели по критерию Акаике (AIC)
AIC(mod_sp)
AIC(mod2_sp)
#у mod2 меньше чем у mod, лучше 

summary(mod2_sp)
summary(mod3_sp)
#модели одинаковы по значимости переменных

#не логарифмируем

#10 Тест Бреуша-Пагана 
#H0: гетероскедастичность отсутствует
bptest(mod2_sp, studentize = FALSE)
#p-value = 1.75e-13 < 0.05 H0 не принимается
#гетероскедостичность



#### Панельные модели ####
names(data2017)
View(data2017)

All.business <- data2017_cor$Established.Business.Ownership + data2017_cor$Total.early.stage.Entrepreneurial.Activity
All.business5 <- data2005_cor$Established.Business.Ownership + data2005_cor$Total.early.stage.Entrepreneurial.Activity
All.business1 <- data2001_cor$Established.Business.Ownership + data2001_cor$Total.early.stage.Entrepreneurial.Activity

data2017_m <- data.frame(data2017$economy, data2017_cor, All.business)
#View(data2017_m)
str(data2017_m)

data2005_m <- data.frame(data2005$economy, data2005_cor, All.business5)
#View(data2005_m)
str(data2005_m)

data2001_m <- data.frame(data2001$economy, data2001_cor, All.business1)
#View(data2001_m)
str(data2001_m)

#data2 <- data %>% mutate(output_l = log(output), emp_l = log(emp), capital_l = log(capital)) %>% 
#  dplyr::select(- output, - emp, - capital) %>% pdata.frame(index = c("firm", "year"), drop.index = TRUE)
#head(data2)

####2017 ####
#построим модели (1-pooled, 2 - фиксир, 3 - случ, 4 - фиксир двусторон(эфф времени))


#Established.Business.Ownership
mod1 <- plm(Established.Business.Ownership ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
            + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2017_m, index = c("data2017.economy","year"), model = "pooling")
mod2 <- plm(Established.Business.Ownership ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
            + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2017_m, index = c("data2017.economy","year"), model = "within")
mod3 <- plm(Established.Business.Ownership ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
            + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2017_m, index = c("data2017.economy","year"), model = "random")
mod4 <- plm(Established.Business.Ownership ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
            + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2017_m, index = c("data2017.economy","year"), model = "within",
            effect = "twoways")

summary(mod1)

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2, mod1)
#p-value  2.2e-16 < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2, mod3)
#p-value = 0.03581 < 0.05 - FE лучше, на 1% уровне значимости - нет

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3)
# p-value < 2.2e-16 p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4, mod1)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

cse <- function(model) {
  A <- sqrt(diag(plm::vcovHC(model)))
  return(A)
}

stargazer(mod1, mod2, mod3, mod4, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1), cse(mod2), cse(mod3), cse(mod4)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value < 0.05",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2017 Established.Business.Ownership.html")

#Total early-stage Entrepreneurial Activity 

mod1tea <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
            + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2017_m, index = c("data2017.economy","year"), model = "pooling")
mod2tea <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
            + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2017_m, index = c("data2017.economy","year"), model = "within")
mod3tea <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
            + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2017_m, index = c("data2017.economy","year"), model = "random")
mod4tea <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
            + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2017_m, index = c("data2017.economy","year"), model = "within",
            effect = "twoways")

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2tea, mod1tea)
#p-value < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2tea, mod3tea)
#p-value = 0.5803 > 0.05 - RE лучше

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3tea)
#p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4tea, mod1tea)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

#RE

stargazer(mod1tea, mod2tea, mod3tea, mod4tea, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1tea), cse(mod2tea), cse(mod3tea), cse(mod4tea)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value > 0.5",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2017 TEA.html")

#Entrepreneurial.intentions
mod1int<- plm(Entrepreneurial.intentions ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
               + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2017_m, index = c("data2017.economy","year"), model = "pooling")
mod2int <- plm(Entrepreneurial.intentions ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
               + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2017_m, index = c("data2017.economy","year"), model = "within")
mod3int <- plm(Entrepreneurial.intentions ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
               + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2017_m, index = c("data2017.economy","year"), model = "random")
mod4int <- plm(Entrepreneurial.intentions ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
               + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2017_m, index = c("data2017.economy","year"), model = "within",
               effect = "twoways")

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2int, mod1int)
#p-value < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2int, mod3int)
#p-value = 0.3843 > 0.05 - RE лучше

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3int)
#p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4int, mod1int)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

#RE


stargazer(mod1int, mod2int, mod3int, mod4int, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1int), cse(mod2int), cse(mod3int), cse(mod4int)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value = 0.3843",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2017 intentions.html")

#All business 
mod1b <- plm(All.business  ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
             + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
             data = data2017_m, index = c("data2017.economy","year"), model = "pooling")
mod2b <- plm(All.business ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
             + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
             data = data2017_m, index = c("data2017.economy","year"), model = "within")
mod3b <- plm(All.business ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
             + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
             data = data2017_m, index = c("data2017.economy","year"), model = "random")
mod4b <- plm(All.business ~ Property.Rights + Judicial.Effectiveness + Government.Integrity + Tax.Burden + Government.Spending + Fiscal.Health 
             + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
             data = data2017_m, index = c("data2017.economy","year"), model = "within",
             effect = "twoways")

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2b, mod1b)
#p-value < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2b, mod3b)
#p-value = 0.7609 > 0.05 - RE лучше

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3b)
#p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4b, mod1b)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

#RE


stargazer(mod1b, mod2b, mod3b, mod4b, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1b), cse(mod2b), cse(mod3b), cse(mod4b)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value = 0.7609",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2017 All business.html")

####с 2005####
#Total early-stage Entrepreneurial Activity 

mod1tea5 <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending 
               + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2005_m, index = c("data2005.economy","year"), model = "pooling")
mod2tea5 <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending  
               + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2005_m, index = c("data2005.economy","year"), model = "within")
mod3tea5 <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending 
               + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2005_m, index = c("data2005.economy","year"), model = "random")
mod4tea5 <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending  
               + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2005_m, index = c("data2005.economy","year"), model = "within",
               effect = "twoways")

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2tea5, mod1tea5)
#p-value < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2tea5, mod3tea5)
#p-value = 0.5803 < 0.05 - FE лучше

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3tea5)
#p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4tea5, mod1tea5)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

#FE two-way

stargazer(mod1tea5, mod2tea5, mod3tea5, mod4tea5, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1tea5), cse(mod2tea5), cse(mod3tea5), cse(mod4tea5)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value < 0.05",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2005 TEA.html")


####с 2001####

#Established.Business.Ownership
mod1a <- plm(Established.Business.Ownership ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending 
            + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2001_m, index = c("data2001.economy","year"), model = "pooling")
mod2a <- plm(Established.Business.Ownership ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
            + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2001_m, index = c("data2001.economy","year"), model = "within")
mod3a <- plm(Established.Business.Ownership ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
            + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2001_m, index = c("data2001.economy","year"), model = "random")
mod4a <- plm(Established.Business.Ownership ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
            + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
            data = data2001_m, index = c("data2001.economy","year"), model = "within",
            effect = "twoways")

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2a, mod1a)
#p-value < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2a, mod3a)
#p-value < 0.05 - FE лучше

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3a)
#p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4a, mod1a)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

#FE с временными

stargazer(mod1a, mod2a, mod3a, mod4a, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1), cse(mod2), cse(mod3), cse(mod4)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value < 0.05",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2001 Established.Business.Ownership.html")


#Total early-stage Entrepreneurial Activity 

mod1teaa <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
               + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2001_m, index = c("data2001.economy","year"), model = "pooling")
mod2teaa <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
               + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2001_m, index = c("data2001.economy","year"), model = "within")
mod3teaa <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
               + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2001_m, index = c("data2001.economy","year"), model = "random")
mod4teaa <- plm(Total.early.stage.Entrepreneurial.Activity ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
               + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2001_m, index = c("data2001.economy","year"), model = "within",
               effect = "twoways")

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2teaa, mod1teaa)
#p-value < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2teaa, mod3teaa)
#p-value < 0.05 - FE лучше

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3teaa)
#p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4teaa, mod1teaa)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

#FE с временными

stargazer(mod1teaa, mod2teaa, mod3teaa, mod4teaa, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1teaa), cse(mod2teaa), cse(mod3teaa), cse(mod4teaa)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value < 0.05",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2001 TEA.html")

#Entrepreneurial.intentions
mod1inta<- plm(Entrepreneurial.intentions ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending 
              + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
              data = data2001_m, index = c("data2001.economy","year"), model = "pooling")
mod2inta <- plm(Entrepreneurial.intentions ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
               + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2001_m, index = c("data2001.economy","year"), model = "within")
mod3inta <- plm(Entrepreneurial.intentions ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending 
               + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2001_m, index = c("data2001.economy","year"), model = "random")
mod4inta <- plm(Entrepreneurial.intentions ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
               + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2001_m, index = c("data2001.economy","year"), model = "within",
               effect = "twoways")

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2teaa, mod1teaa)
#p-value < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2teaa, mod3teaa)
#p-value < 0.05 - FE лучше

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3teaa)
#p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4teaa, mod1teaa)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

#FE с временными

stargazer(mod1inta, mod2inta, mod3inta, mod4inta, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1inta), cse(mod2inta), cse(mod3inta), cse(mod4inta)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value < 0.05",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2001 Intentions.html")

#All.business1
mod1ab<- plm(All.business1 ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending 
               + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
               data = data2001_m, index = c("data2001.economy","year"), model = "pooling")
mod2ab <- plm(All.business1 ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
                + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
                data = data2001_m, index = c("data2001.economy","year"), model = "within")
mod3ab <- plm(All.business1 ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending 
                + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
                data = data2001_m, index = c("data2001.economy","year"), model = "random")
mod4ab <- plm(All.business1 ~ Property.Rights + Government.Integrity + Tax.Burden + Government.Spending
                + Business.Freedom + Monetary.Freedom + Trade.Freedom + Investment.Freedom + Financial.Freedom, 
                data = data2001_m, index = c("data2001.economy","year"), model = "within",
                effect = "twoways")

#проведем тесты на выбор модели
#FE против pooled - тест на линейное ограничение
#H_0: Средние значения для каждой страны одинаковы
pFtest(mod2ab, mod1ab)
#p-value < 0.05 - FE лучше

#FE против RE - тест Хаусмана
#H_0: Индивидуальные эффекты не коррелированы с регрессорами
phtest(mod2ab, mod3ab)
#p-value < 0.05 - FE лучше

#RE против pooled - тест бреуша-пагана
#H_0: Индивидуальные эффекты отсутствуют
plmtest(mod3ab)
#p-value < 0.05 - RE лучше

#нужны ли эффекты времени - короткая против длинной - тест на линейное ограничение тоже
#H_0: Коэффициенты перед бинарными переменными года равны 0
pFtest(mod4ab, mod1ab)
#взяли сначала длинную - с эффектами времени
#p-value < 0.05 - модель с эффектами времени лучше
#итого. общий вывод: модель с эффектами времени и фиксированными эффектами оказалась наилучшей
#расчетные статистики  - F

#FE с временными

stargazer(mod1ab, mod2ab, mod3ab, mod4ab, type = "html", 
          column.labels = c("Pooled", "FE", "RE", "FE twoways"), 
          se = list(cse(mod1ab), cse(mod2ab), cse(mod3ab), cse(mod4ab)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value < 0.05",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2001 All.business.html")

#table(index(data2017), useNA = "ifany")

# ctrl A - выделяем и сохраняем


#### финальные модели ####

#Established.Business.Ownership
V1est <- plm::vcovHC(mod4a)
coeftest(mod4a, vcov. = V1est)

#модель с только значимыми коэффициентами
modest <- plm(Established.Business.Ownership ~ Tax.Burden + Trade.Freedom, 
             data = data2001_m, index = c("data2001.economy","year"), model = "within",
             effect = "twoways")

#короткая против длинной
#H_0: Коэффициенты перед переменными, пропущенными в короткой регрессии равны 0
V2est <- plm::vcovHC(modest)
waldtest(modest, mod4a, vcov = V1est)
#p-value=0.7328 > 0.05 - нулевая гипотеза принимается, короткая модель лучше

#Total early-stage Entrepreneurial Activity 
V1tea <- plm::vcovHC(mod4teaa)
coeftest(mod4teaa, vcov. = V1tea)

#модель с только значимыми коэффициентами
modtea <- plm(Total.early.stage.Entrepreneurial.Activity ~ Tax.Burden + Monetary.Freedom + Trade.Freedom, 
                data = data2001_m, index = c("data2001.economy","year"), model = "within",
                effect = "twoways")
#короткая против длинной
#H_0: Коэффициенты перед переменными, пропущенными в короткой регрессии равны 0
V2tea <- plm::vcovHC(modtea)
waldtest(modtea, mod4teaa, vcov = V1tea)
#p-value=0.7957 > 0.05 - нулевая гипотеза принимается, короткая модель лучше

#Entrepreneurial.intentions
V1int <- plm::vcovHC(mod4inta)
coeftest(mod4inta, vcov. = V1int)

#модель с только значимыми коэффициентами
modint <- plm(Entrepreneurial.intentions ~ Trade.Freedom, 
              data = data2001_m, index = c("data2001.economy","year"), model = "within",
              effect = "twoways")
summary(modint)

#короткая против длинной
#H_0: Коэффициенты перед переменными, пропущенными в короткой регрессии равны 0
V2int <- plm::vcovHC(modint)
waldtest(modint, mod4inta, vcov = V1int)
#R не проверяет с исключением всех кроме одной переменной, поэтому проверяем дважды: вместе с Trade.Freedom оставляем попеременно две другие незначимые перменные,
#тест говорит о превосходстве коротких регрессий

#All.business
V1all <- plm::vcovHC(mod4ab)
coeftest(mod4ab, vcov. = V1all)

#модель с только значимыми коэффициентами
modall <- plm(All.business1 ~ Tax.Burden + Monetary.Freedom + Trade.Freedom, 
    data = data2001_m, index = c("data2001.economy","year"), model = "within",
    effect = "twoways")

#короткая против длинной
#H_0: Коэффициенты перед переменными, пропущенными в короткой регрессии равны 0
V2all <- plm::vcovHC(modall)
waldtest(modall, mod4ab, vcov = V1all)
#p-value=0.6712 > 0.05 - нулевая гипотеза принимается, короткая модель лучше

stargazer(modest, modtea, modint, modall, type = "html", 
          column.labels = c("Established Business Ownership", "Total early-stage Entrepreneurial Activity", "Entrepreneurial intentions", "All business"), 
          se = list(cse(modest), cse(modtea), cse(modint), cse(modall)), 
          add.lines = list(c("F-test", "p-value < 0.05", "", "", ""),
                           c("Breusch-Pagan test", "", "p-value < 0.05", "", ""),
                           c("Hausman test", "", "", "p-value < 0.05",""),
                           c("Time effect", "", "", "","p-value < 0.05")), 
          omit.stat=c("adj.rsq"), out = "2001 final.html")

#### МГК ####

#Построим модель с использованием метода главных компонент (функция PCA) 
mod <- PCA(data2017_cor)

#строим визуализации МГК, удаляем переменные с короткими векторами - те, информацию о которых
#мы плохо сохраняем
data2017_cor2 <- dplyr::select(data2017_cor, -year)
mod_pca <- PCA(data2017_cor2)

data2001_cor2 <- dplyr::select(data2001_cor, -year)
mod_pca2 <- PCA(data2001_cor2)

data2017_cor3 <- dplyr::select(data2017_cor, -year, - Judicial.Effectiveness, - Established.Business.Ownership,
                               - Fiscal.Health)
mod_pca3 <- PCA(data2017_cor3)

data2005_cor2 <- dplyr::select(data2005_cor, -year, - Judicial.Effectiveness,  - Fiscal.Health)
mod_pca4 <- PCA(data2005_cor2)
#View(data2005)

#Визуализируем матрицу нагрузок и дайте интерпретацию первым 
#двум главным компонентам (corrplot). 
#Какие основные противоречия наблюдаются в данных (внутри главных компонент)?
corrplot(mod_pca4$var$coord[,1:2], is.corr = FALSE, cl.align.text = "l")
#компонента 1 отвечает за привлекательность компании на рынке
#компонента 2 отвечает за эффективность использования капитала

#Построим график каменистой осыпи и дайте ему интерпретацию 
fviz_eig(mod_pca4, addlabels = TRUE)
#резкое падение % объясняемой компонентой информации после второй
#берем две главные компоненты

#С использованием иерархической кластеризации определяем оптимальное число кластеров
mod_clust <- HCPC(mod_pca4, graph = FALSE)

summary(mod_pca4)

#Визуализируем дендрограмму  
fviz_dend(mod_clust, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)
#3 кластера

#Визуализируем кластеры наблюдений в пространстве главных компонент 
fviz_cluster(mod_clust, palette = "jco", repel = TRUE)



