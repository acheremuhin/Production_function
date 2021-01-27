library("dplyr")
library("erer")
library("vcd")
library("ggplot2")
library("reshape2")
library("readxl")
Base1 <- read_excel("D:/Наука/Статьи и работы начатые/Ч003 - Статья с Д. В. - по отбору ПФ/Производственные функции.xlsx", sheet = 4)
glimpse(Base1)
m_1 <- lm(data = Base1, lnY ~ .)
summary(m_1)
# Step-wise регрессия
library(MASS)
model_step_wise<-stepAIC(m_1, direction = "both")
summary(model_step_wise)
# Случайный лес для отбора признаков
library(party)
cf1 <- cforest(lnY ~ . , data=Base1, control=cforest_unbiased(mtry=2,ntree=50))
per<-varimp(cf1)
order(varimp(cf1)) # получаем значения важности переменных
sort(per, decreasing=TRUE)
model_trees_1 <- lm(data = Base1, lnY ~ lnT)
summary(model_trees_1)
model_trees_2 <- lm(data = Base1, lnY ~ lnT+lnTL) # No
summary(model_trees_2)
# Регрессия с регуляризацией
library(glmnet)
x<-model.matrix(lnY~.,data=Base1)
x=x[,-1]
model_glmnet<-glmnet(x=x,y=Base1$lnY,type.measure='mse')
nam<-coef(model_glmnet, s = 0.01)
row.names(nam)[order(nam, decreasing = TRUE)]
model_glm_1 <- lm(data = Base1, lnY ~ lnT)
summary(model_glm_1)
model_glm_2 <- lm(data = Base1, lnY ~ lnT+lnS)
summary(model_glm_2)
model_glm_3 <- lm(data = Base1, lnY ~ lnT+lnS+lnSonT) # No
summary(model_glm_3)
# MARS-метод проверки важности переменных
library(earth)
model_mars <- earth(lnY ~ .,data=Base1)
ev <- evimp(model_mars)
plot(ev)
model_ev_1 <- lm(data = Base1, lnY ~ lnT)
summary(model_ev_1)
model_ev_2 <- lm(data = Base1, lnY ~ lnT+lnSonL)
summary(model_ev_2)
model_ev_3 <- lm(data = Base1, lnY ~ lnT+lnSonL+lnK)   # No
summary(model_ev_3)
library(gvlma)
summary(gvlma(model_step_wise))
summary(gvlma(model_trees_1))
summary(gvlma(model_trees_2))
summary(gvlma(model_glm_1))
summary(gvlma(model_glm_2))
summary(gvlma(model_ev_1))
summary(gvlma(model_ev_2))
# Ни одна модель не значима
model_trees_2
library(trafo)
assumptions(model_trees_2)
x<-logshiftopt(object = model_trees_2, method = "skew", plotit = FALSE)
Base1_1<-as.data.frame(x, row.names = NULL,
                           std = FALSE)
model_trees_2_lm <- lm(lnYt ~ lnT,data=Base1_1)
summary(model_trees_2_lm)
summary(gvlma(model_trees_2_lm))
# Ни одно преобразование не успешно
library(quantreg)
rq_1<-rq(data = Base1, tau = 0.5, lnY ~ 0+ lnT+lnSonL)
summary(rq_1)
