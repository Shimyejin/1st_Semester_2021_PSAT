library(tidyverse)

setwd('C:\\Users\\User\\Desktop\\P-SAT\\백신 주제')

data <- read.csv('final_train.csv')

nafactor_train_h1n1 <- select(data,-'seasonal_vaccine')

logistic_nafactor <- glm(h1n1_vaccine ~., data = nafactor_train_h1n1)

variable_select_h1n1 <- step(logistic_nafactor, direction = "both")

summary(variable_select_h1n1)

exp(coef(variable_select_h1n1)) #오즈

##

nafactor_train_seas <- select(data,-'h1n1_vaccine')

logistic_nafactor <- glm(seasonal_vaccine~., data=nafactor_train_seas)

variable_select_seas <- step(logistic_nafactor, direction = "both")

summary(variable_select_seas)

exp(coef(variable_select_seas))