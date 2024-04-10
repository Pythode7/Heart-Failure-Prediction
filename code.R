### Term project
rm(list=ls())
setwd("~/R/R data")

data <- read.csv('heart_failure_clinical_records_dataset.csv')
attach(data)

# summary of heart failure caused death number by sex
sm_1 <- aggregate(DEATH_EVENT, by=list(sex), table)
sm_1

# 2-sample test for equality of heat failure caused death in men and women
prop.test(c(62, 132), c(96, 203), alternative = 'two.sided')

# create a multiple logistic regression model to fit the data
m <- glm(DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes
         + ejection_fraction + high_blood_pressure + platelets + serum_creatinine
         + serum_sodium + sex + smoking)

library(aod)

# global test of the multiple logistic regression model
wald.test(b=coefficients(m), Sigma = vcov(m), Terms = 2:12)

# summarize the model to show the significant explanatory variables
summary(m)

# odds ratio for every year increase in age
exp(m$coefficients[2])
# odd ratio for ejection fraction
exp(m$coefficients[6])
# odd ratio for serum creatinine 
exp(m$coefficients[9])


library(pROC)

# check how well the model predicts the heart failure 
data$prob <- predict(m, type = c('response'))
g <- roc(data$DEATH_EVENT ~ data$prob)
# see the c-statistic
g

#create the ROC curve
plot(g)


