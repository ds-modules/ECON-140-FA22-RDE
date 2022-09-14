## Section 2, R Exercise
## Code by Daniela Paz (based on Problem Set 2, Econ 140, Spring 2022)


## For this problem we will use the dataset wages.csv. This dataset contains information on about 300 American workers. 
## It includes their average monthly wage (wage), gender (male) and completed years of formal education (educ). 
## You suspect that people with higher educational attainment earn more on average.

## 1. Setting a working directory 

setwd("/Users/Dani/Desktop/Section2_prep/")

##install.packages("AER")
##install.packages("psych")
##install.packages("pastecs")
##install.packages("dummies")
##install.packages("glmnet")
##install.packages("gmodels")
##install.packages("tidyverse")
##install.packages("ggplot2")

library(FNN)
library(class)
library(AER)
library(psych)
library(pastecs)
library(glmnet)
library(dummies)
library(gmodels)
library(tidyverse)
library(haven)
library(ggplot2)

## 2. read the dataset
data<-read.csv("wages.csv")

## 3. check variable names and first observations/cells to understand the dataset
colnames(data)
head(data)

## 4. how many observations are in the dataset? A: 
dim(data)

## 5. how many variables are in the dataset? A: 
dim(data)

## 6. how would you characterize this dataset (think about the sample, unit of analysis, time frame, etc)? A: 
View(data)

## 7. are any values missing? A: 
View(data)
is.na(data)
data[!complete.cases(data),]


## 8. are any of the columns categorical? A:
View(data)

## extra: how do you transform a categorical variable into a continuous one?

## 9. what is the proportion of male? A: 
mean(data$male)

## 10. what is the mean of the education variable? A: 
summary(data$educ)
mean(data$educ)

round(mean(data$educ),1)

## 11. what is the standard deviation of wage? A:
summary(data$wage)
sd(data$wage)

round(sd(data$wage),2)

## 12. do you see outliers? What would you do with outliers in wage? A:


## 13. Plot a scatter diagram of the average monthly wage against the male dummy. 
## What differences do you see? Explain. A: 

## scatter plot of wage and gender
x <- data$male
y <- data$wage

plot(x, y, main = "Wage by Gender",
     xlab = "Male", ylab = "Wage",
     pch = 1)


## 14. run linear model that regresses wage on gender. A:

wage_male  <- lm(wage~male, data)
summary(wage_male)
print(wage_male)

## 15. how do you interpret the coefficients (see section slides or class 4 takeaways)? A:

mean(data[data$male==0, "wage"])
mean(data[data$male==1, "wage"])


## 16. is this evidence of discrimination or something else? How would you test this? A:

mean(data[data$male==0, "educ"])
mean(data[data$male==1, "educ"])


## 17. let's repeat the exercise and now create a scatter plot of wage and education. A:
x_2 <- data$educ
y <- data$wage

plot(x_2, y, main = "Wage by Education",
     xlab = "Education", ylab = "Wage",
     pch = 1)
abline(lm(y ~ x_2, data = data), col = "blue")

## 18. Do you see a high-school "diploma effect" ? A:

## 19. Let's perform a linear model that regresses wage on education
wage_educ        <- lm(wage~educ, data)
summary(wage_educ)
print(wage_educ)

## 20. how do you interpret the constant in this case? (see section slides or class 4 takeaways)

## 21. how do you interpret the effect on the education variable?

## extra: how would you change this model to evaluate the theory of the "diploma effect" ?

## 22. do you think our regressions reflects the causal effect of schooling on wages ? (think about cofounders, sampling strategy, outliers)

## extra: what is the advantage of using log wage instead of wages?

## extra: how would you test the null hypothesis that the return to schooling is $100?

## 23. run linear model that regresses wage on all variables
wage_all        <- lm(wage~., data)
summary(wage_all)
print(wage_all)



## Final discussion: let's come back to think how to measure discrimination and returns to education




