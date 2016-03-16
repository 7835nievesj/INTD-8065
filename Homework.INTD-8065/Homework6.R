#INTD 8065 Data Analysis for Cancer Research
##Homework #6

#1. Plot the data

Mortality.rate <- c(102.5,104.5,100.4,95.9,87.0,95.0,88.6,89.2,78.9,84.6,81.7,
                    72.2,65.1,68.1,67.3,52.5)
Temperature <- c(51.3,49.4,50.0,49.2,48.5,47.8,47.3,45.1,46.3,42.1,44.2,43.5,
                 42.3,40.2,31.8,34.0)
tempMortality <- data.frame(Temperature,Mortality.rate)
plot(tempMortality)
cor(tempMortality)

#2. Fit the regression

morTemp.lm1 <- lm(Mortality.rate~Temperature, data = tempMortality)
names(morTemp.lm1)
coef(morTemp.lm1)

#3. Which parameter estimates are significant?

summary(morTemp.lm1)

#4. How much of the variance is explained by the regression?

morTemp.lm0 <- lm(Mortality.rate ~ 1, data=tempMortality)
summary(morTemp.lm0)
anova(morTemp.lm0,morTemp.lm1)

#5. Plot the residuals. What is your diagnostics? 

plot(morTemp.lm1$residuals)

par(mfrow=c(2,2))
plot(morTemp.lm1,pch=15)

#6. Find the confidence intervals of the regression line for the following 
#   values of T:  35,40,45,50

temp <- c(35,40,45,50)
names(temp)<- c(35,40,45,50) 

#We will work with a "mean" of tempMortality
with(tempMortality,c(mean(Temperature),mean(Mortality.rate)))

# Confidence interval for mean of tempMortality

predict(morTemp.lm1,data.frame(Temperature = temp,
                 Mortality.rate = 83.34375),interval="confidence")

#7. Find the prediction interval of the regression line for the same values of 
#   previous question.

predict(morTemp.lm1,data.frame(Temperature = temp,
                 Mortality.rate = 83.34375),interval="prediction")

#8. What is your overall conclusion? 
library(ggplot2)
ggplot(tempMortality, aes(x=Mortality.rate, y=Temperature)) +
        geom_point(colour = 'red', size = 3) +   
        geom_smooth(method=lm)
