library(MASS)
library(ISLR)

##Simple linear regression

names(Boston)
?Boston

## Plot data - "medv~lstat means medv = y, lstat = x" 
## ~ = 'modeled as'
plot(medv~lstat, Boston)
fit1 = lm(medv~lstat, data = Boston)

## prints out coefficients (indicates relationship)
fit1
## prints residuals, coefficients, t-values and p-values
summary(fit1)
##Fit to plot
abline(fit1, col = "red")
names(fit1)
##Confidence intervals
confint(fit1)
##Querying model fit for 3 specific values, give CI of those 3
predict(fit1, data.frame(lstat = c(5,10,15)), interval = "confidence")


### Multiple linear regression
fit2 <- lm(medv~lstat+age, data = Boston)
summary(fit2)

## Including all variables in model
fit3 <-  lm(medv~., Boston)
summary(fit3)

## Show plots in a 2 x 2 layout
plot(fit3)
par(mfrow = c(2,2))

## Updating model to change variables
fit4 <- update(fit3, ~.-age-indus)
summary(fit4)

