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

## Nonlinear terms and Interactions, * indicates interaction
fit5 <- lm(medv~lstat*age, Boston)
summary(fit5)

## Adding a quadratic term
fit6 <- lm(medv~lstat + I(lstat^2), Boston); summary(fit6)
attach(Boston)
par(mfrow = c(1,1))
plot(medv~lstat)

## Fit quadratic model to data
points(lstat, fitted(fit6), col = "red", pch = 20)

## Fitting polynomials with poly function  (overfitting the data)
fit7 <- lm(medv~poly(lstat, 4)) ## 4 = degree of polynomial
points(lstat,fitted(fit7), col = "blue", pch = 20)

## Available plotting characters
plot(1:20,1:20, pch = 1:20, cex = 2)


## Qualitative predictors
names(Carseats)
summary(Carseats)
## See that ShelveLoc, Urban, and US are qualitative variables

fit1 <- lm(Sales~. + Income:Advertising + Age:Price, Carseats)
summary(fit1)
## Shows how qualitative measures are coded into dummy variables
contrasts(Carseats$ShelveLoc)


## Writing R functions
## Fit a regression model, make a plot
regplot <- function(x,y){
  fit <- lm(y~x)
  plot(x,y)
  abline(fit, col = "red")
}
attach(Carseats)
regplot(Price, Sales)

## Adding unnamed arguments (...)
regplot <- function(x,y,...){
  fit <- lm(y~x)
  plot(x,y,...)
  abline(fit, col = "red")
}

regplot(Price, Sales, xlab = "Price", ylab = "Sales", col = "blue", pch = 20)
