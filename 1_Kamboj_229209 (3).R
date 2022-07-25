#Group1
#Kartik Kamboj (229209)
############
library(ggplot2)
library(olsrr)
library(car)
library(carData)
usedcars_data <- read.csv('C:/Users/karti/Downloads/vw_data.csv')
## Any missing values
sum(is.na(usedcars_data))

summary(usedcars_data)

## Exclude inaccurate and meaningless data
usedcars_data <- usedcars_data[!(names(usedcars_data) %in% c("X"))]
summary(usedcars_data)

#Task1
usedcars_data$lp100km <- 282.48/usedcars_data$mpg

#Task 2
usedcars_data$age <- 2020 - usedcars_data$year

#drop the column mpg and year
usedcars_data <- usedcars_data[!(names(usedcars_data) %in% c("year", "mpg"))]


summary(usedcars_data)

##Task 3

usedcars_data$log_price <- log(usedcars_data$price)
summary(usedcars_data$log_price)
par(mfrow=c(2,2))

## Model fit using price as response variable
fit_model1 = lm(price ~ model+mileage+fuelType+engineSize+tax+transmission+lp100km+age, data = usedcars_data) # price as response variable
plot(fit_model1, add.smooth = FALSE) 
##Multicollinearity check
round(vif(fit_model1),3)
#plot(fit_1$residuals, pch = 16) #, col = "red")


## Model fit using log_price as response variable
fit_2 = lm(log_price ~ model+mileage+fuelType+engineSize+tax+transmission+lp100km+age, data = usedcars_data) # log_price as response variable
plot(fit_2,add.smooth = FALSE)


####
## drop price variable
usedcars_data <- usedcars_data[!(names(usedcars_data) %in% c("price"))]
summary(usedcars_data)
###
## Make categorical variables as factors
usedcars_data$model <- as.factor(usedcars_data$model)
usedcars_data$fuelType <- as.factor(usedcars_data$fuelType)
usedcars_data$transmission <- as.factor(usedcars_data$transmission)

#### Finding best subset

# Finding the "best" predictors for sqmPrice using Best Subset Selection

## best subset Check with combinations of all variables
best_subset <- ols_step_best_subset(lm(log_price ~ .,data = usedcars_data))
best_subset
# models with best combinations of all variables with their corresponding AIC and BIC values
data.frame(cbind( parameters = best_subset$predictors, AIC = round(best_subset$aic, 2) ))

## Task4
final_lm = lm(log_price ~ model+mileage+fuelType+engineSize+tax+transmission+lp100km+age, data = usedcars_data) 

summary(final_lm)
plot(fit_2,add.smooth = FALSE)
##Multicollinearity check
vif(final_lm)




###Confidence Interval####
round(confint(final_lm, level=0.95),3)
######







