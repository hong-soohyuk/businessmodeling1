library(forecast)

toyota.corolla.df <- read.csv("ToyotaCorolla.csv")
View(toyota.corolla.df)

training <- sample(toyota.corolla.df$Id, 600) # use 600 records as samples
head(training)
length(training)

validation <- sample(setdiff(toyota.corolla.df$Id, training), 400) # select discrete data from training dataset
head(validation)
length(validation)

reg <- lm(Price~., data = toyota.corolla.df[, -c(1,2,8,11)], subset = training,
          na.action = na.exclude)
#   regression
# (dependent variable name) ~ (independent variable name)
# if (independent variable name) == . then means that all of the variables
# include all the records from the data, exclude 1st, 2nd, 8th, 11th columns, which are respectively Id, Model Name, Fuel type, Color.
# how will not available data be handled? they will excluded

pred_t <- predict(reg, na.action = na.pass)
# data for the prediction is written in lm func
# na.pass : not available data will be included.

pred_v <- predict(reg, newdata = toyota.corolla.df[validation, -c(1, 2, 8, 11)],
                  na.action = na.pass)

pred_t
pred_v


accuracy(pred_t, toyota.corolla.df[training,]$Price)
# this prediction is compared with the training data that is used to make the model
# -> cannot be an objective validation.

accuracy(pred_v, toyota.corolla.df[validation,]$Price)
# RMSE,, Error is observed with increased value, that means there could be a over-fitting

owner.df <- read.csv("ownerExample.csv")
View(owner.df)

install.packages("caret")
library(caret)
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.5, 'owner', 'nonowner')), as.factor(owner.df$Class))






