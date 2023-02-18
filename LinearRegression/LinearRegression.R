#### Multiple Linear Regression ####
# Linear Regression is a machine learning algorithm for prediction of continuous data
# Author - Chaitanya Shejwal

# Scenario - Predicting the Revenue based on
# 1. Paid Traffic - All the traffic that comes from advertisements
# 2. Organic Traffic - Non paid traffic that came from search engines
# 3. Social Traffic - The traffic that came from the social media websites

# y = m1x1 + m2x2 + m3x3 + c
# c - intercept
# m1, m2, m3 - slopes
# x1 - paid traffic
# x2 - organic traffic
# x3 - social traffic

# Flow - 
# 1. Genereate inputs using csv
# 2. Importing libraries
# 3. Splitting dataset into train & test
# 4. Applying regression
# 5. Validating model

sales <- read.csv("D:/Study/Learning/R/ML Course/Datasets/revenue.csv")

# first 6 records
head(sales)

# Summary
summary(sales)

# plot
plot(sales)

# Splitting data into training and test data
# Setting the seed generates the same set of random numbers
set.seed(2)

# Import library for loading the split method
library(caTools)

# Splits the data in 70% and 30%
split <- caTools::sample.split(sales, SplitRatio=0.7)

# Create a train set
train <- subset(sales, split == "TRUE")

# Create a test set
test <- subset(sales, split == "FALSE")

# Creating Model
linearModel <- lm(Profit ~., data = train)

# Summary of model
summary(linearModel)

# Prediction
pred <- predict(linearModel, test)

# Comparing Actual Values vs Predicted Values
plot(test$Profit, type = "l", col = "red")
lines(pred, type = "l", col = "blue")

# Plot predicted value
plot(pred, type="l", col = "blue", lty = 1.8)

# Finding accuracy
rmse <- sqrt(mean(pred-sales$Profit)^2)

# Package for calculate accuracy
library(caret)
caret::RMSE(pred, sales$Profit)

x = 1:nrow(train)
plot(x, train$Profit, col = "blue")
lines(x, pred, col="red")

newValues <- data.frame(
  Paid = c(160000),
  Organic = c(130000),
  Social = c(480000)
)
predictedValue <- predict(linearModel, newValues)
predictedValue
