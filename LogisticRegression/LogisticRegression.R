#### LOGISTIC REGRESSION ####

# SCENARIO
# Predict whether a candidate will get admission in desired college or not based on:
# 1. GPA
# 2. College Rank

# Import dataset and libraries
# Split the dataset into train and test
# Train the model using GPA and Rank
# Test the model
# Validate

# Load libraries
library(caTools)

# Ingest the data
# setwd("D:/Study/Learning/R/ML Course/Datasets/") -- Set the path according to your need
studentData <- read.csv("data.csv")

# Removing unncecessary column
studentData <- subset(studentData, select = -c(X), na.rm = TRUE)

# Change M to 1 and B to 0
studentData$diagnosis[studentData$diagnosis == "M"] <- 1
studentData$diagnosis[studentData$diagnosis == "B"] <- 0

# Change data type of the column
studentData$diagnosis <- as.numeric(studentData$diagnosis)

# Splitting data into training and testing
split <- sample.split(studentData, SplitRatio = 0.8)

# Train data
train_data <- subset(studentData, split == "TRUE")

# Test data
test_data <- subset(studentData, split == "FALSE")

# Munge the data
studentData$diagnosis <- as.factor(studentData$diagnosis)

# Train the model
patientModel <- glm(diagnosis ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + fractal_dimension_mean + radius_se + texture_se + perimeter_se + area_se, data = train_data, family = "binomial")
summary(patientModel)

testRes <- predict(patientModel, test_data, type = "response")
testRes

trainRes <- predict(patientModel, train_data, type = "response")
trainRes

# Validating the model - using confusion matrix
confMatrix <- table(Actual_Value = train_data$diagnosis, Predicted_Value = trainRes > 0.5)
confMatrix

#             Predicted_Value
# Actual_Value FALSE TRUE
#           0   276    3
#           1    10  157

# The above data suggests, When Actual Value was 0, Predicted Value was 0 for 276 times,
# And when the actual value was 1, our predicted value was 1, 157 times, which is great

randomData <- studentData[sample(1:nrow(studentData), size = 1), ]
res <- predict(patientModel, randomData, type = "response")