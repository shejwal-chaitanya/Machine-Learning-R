#### RANDOM FOREST ####
# SCENARIO - to estimate the wine quality based on different factors
# The dataset has been extracted from Kaggle
# URL - https://www.kaggle.com/datasets/yasserh/wine-quality-dataset?select=WineQT.csv

# Set Environment JAVA_HOME
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk-18.0.1.1")

# Importing libraries
library(randomForest)
library(caTools)

# Extracting data
wineData <- read.csv("WineQT.csv")

# Removing ID
wineData <- subset(wineData, select = -c(Id))

# Munging data
wineData$quality <- as.factor(wineData$quality)

# Splitting dataset
split <- caTools::sample.split(wineData, SplitRatio = 0.80)
trainData <- subset(wineData, split == TRUE)
testData <- subset(wineData, split == FALSE)

# Generating random forest model
rfModel <- randomForest(quality ~ ., data = trainData, mtry = 4, ntree = 2001, importance = TRUE)

# Plotting
plot(rfModel)

# The OOB - out of bag error rate is 33.33% which is quite high in this case

# This shows, actual value v/s predicted value
result <- data.frame(testData$quality, predict(rfModel, testData, type = "response"))
print(result)
