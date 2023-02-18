#### DECISION TREE ####
# SCENARIO
# The data contains information about a ship that has 20 life boats
# These lifeboats are being distributed based on the class, gender and age of the passenge
# Aim - To develop a model that recognizes the relationship between these factors and predicts the survival of a passenger

# Setting the JAVA Environment
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk-18.0.1.1")
Sys.getenv("JAVA_HOME")

#Loading libraries
library(FSelector)
library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(data.tree)
library(caTools)

# Setting working directory
# setwd("D:\\Study\\Learning\\R\\ML Course\\Datasets") -- set according to your dataset paths

# Import data
shipData <- read.csv("ship.csv")

# Selecting only meaningful columns
shipData <- select(shipData, Survived, Pclass, Sex, Age)
newShipData <- mutate(shipData, Survived = factor(Survived), Pclass = as.numeric(Pclass), Age = as.numeric(Age))
newShipData <- na.omit(newShipData)

# Splitting data
set.seed(123)
sample = sample.split(newShipData$survived, SplitRatio = 0.70)
train = subset(newShipData, sample == TRUE)
test = subset(newShipData, sample == FALSE)

# Training the decision tree classifier
tree <- rpart(Survived ~., data = train)

# Predictions
tree.survived.predicted <- predict(tree, test, type="class")

# Confusion matrix for evaluating the model
confusionMatrix(tree.survived.predicted, test$Survived)

# Visualizing the decision tree
prp(tree)

# Predicting any random value
randomValue <- newShipData[sample(1:nrow(newShipData), size = 1), ]

# Survival prediction
survival <- predict(tree, randomValue, type = "class")

# Result
if (as.integer(survival) == 1) {
  cat("Congratulations, you survived")
} else {
  cat("Rest in peace!")
}
