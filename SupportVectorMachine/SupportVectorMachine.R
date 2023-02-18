#### Support Vector Machine ####
# SCENARIO - To predict the species based on the other factors provided in the dataset

# Library import
library(e1071)
library(caTools)

# Utilizing the inbuild dataset provided by R
plot(iris)

# plotting and color coding species
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species) # The data points are not scattered according to class. Thus, creating ideal hyperlane is not possible in linear SVM
plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species) # The data is mostly scattered according to class, but there are few data points that are overlapping

# Sampling data set
sample <- sample.split(iris, SplitRatio = 0.7)
col <- c("Petal.Length", "Petal.Width", "Species")
train <- subset(iris, sample == TRUE)[, col]
test <- subset(iris, sample == FALSE)[, col]

# Model Generation
svmFit <- svm(Species ~., data = train, kernel = "linear", cost = 100, scale = FALSE)
print(svmFit)

# plot model
plot(svmFit, train[, col])

# Tuning the model for getting the best parameters
tuneModel <- tune(svm, Species ~., data = train, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(tuneModel)

# Prediction
p <- predict(svmFit, test, type = "class")
plot(p)

# Cross check predictions
table(p, test[,3])

# Accuracy
mean(p == test[, 3])

# Random sample
randomSample <- iris[sample(1:nrow(iris), size = 1), col]
samplePrediction <- predict(svmFit, randomSample, type = "response")
print(as.character(samplePrediction))
