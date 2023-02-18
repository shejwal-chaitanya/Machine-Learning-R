library(factoextra)
library(class)
library(scorecard)
library(caTools)

# data
data = iris[1:4]

# Scaled Data
scaledData <- scale(data)

# Calculating distance
distData <- dist(scaledData, method = "euclidean")

# Row names
rownames(scaledData) <- paste(iris$Species, 1:dim(iris)[1], sep = "_")

# Creating clustering model
hc_iris <- hclust(distData, method = "complete")

# labeling data
hc_iris$labels <- levels(iris$Species)

# Groups
groups <- cutree(hc_iris, k = 2)

# plotting the data
fviz_cluster(list(data = scaledData, cluster = groups))

# Predicting new data
randomData <- iris[1:4][sample(1:nrow(iris), size = 1),]
predictedData <- knn(train = data, test = randomData, k = 1, cl = groups)
hc_iris$labels[predictedData]
