library(factoextra)

iris

# Retrieve all the species
iris.labels = iris$Species

# Plotting in the table
table(iris.labels)

# Preparing unlabeled data
iris_data <- iris[1:4]

# Scaling the data
iris_data_scale = scale(iris_data)

# Finding the distance measure using euclidean method
iris_data <- dist(iris_data_scale, method = "euclidean")

# Calculate the number of clusters we need
# wss - within sum of squares, silhouette, gap_stat
fviz_nbclust(iris_data_scale, kmeans, method = "gap_stat") + labs(subtitle = "Elbow method")

# Kmeans
km.out <- kmeans(iris_data_scale, centers = 3)
print(km.out)

# Visualize the clustering algorithm results
clusters = km.out$cluster
rownames(iris_data_scale) <- paste(iris$Species, 1:dim(iris)[1], sep = "_")

fviz_cluster(list(data=iris_data_scale, cluster = clusters))
