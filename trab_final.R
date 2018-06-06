########################################
# Trabalho Final - INF-0613          
# Nome(s): Felipe Wolff Ramos
#          Lucas Aoki Heredia
########################################

# Libraries
library(cluster)

# Set Working Dir
setwd("/Users/wfr005/work/courses/inf-0613/t_final")

# Increase max print
options(max.print=10000)
features <- read.csv("features.csv", header = TRUE, sep = ",")

# Apply PCA with scale
# 85% - 1654
# 90% - 1804
features.pca <- prcomp(features, scale. = TRUE)
features.pca.summary <- summary(features.pca)

# Apply PCA without scale
# 85% - 1390
# 90% - 1598
features.pca2 <- prcomp(features, scale. = FALSE)
features.pca2.summary <- summary(features.pca2)

# Sampling data to develop solution - only 10% of rows - comment to have full results
set.seed(42); features.sample <- features[sample(1:nrow(features), nrow(features) * 0.1, replace = FALSE),]
features.sample.pca <- prcomp(features.sample, scale. = FALSE)

# Kmeans clustering
features.sample.kmeans.5 <- kmeans(features.sample.pca$x[,1:1390], 5, nstart = 20)
features.sample.kmeans.10 <- kmeans(features.sample.pca$x[,1:1390], 10, nstart = 20)
features.sample.kmeans.15 <- kmeans(features.sample.pca$x[,1:1390], 15, nstart = 20)
features.sample.kmeans.20 <- kmeans(features.sample.pca$x[,1:1390], 20, nstart = 20)

# Silhouette
features.sample.kmeans.5.sil <- silhouette(features.sample.kmeans.5$cluster, dist(features.sample.pca$x[,1:1390]))
features.sample.kmeans.10.sil <- silhouette(features.sample.kmeans.10$cluster, dist(features.sample.pca$x[,1:1390]))
features.sample.kmeans.15.sil <- silhouette(features.sample.kmeans.15$cluster, dist(features.sample.pca$x[,1:1390]))
features.sample.kmeans.20.sil <- silhouette(features.sample.kmeans.20$cluster, dist(features.sample.pca$x[,1:1390]))

mean(features.sample.kmeans.5.sil[,3])
mean(features.sample.kmeans.10.sil[,3])
mean(features.sample.kmeans.15.sil[,3])
mean(features.sample.kmeans.20.sil[,3])
