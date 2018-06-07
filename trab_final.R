########################################
# Trabalho Final - INF-0613          
# Nome(s): Felipe Wolff Ramos
#          Lucas Aoki Heredia
########################################

# Libraries
# install.packages(c('cluster', 'NLP', 'e1071', 'flexclust'))
library(cluster)
library(NLP)
library(e1071)
library(flexclust)

# Set Working Dir
# setwd("/Users/wfr005/work/courses/inf-0613/t_final")

# Increase max print
options(max.print=10000)

# Load csv files
features <- read.csv("features.csv", header = TRUE, sep = ",")
headlines <- read.csv("headlines.csv", header = TRUE, sep = ",")
headlines$publish_date <- strptime(headlines$publish_date, "%Y%m%d")

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

# PC choice
features.pc.choice <- 1390

# Sampling data to develop solution - only 10% of rows - comment to have full results
set.seed(42); 
features.sample <- features[sample(1:nrow(features), nrow(features) * 0.1, replace = FALSE),]
features.sample.pca <- prcomp(features.sample, scale. = FALSE)
features.sample.pca.dist <- dist(features.sample.pca$x[,1:features.pc.choice])

# k-means clustering
features.sample.kmeans.5 <- kmeans(features.sample.pca$x[,1:features.pc.choice], 5, nstart = 20)
features.sample.kmeans.10 <- kmeans(features.sample.pca$x[,1:features.pc.choice], 10, nstart = 20)
features.sample.kmeans.15 <- kmeans(features.sample.pca$x[,1:features.pc.choice], 15, nstart = 20)
features.sample.kmeans.20 <- kmeans(features.sample.pca$x[,1:features.pc.choice], 20, nstart = 20)
# k-means silhouette
features.sample.kmeans.5.sil <- silhouette(features.sample.kmeans.5$cluster, features.sample.pca.dist)
features.sample.kmeans.10.sil <- silhouette(features.sample.kmeans.10$cluster, features.sample.pca.dist)
features.sample.kmeans.15.sil <- silhouette(features.sample.kmeans.15$cluster, features.sample.pca.dist)
features.sample.kmeans.20.sil <- silhouette(features.sample.kmeans.20$cluster, features.sample.pca.dist)

mean(features.sample.kmeans.5.sil[,3])
mean(features.sample.kmeans.10.sil[,3])
mean(features.sample.kmeans.15.sil[,3])
mean(features.sample.kmeans.20.sil[,3])

# k-medians
features.sample.kmedians.5 <- kcca(features.sample.pca$x[,1:features.pc.choice], 5, family=kccaFamily("kmedians"))
features.sample.kmedians.10 <- kcca(features.sample.pca$x[,1:features.pc.choice], 10, family=kccaFamily("kmedians"))
features.sample.kmedians.15 <- kcca(features.sample.pca$x[,1:features.pc.choice], 15, family=kccaFamily("kmedians"))
features.sample.kmedians.20 <- kcca(features.sample.pca$x[,1:features.pc.choice], 20, family=kccaFamily("kmedians"))
# k-medians silhouette
features.sample.kmedians.5.sil <- silhouette(clusters(features.sample.kmedians.5), features.sample.pca.dist)
features.sample.kmedians.10.sil <- silhouette(clusters(features.sample.kmedians.10), features.sample.pca.dist)
features.sample.kmedians.15.sil <- silhouette(clusters(features.sample.kmedians.15), features.sample.pca.dist)
features.sample.kmedians.20.sil <- silhouette(clusters(features.sample.kmedians.20), features.sample.pca.dist)
# avg
summary(features.sample.kmedians.5.sil )$avg.width
summary(features.sample.kmedians.10.sil)$avg.width
summary(features.sample.kmedians.15.sil)$avg.width
summary(features.sample.kmedians.20.sil)$avg.width

# Fuzzy c-means
features.sample.cmeans.5 <- cmeans(features.sample.pca$x[,1:features.pc.choice], 5, m = 2)
features.sample.cmeans.10 <- cmeans(features.sample.pca$x[,1:features.pc.choice], 10, m = 2)
features.sample.cmeans.15 <- cmeans(features.sample.pca$x[,1:features.pc.choice], 15, m = 2)
features.sample.cmeans.20 <- cmeans(features.sample.pca$x[,1:features.pc.choice], 20, m = 2)
# Fuzzy c-means silhouette
features.sample.cmeans.5.sil <- silhouette(features.sample.cmeans.5$cluster, features.sample.pca.dist)
features.sample.cmeans.10.sil <- silhouette(features.sample.cmeans.10$cluster, features.sample.pca.dist)
features.sample.cmeans.15.sil <- silhouette(features.sample.cmeans.15$cluster, features.sample.pca.dist)
features.sample.cmeans.20.sil <- silhouette(features.sample.cmeans.20$cluster, features.sample.pca.dist)
# avg
summary(features.sample.kmedians.5.sil )$avg.width
summary(features.sample.kmedians.10.sil)$avg.width
summary(features.sample.kmedians.15.sil)$avg.width
summary(features.sample.kmedians.20.sil)$avg.width

# Bigrams calculation
get_bigram_freq <- function(headlines) {
  # acquire all possible bigrams
  all_bigrams <- c()
  for (headline in headlines) {
    all_bigrams <- c(all_bigrams, get_bigrams(headline))
  }
  unique_bigrams <- unique(all_bigrams)
  
  # Named vector to count bigrams frequency
  result <- rep(0, length(unique_bigrams))
  names(result) <- unique_bigrams
  
  # Count bigrams occurrence
  for (bigram in all_bigrams) {
    result[bigram] = result[bigram] + 1
  }
  
  return(result)
}

get_bigrams <- function(headline) {
  result <- c()
  words <- strsplit(headline, " ", fixed = TRUE)[[1L]]
  bigrams <- ngrams(words, 2L)
  for (bigram in bigrams) {
    result <- c(result, paste(bigram, collapse = " "))
  }
  return(result)
}

get_cluster_freq_bigrams <- function(cluster, headlines, n = 3) {
  clusters <- sort(unique(cluster))
  result <- list()
  for (c in clusters) {
    bigrams <- get_bigram_freq(headlines[as.integer(names(which(cluster == c)))])
    bigrams <- sort(bigrams, decreasing = TRUE)
    result[[length(result) + 1]] <- names(bigrams[1:n])
  }
  return(result)
}

# k-means bigrams
features.sample.kmeans.5.bigrams <- get_cluster_freq_bigrams(features.sample.kmeans.5$cluster, as.character(headlines$headline_text))
features.sample.kmeans.10.bigrams <- get_cluster_freq_bigrams(features.sample.kmeans.10$cluster, as.character(headlines$headline_text))
features.sample.kmeans.15.bigrams <- get_cluster_freq_bigrams(features.sample.kmeans.15$cluster, as.character(headlines$headline_text))
features.sample.kmeans.20.bigrams <- get_cluster_freq_bigrams(features.sample.kmeans.20$cluster, as.character(headlines$headline_text))

# k-medians bigrams
features.sample.kmedians.5.bigrams <- get_cluster_freq_bigrams(clusters(features.sample.kmedians.5), as.character(headlines$headline_text))
features.sample.kmedians.10.bigrams <- get_cluster_freq_bigrams(clusters(features.sample.kmedians.10), as.character(headlines$headline_text))
features.sample.kmedians.15.bigrams <- get_cluster_freq_bigrams(clusters(features.sample.kmedians.15), as.character(headlines$headline_text))
features.sample.kmedians.20.bigrams <- get_cluster_freq_bigrams(clusters(features.sample.kmedians.20), as.character(headlines$headline_text))

# Fuzzy c-means bigrams
features.sample.cmeans.5.bigrams <- get_cluster_freq_bigrams(features.sample.cmeans.5$cluster, as.character(headlines$headline_text))
features.sample.cmeans.10.bigrams <- get_cluster_freq_bigrams(features.sample.cmeans.10$cluster, as.character(headlines$headline_text))
features.sample.cmeans.15.bigrams <- get_cluster_freq_bigrams(features.sample.cmeans.15$cluster, as.character(headlines$headline_text))
features.sample.cmeans.20.bigrams <- get_cluster_freq_bigrams(features.sample.cmeans.20$cluster, as.character(headlines$headline_text))


## 2016 Dataset
features.2016.pca <- prcomp(features[headlines$publish_date$year + 1900 == 2016, ], scale. = FALSE) # can't scale
features.2016.pca.summary <- summary(features.2016.pca)
# 85% - PC 919
# 90% - PC 1084
features.2016.pc.choice <- 919
features.2016.pca.dist <- dist(features.2016.pca$x[, 1:features.2016.pc.choice])

# 2016 k-means clustering
features.2016.kmeans.5 <- kmeans(features.2016.pca$x[, 1:features.2016.pc.choice], 5, nstart = 20)
features.2016.kmeans.10 <- kmeans(features.2016.pca$x[, 1:features.2016.pc.choice], 10, nstart = 20)
features.2016.kmeans.15 <- kmeans(features.2016.pca$x[, 1:features.2016.pc.choice], 15, nstart = 20)
features.2016.kmeans.20 <- kmeans(features.2016.pca$x[, 1:features.2016.pc.choice], 20, nstart = 20)
# 2016 k-means silhouette
features.2016.kmeans.5.sil <- silhouette(features.2016.kmeans.5$cluster, features.2016.pca.dist)
features.2016.kmeans.10.sil <- silhouette(features.2016.kmeans.10$cluster, features.2016.pca.dist)
features.2016.kmeans.15.sil <- silhouette(features.2016.kmeans.15$cluster, features.2016.pca.dist)
features.2016.kmeans.20.sil <- silhouette(features.2016.kmeans.20$cluster, features.2016.pca.dist)
# 2016 AVG
summary(features.2016.kmeans.5.sil)$avg.width
summary(features.2016.kmeans.10.sil)$avg.width
summary(features.2016.kmeans.15.sil)$avg.width
summary(features.2016.kmeans.20.sil)$avg.width
# k-means bigrams
features.2016.kmeans.5.bigrams <- get_cluster_freq_bigrams(features.2016.kmeans.5$cluster, as.character(headlines$headline_text))
features.2016.kmeans.10.bigrams <- get_cluster_freq_bigrams(features.2016.kmeans.10$cluster, as.character(headlines$headline_text))
features.2016.kmeans.15.bigrams <- get_cluster_freq_bigrams(features.2016.kmeans.15$cluster, as.character(headlines$headline_text))
features.2016.kmeans.20.bigrams <- get_cluster_freq_bigrams(features.2016.kmeans.20$cluster, as.character(headlines$headline_text))
