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
library(parallel)

# Set Working Dir
# setwd("/Users/wfr005/work/courses/inf-0613/t_final")

# Increase max print
options(max.print=10000)

# Load csv files
features <- read.csv("features.csv", header = TRUE, sep = ",")
headlines <- read.csv("headlines.csv", header = TRUE, sep = ",")
headlines$publish_date <- strptime(headlines$publish_date, "%Y%m%d")

pca  <- mcparallel(prcomp(features, scale. = TRUE))
pca2 <- mcparallel(prcomp(features, scale. = FALSE))

features.pca  <- mccollect(pca)[[1]]
features.pca2 <- mccollect(pca2)[[1]]

# Apply PCA with scale
# 85% - 1654
# 90% - 1804
features.pca.summary <- summary(features.pca)
# Apply PCA without scale
# 85% - 1390
# 90% - 1598
features.pca2.summary <- summary(features.pca2)

# PCA with scale - 85%
features.pc.choice <- 1390

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

add <- function(list, v) {
  name <- deparse(substitute(v))
  if (inherits(v,"parallelJob")) {
    v <- mccollect(v)[[1]]
  }
  l <- list()
  l[[name]] <- v
  return(c(list,l))
}

run_analysis <- function(features, headlines, pc, sc) {
  r <- list()

  features.pca <- prcomp(features, scale. = sc)
  features.pca.dist <- dist(features.pca$x[,1:pc])
  r <- add(r, features.pca)
  r <- add(r, features.pca.dist)

  # k-means clustering
  features.kmeans.5 <- mcparallel(kmeans(r$features.pca$x[,1:pc], 5, nstart = 20))
  features.kmeans.10 <- mcparallel(kmeans(r$features.pca$x[,1:pc], 10, nstart = 20))
  features.kmeans.15 <- mcparallel(kmeans(r$features.pca$x[,1:pc], 15, nstart = 20))
  features.kmeans.20 <- mcparallel(kmeans(r$features.pca$x[,1:pc], 20, nstart = 20))
  #features.kmeans.5 <- kmeans(features.pca$x[,1:features.pc.choice], 5, nstart = 20)
  #features.kmeans.10 <- kmeans(features.pca$x[,1:features.pc.choice], 10, nstart = 20)
  #features.kmeans.15 <- kmeans(features.pca$x[,1:features.pc.choice], 15, nstart = 20)
  #features.kmeans.20 <- kmeans(features.pca$x[,1:features.pc.choice], 20, nstart = 20)
  r <- add(r, features.kmeans.5)
  r <- add(r, features.kmeans.10)
  r <- add(r, features.kmeans.15)
  r <- add(r, features.kmeans.20)

  # k-means silhouette
  features.kmeans.5.sil <- mcparallel(mean(silhouette(r$features.kmeans.5$cluster, r$features.pca.dist)[,3]))
  features.kmeans.10.sil <- mcparallel(mean(silhouette(r$features.kmeans.10$cluster, r$features.pca.dist)[,3]))
  features.kmeans.15.sil <- mcparallel(mean(silhouette(r$features.kmeans.15$cluster, r$features.pca.dist)[,3]))
  features.kmeans.20.sil <- mcparallel(mean(silhouette(r$features.kmeans.20$cluster, r$features.pca.dist)[,3]))
  #features.kmeans.5.sil <- silhouette(features.kmeans.5$cluster, features.pca.dist)
  #features.kmeans.10.sil <- silhouette(features.kmeans.10$cluster, features.pca.dist)
  #features.kmeans.15.sil <- silhouette(features.kmeans.15$cluster, features.pca.dist)
  #features.kmeans.20.sil <- silhouette(features.kmeans.20$cluster, features.pca.dist)
  r <- add(r, features.kmeans.5.sil)
  r <- add(r, features.kmeans.10.sil)
  r <- add(r, features.kmeans.15.sil)
  r <- add(r, features.kmeans.20.sil)

  # k-medians
  features.kmedians.5 <- mcparallel(kcca(r$features.pca$x[,1:pc], 5, family=kccaFamily("kmedians")))
  features.kmedians.10 <- mcparallel(kcca(r$features.pca$x[,1:pc], 10, family=kccaFamily("kmedians")))
  features.kmedians.15 <- mcparallel(kcca(r$features.pca$x[,1:pc], 15, family=kccaFamily("kmedians")))
  features.kmedians.20 <- mcparallel(kcca(r$features.pca$x[,1:pc], 20, family=kccaFamily("kmedians")))
  #features.kmedians.5 <- kcca(features.pca$x[,1:pc], 5, family=kccaFamily("kmedians"))
  #features.kmedians.10 <- kcca(features.pca$x[,1:pc], 10, family=kccaFamily("kmedians"))
  #features.kmedians.15 <- kcca(features.pca$x[,1:pc], 15, family=kccaFamily("kmedians"))
  #features.kmedians.20 <- kcca(features.pca$x[,1:pc], 20, family=kccaFamily("kmedians"))
  r <- add(r, features.kmedians.5)
  r <- add(r, features.kmedians.10)
  r <- add(r, features.kmedians.15)
  r <- add(r, features.kmedians.20)

  # k-medians silhouette
  features.kmedians.5.sil <- mcparallel(summary(silhouette(clusters(r$features.kmedians.5), r$features.pca.dist))$avg.width)
  features.kmedians.10.sil <- mcparallel(summary(silhouette(clusters(r$features.kmedians.10), r$features.pca.dist))$avg.width)
  features.kmedians.15.sil <- mcparallel(summary(silhouette(clusters(r$features.kmedians.15), r$features.pca.dist))$avg.width)
  features.kmedians.20.sil <- mcparallel(summary(silhouette(clusters(r$features.kmedians.20), r$features.pca.dist))$avg.width)
  #features.kmedians.5.sil <- silhouette(clusters(features.kmedians.5), features.pca.dist)
  #features.kmedians.10.sil <- silhouette(clusters(features.kmedians.10), features.pca.dist)
  #features.kmedians.15.sil <- silhouette(clusters(features.kmedians.15), features.pca.dist)
  #features.kmedians.20.sil <- silhouette(clusters(features.kmedians.20), features.pca.dist)
  r <- add(r, features.kmedians.5.sil)
  r <- add(r, features.kmedians.10.sil)
  r <- add(r, features.kmedians.15.sil)
  r <- add(r, features.kmedians.20.sil)

  # Fuzzy c-means
  features.cmeans.5 <- mcparallel(cmeans(r$features.pca$x[,1:pc], 5, m = 2))
  features.cmeans.10 <- mcparallel(cmeans(r$features.pca$x[,1:pc], 10, m = 2))
  features.cmeans.15 <- mcparallel(cmeans(r$features.pca$x[,1:pc], 15, m = 2))
  features.cmeans.20 <- mcparallel(cmeans(r$features.pca$x[,1:pc], 20, m = 2))
  #features.cmeans.5 <- cmeans(features.pca$x[,1:pc], 5, m = 2)
  #features.cmeans.10 <- cmeans(features.pca$x[,1:pc], 10, m = 2)
  #features.cmeans.15 <- cmeans(features.pca$x[,1:pc], 15, m = 2)
  #features.cmeans.20 <- cmeans(features.pca$x[,1:pc], 20, m = 2)
  r <- add(r, features.cmeans.5)
  r <- add(r, features.cmeans.10)
  r <- add(r, features.cmeans.15)
  r <- add(r, features.cmeans.20)

  # Fuzzy c-means silhouette
  features.cmeans.5.sil <- mcparallel(summary(silhouette(r$features.cmeans.5$cluster, r$features.pca.dist))$avg.width)
  features.cmeans.10.sil <- mcparallel(summary(silhouette(r$features.cmeans.5$cluster, r$features.pca.dist))$avg.width)
  features.cmeans.15.sil <- mcparallel(summary(silhouette(r$features.cmeans.5$cluster, r$features.pca.dist))$avg.width)
  features.cmeans.20.sil <- mcparallel(summary(silhouette(r$features.cmeans.5$cluster, r$features.pca.dist))$avg.width)
  #features.cmeans.5.sil <- silhouette(features.cmeans.5$cluster, features.pca.dist)
  #features.cmeans.10.sil <- silhouette(features.cmeans.10$cluster, features.pca.dist)
  #features.cmeans.15.sil <- silhouette(features.cmeans.15$cluster, features.pca.dist)
  #features.cmeans.20.sil <- silhouette(features.cmeans.20$cluster, features.pca.dist)
  r <- add(r, features.cmeans.5.sil)
  r <- add(r, features.cmeans.10.sil)
  r <- add(r, features.cmeans.15.sil)
  r <- add(r, features.cmeans.20.sil)

  # k-means bigrams
  features.kmeans.5.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.kmeans.5$cluster, as.character(headlines$headline_text)))
  features.kmeans.10.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.kmeans.10$cluster, as.character(headlines$headline_text)))
  features.kmeans.15.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.kmeans.15$cluster, as.character(headlines$headline_text)))
  features.kmeans.20.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.kmeans.20$cluster, as.character(headlines$headline_text)))
  #features.kmeans.5.bigrams <- get_cluster_freq_bigrams(features.kmeans.5$cluster, as.character(headlines$headline_text))
  #features.kmeans.10.bigrams <- get_cluster_freq_bigrams(features.kmeans.10$cluster, as.character(headlines$headline_text))
  #features.kmeans.15.bigrams <- get_cluster_freq_bigrams(features.kmeans.15$cluster, as.character(headlines$headline_text))
  #features.kmeans.20.bigrams <- get_cluster_freq_bigrams(features.kmeans.20$cluster, as.character(headlines$headline_text))
  r <- add(r, features.kmeans.5.bigrams)
  r <- add(r, features.kmeans.10.bigrams)
  r <- add(r, features.kmeans.15.bigrams)
  r <- add(r, features.kmeans.20.bigrams)

  # k-medians bigrams
  features.kmedians.5.bigrams <- mcparallel(get_cluster_freq_bigrams(clusters(r$features.kmedians.5), as.character(headlines$headline_text)))
  features.kmedians.10.bigrams <- mcparallel(get_cluster_freq_bigrams(clusters(r$features.kmedians.10), as.character(headlines$headline_text)))
  features.kmedians.15.bigrams <- mcparallel(get_cluster_freq_bigrams(clusters(r$features.kmedians.15), as.character(headlines$headline_text)))
  features.kmedians.20.bigrams <- mcparallel(get_cluster_freq_bigrams(clusters(r$features.kmedians.20), as.character(headlines$headline_text)))
  #features.kmedians.5.bigrams <- get_cluster_freq_bigrams(clusters(features.kmedians.5), as.character(headlines$headline_text))
  #features.kmedians.10.bigrams <- get_cluster_freq_bigrams(clusters(features.kmedians.10), as.character(headlines$headline_text))
  #features.kmedians.15.bigrams <- get_cluster_freq_bigrams(clusters(features.kmedians.15), as.character(headlines$headline_text))
  #features.kmedians.20.bigrams <- get_cluster_freq_bigrams(clusters(features.kmedians.20), as.character(headlines$headline_text))
  r <- add(r, features.kmedians.5.bigrams)
  r <- add(r, features.kmedians.10.bigrams)
  r <- add(r, features.kmedians.15.bigrams)
  r <- add(r, features.kmedians.20.bigrams)

  # Fuzzy c-means bigrams
  features.cmeans.5.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.cmeans.5$cluster, as.character(headlines$headline_text)))
  features.cmeans.10.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.cmeans.10$cluster, as.character(headlines$headline_text)))
  features.cmeans.15.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.cmeans.15$cluster, as.character(headlines$headline_text)))
  features.cmeans.20.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.cmeans.20$cluster, as.character(headlines$headline_text)))
  #features.cmeans.5.bigrams <- get_cluster_freq_bigrams(features.cmeans.5$cluster, as.character(headlines$headline_text))
  #features.cmeans.10.bigrams <- get_cluster_freq_bigrams(features.cmeans.10$cluster, as.character(headlines$headline_text))
  #features.cmeans.15.bigrams <- get_cluster_freq_bigrams(features.cmeans.15$cluster, as.character(headlines$headline_text))
  #features.cmeans.20.bigrams <- get_cluster_freq_bigrams(features.cmeans.20$cluster, as.character(headlines$headline_text))
  r <- add(r, features.cmeans.5.bigrams)
  r <- add(r, features.cmeans.10.bigrams)
  r <- add(r, features.cmeans.15.bigrams)
  r <- add(r, features.cmeans.20.bigrams)
  return(r)
}

# Sampling data to develop solution - only 10% of rows - comment to have full results
set.seed(42);
features.sample <- features[sample(1:nrow(features), nrow(features) * 0.08, replace = FALSE),]
r.sample <- run_analysis(features.sample, headlines, features.pc.choice, FALSE)

## 2016 Dataset
# 85% - PC 919
# 90% - PC 1084
features.2016.pc.choice <- 919
features.2016 <- features[headlines$publish_date$year + 1900 == 2016, ]
r.2016 <- run_analysis(features.2016, headlines, features.2016.pc.choice, FALSE)