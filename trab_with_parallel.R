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
library(ggplot2)

# Set Working Dir
# setwd("/Users/wfr005/work/courses/inf-0613/t_final")

# Increase max print
options(max.print=10000)

# Load csv files
features <- read.csv("features.csv", header = TRUE, sep = ",")
headlines <- read.csv("headlines.csv", header = TRUE, sep = ",")
headlines$publish_date <- strptime(headlines$publish_date, "%Y%m%d")

# Bigrams calculation
get_bigram_freq <- function(headlines) {
  # acquire all possible bigrams
  print("calc all bigrams")
  all_bigrams <- c()
  for (headline in headlines) {
    all_bigrams <- c(all_bigrams, get_bigrams(headline))
  }
  print("calc unique bigrams")
  unique_bigrams <- unique(all_bigrams)
  
  # Named vector to count bigrams frequency
  result <- rep(0, length(unique_bigrams))
  names(result) <- unique_bigrams
  # Count bigrams occurrence
  print(length(all_bigrams))
  
  r1 <- mcparallel(sum_bigrams(result, unique_bigrams[1:(length(unique_bigrams)/2)], all_bigrams))
  r2 <- mcparallel(sum_bigrams(result, unique_bigrams[(length(unique_bigrams)/2 + 1):length(unique_bigrams)], all_bigrams))
  
  result <- c(mccollect(r1)[[1]], mccollect(r2)[[1]])
  return(result)
}

sum_bigrams <- function(result, unique_bigrams, all_bigrams) {
  print("calc sum of bigrams")
  for (bigram in unique_bigrams) {
    result[bigram] <- sum(bigram == all_bigrams)
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
    print(headlines[cluster == c])
    bigrams <- get_bigram_freq(headlines[cluster == c])
    bigrams <- sort(bigrams, decreasing = TRUE)
    # print(bigrams)
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

run_analysis <- function(features, headlines, pc, sc, features.pca = NULL) {
  r <- list()

  if (is.null(features.pca)) {
    features.pca <- prcomp(features, scale. = sc)    
  }
  
  features.pca.dist <- dist(features.pca$x[,1:pc])
  r <- add(r, features.pca)
  r <- add(r, features.pca.dist)

  # k-means clustering
  features.kmeans.5 <- mcparallel(kmeans(r$features.pca$x[,1:pc], 5, nstart = 20))
  features.kmeans.10 <- mcparallel(kmeans(r$features.pca$x[,1:pc], 10, nstart = 20))
  features.kmeans.15 <- mcparallel(kmeans(r$features.pca$x[,1:pc], 15, nstart = 20))
  features.kmeans.20 <- mcparallel(kmeans(r$features.pca$x[,1:pc], 20, nstart = 20))
  r <- add(r, features.kmeans.5)
  r <- add(r, features.kmeans.10)
  r <- add(r, features.kmeans.15)
  r <- add(r, features.kmeans.20)

  # k-means silhouette
  features.kmeans.5.sil <- mcparallel(mean(silhouette(r$features.kmeans.5$cluster, r$features.pca.dist)[,3]))
  features.kmeans.10.sil <- mcparallel(mean(silhouette(r$features.kmeans.10$cluster, r$features.pca.dist)[,3]))
  features.kmeans.15.sil <- mcparallel(mean(silhouette(r$features.kmeans.15$cluster, r$features.pca.dist)[,3]))
  features.kmeans.20.sil <- mcparallel(mean(silhouette(r$features.kmeans.20$cluster, r$features.pca.dist)[,3]))
  r <- add(r, features.kmeans.5.sil)
  r <- add(r, features.kmeans.10.sil)
  r <- add(r, features.kmeans.15.sil)
  r <- add(r, features.kmeans.20.sil)

  # k-medians
  features.kmedians.5 <- mcparallel(kcca(r$features.pca$x[,1:pc], 5, family=kccaFamily("kmedians")))
  features.kmedians.10 <- mcparallel(kcca(r$features.pca$x[,1:pc], 10, family=kccaFamily("kmedians")))
  features.kmedians.15 <- mcparallel(kcca(r$features.pca$x[,1:pc], 15, family=kccaFamily("kmedians")))
  features.kmedians.20 <- mcparallel(kcca(r$features.pca$x[,1:pc], 20, family=kccaFamily("kmedians")))
  r <- add(r, features.kmedians.5)
  r <- add(r, features.kmedians.10)
  r <- add(r, features.kmedians.15)
  r <- add(r, features.kmedians.20)

  # k-medians silhouette
  features.kmedians.5.sil <- mcparallel(summary(silhouette(clusters(r$features.kmedians.5), r$features.pca.dist))$avg.width)
  features.kmedians.10.sil <- mcparallel(summary(silhouette(clusters(r$features.kmedians.10), r$features.pca.dist))$avg.width)
  features.kmedians.15.sil <- mcparallel(summary(silhouette(clusters(r$features.kmedians.15), r$features.pca.dist))$avg.width)
  features.kmedians.20.sil <- mcparallel(summary(silhouette(clusters(r$features.kmedians.20), r$features.pca.dist))$avg.width)
  r <- add(r, features.kmedians.5.sil)
  r <- add(r, features.kmedians.10.sil)
  r <- add(r, features.kmedians.15.sil)
  r <- add(r, features.kmedians.20.sil)

  # Fuzzy c-means
  features.cmeans.5 <- mcparallel(cmeans(r$features.pca$x[,1:pc], 5, m = 2))
  features.cmeans.10 <- mcparallel(cmeans(r$features.pca$x[,1:pc], 10, m = 2))
  features.cmeans.15 <- mcparallel(cmeans(r$features.pca$x[,1:pc], 15, m = 2))
  features.cmeans.20 <- mcparallel(cmeans(r$features.pca$x[,1:pc], 20, m = 2))
  r <- add(r, features.cmeans.5)
  r <- add(r, features.cmeans.10)
  r <- add(r, features.cmeans.15)
  r <- add(r, features.cmeans.20)

  # Fuzzy c-means silhouette
  features.cmeans.5.sil <- mcparallel(summary(silhouette(r$features.cmeans.5$cluster, r$features.pca.dist))$avg.width)
  features.cmeans.10.sil <- mcparallel(summary(silhouette(r$features.cmeans.5$cluster, r$features.pca.dist))$avg.width)
  features.cmeans.15.sil <- mcparallel(summary(silhouette(r$features.cmeans.5$cluster, r$features.pca.dist))$avg.width)
  features.cmeans.20.sil <- mcparallel(summary(silhouette(r$features.cmeans.5$cluster, r$features.pca.dist))$avg.width)
  r <- add(r, features.cmeans.5.sil)
  r <- add(r, features.cmeans.10.sil)
  r <- add(r, features.cmeans.15.sil)
  r <- add(r, features.cmeans.20.sil)

  # k-means bigrams
  features.kmeans.5.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.kmeans.5$cluster, as.character(headlines$headline_text)))
  features.kmeans.10.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.kmeans.10$cluster, as.character(headlines$headline_text)))
  features.kmeans.15.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.kmeans.15$cluster, as.character(headlines$headline_text)))
  features.kmeans.20.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.kmeans.20$cluster, as.character(headlines$headline_text)))
  r <- add(r, features.kmeans.5.bigrams)
  r <- add(r, features.kmeans.10.bigrams)
  r <- add(r, features.kmeans.15.bigrams)
  r <- add(r, features.kmeans.20.bigrams)

  # k-medians bigrams
  features.kmedians.5.bigrams <- mcparallel(get_cluster_freq_bigrams(clusters(r$features.kmedians.5), as.character(headlines$headline_text)))
  features.kmedians.10.bigrams <- mcparallel(get_cluster_freq_bigrams(clusters(r$features.kmedians.10), as.character(headlines$headline_text)))
  features.kmedians.15.bigrams <- mcparallel(get_cluster_freq_bigrams(clusters(r$features.kmedians.15), as.character(headlines$headline_text)))
  features.kmedians.20.bigrams <- mcparallel(get_cluster_freq_bigrams(clusters(r$features.kmedians.20), as.character(headlines$headline_text)))
  r <- add(r, features.kmedians.5.bigrams)
  r <- add(r, features.kmedians.10.bigrams)
  r <- add(r, features.kmedians.15.bigrams)
  r <- add(r, features.kmedians.20.bigrams)

  # Fuzzy c-means bigrams
  features.cmeans.5.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.cmeans.5$cluster, as.character(headlines$headline_text)))
  features.cmeans.10.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.cmeans.10$cluster, as.character(headlines$headline_text)))
  features.cmeans.15.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.cmeans.15$cluster, as.character(headlines$headline_text)))
  features.cmeans.20.bigrams <- mcparallel(get_cluster_freq_bigrams(r$features.cmeans.20$cluster, as.character(headlines$headline_text)))
  r <- add(r, features.cmeans.5.bigrams)
  r <- add(r, features.cmeans.10.bigrams)
  r <- add(r, features.cmeans.15.bigrams)
  r <- add(r, features.cmeans.20.bigrams)
  return(r)
}

# 2016 data set
features.2016 <- features[headlines$publish_date$year + 1900 == 2016, ]

features.scaled.pca <- mcparallel(prcomp(features, scale. = TRUE))
features.nonscaled.pca <- mcparallel(prcomp(features, scale. = FALSE))
features.2016.pca <- mcparallel(prcomp(features.2016, scale. = FALSE))

pca <- list()
pca <- add(pca, features.scaled.pca)
pca <- add(pca, features.nonscaled.pca)
pca <- add(pca, features.2016.pca)

# Summary PCA with scale
# 85% - 1654
# 90% - 1804
pca$features.scaled.pca.summary <- summary(pca$features.scaled.pca)
pca$features.scaled.pc.choice <- 1654

# Summary PCA without scale
# 85% - 1390
# 90% - 1598
pca$features.nonscaled.pca.summary <- summary(pca$features.nonscaled.pca)
pca$features.nonscaled.pc.choice <- 1390

# Summary 2016 PCA without scale
# 85% - PC 919
# 90% - PC 1084
pca$features.2016.pca.summary <- summary(pca$features.2016.pca)
pca$features.2016.pc.choice <- 919

# Sampling data to develop solution - only 10% of rows
# set.seed(42);
# features.sample <- features[sample(1:nrow(features), nrow(features) * 0.1, replace = FALSE),]
# r.sample <- run_analysis(features.sample, headlines, pca$features.nonscaled.pc.choice, FALSE, pca$features.nonscaled.pca)

# Analyze complete set non scaled
r.nonscaled <- run_analysis(features, headlines, pca$features.nonscaled.pc.choice, FALSE, pca$features.nonscaled.pca)

# Analyze complete set scaled
r.scaled<- run_analysis(features, headlines, pca$features.scaled.pc.choice, TRUE, pca$features.scaled.pca)

## Analyze 2016 set
r.2016 <- run_analysis(features.2016, headlines, pca$features.2016.pc.choice, FALSE, pca$features.2016.pca)



# Silhouette graph
graph.kmeans.nscaled.sil <- c(r.nonscaled$features.kmeans.5.sil, r.nonscaled$features.kmeans.10.sil,
                              r.nonscaled$features.kmeans.15.sil, r.nonscaled$features.kmeans.20.sil)

graph.kmeans.scaled.sil <- c(r.scaled$features.kmeans.5.sil, r.scaled$features.kmeans.10.sil,
                             r.scaled$features.kmeans.15.sil, r.scaled$features.kmeans.20.sil)

graph.kmedians.nscaled.sil <- c(r.nonscaled$features.kmedians.5.sil, r.nonscaled$features.kmedians.10.sil,
                                r.nonscaled$features.kmedians.15.sil, r.nonscaled$features.kmedians.20.sil)

graph.kmedians.scaled.sil <- c(r.scaled$features.kmedians.5.sil, r.scaled$features.kmedians.10.sil,
                               r.scaled$features.kmedians.15.sil, r.scaled$features.kmedians.20.sil)

graph.kmedians.nscaled.sil <- c(r.nonscaled$features.kmedians.5.sil, r.nonscaled$features.kmedians.10.sil,
                                r.nonscaled$features.kmedians.15.sil, r.nonscaled$features.kmedians.20.sil)

graph.kmedians.scaled.sil <- c(r.scaled$features.kmedians.5.sil, r.scaled$features.kmedians.10.sil,
                               r.scaled$features.kmedians.15.sil, r.scaled$features.kmedians.20.sil)

graph.cmeans.nscaled.sil <- c(r.nonscaled$features.cmeans.5.sil, r.nonscaled$features.cmeans.10.sil,
                              r.nonscaled$features.cmeans.15.sil, r.nonscaled$features.cmeans.20.sil)

graph.cmeans.scaled.sil <- c(r.scaled$features.cmeans.5.sil, r.scaled$features.cmeans.10.sil,
                             r.scaled$features.cmeans.15.sil, r.scaled$features.cmeans.20.sil)



graph.sil.data <- c(graph.kmeans.nscaled.sil, graph.kmeans.scaled.sil,
                    graph.kmedians.nscaled.sil, graph.kmedians.scaled.sil,
                    graph.cmeans.nscaled.sil, graph.cmeans.scaled.sil)

graph.sil.keys <- rep(c('kmeans - non scaled', 'kmeans - scaled', 
                        'kmedians - non scaled', 'kmedians - scaled',
                        'fuzzy cmeans - non scaled', 'fuzzy cmeans - scaled'), times=1, each=4)



graph.sil.dframe <- data.frame(clusters=c(5,10,15,20), silhouette=graph.sil.data, keys=graph.sil.keys)
g <- ggplot(data = graph.sil.dframe , aes(x=clusters, y=silhouette, colour=keys))
g <- g + geom_point() + geom_line()
g <- g + labs(title="Variação da Silhueta x Número de Clusters", y="Silhueta", x="Total de Clusters", colour="")
g <- g + theme(plot.title = element_text(hjust = 0.5, size = 18),
               axis.text = element_text(size=12),
               axis.title = element_text(size=16),
               legend.text = element_text(size=12),
               legend.position = "bottom")
# Generate graph
g

# Quadractic error calculation
get_kmeans_error_data <- function(data) {
  totss <- c(data$features.kmeans.5$totss, data$features.kmeans.10$totss,
                                  data$features.kmeans.15$totss, data$features.kmeans.20$totss)
  betweenss <- c(data$features.kmeans.5$betweenss, data$features.kmeans.10$betweenss,
                                      data$features.kmeans.15$betweenss, data$features.kmeans.20$betweenss)
  withinss <- c(data$features.kmeans.5$tot.withinss, data$features.kmeans.10$tot.withinss,
                                     data$features.kmeans.15$tot.withinss, data$features.kmeans.20$tot.withinss)
  betss_div_totss <- betweenss / totss
  return(data.frame(betss_div_totss, withinss ))
}

get_kcca_error_data <- function(data) {
  withinss <- c(info(data$features.kmedians.5, "distsum"), info(data$features.kmedians.10, "distsum"),
                info(data$features.kmedians.15, "distsum"), info(data$features.kmedians.20, "distsum"))
  betss_div_totss <- 0
  return(data.frame(betss_div_totss, withinss ))
}

get_cmeans_error_data <- function(data) {
  withinss <- c(data$features.cmeans.5$withinerror, data$features.cmeans.10$withinerror,
                data$features.cmeans.15$withinerror, data$features.cmeans.20$withinerror)
  betss_div_totss <- 0
  return(data.frame(betss_div_totss, withinss ))
}

graph.kmeans.nscaled.error <- get_kmeans_error_data(r.nonscaled)
graph.kmeans.scaled.error  <- get_kmeans_error_data(r.scaled)

graph.kmedians.nscaled.error <- get_kcca_error_data(r.nonscaled)
graph.kmedians.scaled.error <- get_kcca_error_data(r.scaled)

graph.cmeans.nscaled.error <- get_cmeans_error_data(r.nonscaled)
graph.cmeans.scaled.error <- get_cmeans_error_data(r.scaled)
