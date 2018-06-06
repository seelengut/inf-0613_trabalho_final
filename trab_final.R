########################################
# Trabalho Final - INF-0613          
# Nome(s): Felipe Wolff Ramos
#          Lucas Aoki Heredia
########################################

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


# Sampling data to develop solution
set.seed(42); features.sample <- features[sample(1:nrow(features), nrow(features) * 0.1, replace = FALSE),]
