## Attempt to cluster either users or movies in order to train smaller models
#library(plyr)
library(ggplot2)
#library(reshape2)

## Load the cleaned data
#load("Data/Clean/train_clean.RData")
#load("Data/Clean/test_clean.RData")

## Naive k-means
#lol <- randomForest(Rating ~ Year + Genre + Gender + Age, data = train)

library(softImpute)
d_in <- "Data/train_ratings.csv"
setwd("~/stat640-movierec")
trmat <- as.matrix(read.csv(d_in, header = TRUE))

# Our imputed matrix
X <- biScale(trmat, maxit = 50, thresh = 1e-10)
lam <- lambda0(X, lambda = 0, maxit = 1000,
               trace.it = FALSE, thresh = 1e-06)
X_fit <- softImpute(X, rank.max = min(dim(X)), lambda = lam,
                    type = "svd", thresh = 1e-06, maxit = 1000)
X_out <- complete(trmat, X_fit)
X_out[X_out > 5] <- 5
X_out[X_out < 1] <- 1

# PCA
pca <- princomp(x = X_out)
plot(pca)
scores <- pca$scores
qplot(x = scores[, 1], y = scores[, 2])

# Read in movieInfo
u_in <- "Data/userInfo.csv"
u <- read.csv(u_in, header = TRUE)
qplot(x = scores[, 1], y = scores[, 2], fill = u$Gender)

sg <- merge(scores, u$Gender)
qplot(x = sg$)
