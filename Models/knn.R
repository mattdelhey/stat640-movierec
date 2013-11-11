library(softImpute)
library(lsa)
setwd("~/stat640-movierec/")
source("Models/functions.R")
d_in <- "Data/train_ratings.csv"

### Load in raw data
df <- read.csv(d_in, header = TRUE)
X <- as.matrix(df)

###
### Preproccessing
###

### Normalization (double centering)
R <- biScale(X, maxit = 500, thresh = 1e-10)

### Calculate similarities
# s_ij = |U(i,j)| / 
U <- matrix(nrow = dim(R)[2], ncol = dim(R)[2])
for (i in 1:dim(R)[2]) {
    x_i <- which(!is.na(R[, i]))
    for (j in 1:dim(R)[2]) {
        y_j <- which(!is.na(R[, j]))
        U[i, j] <- length(intersect(x_i, y_j))
    }
}


### Create correlations
fd <- t(df)
c <- cor(t(df), use = "pairwise.complete.obs", method = "pearson")
#ck <- cor(t(df), use = "pairwise.complete.obs", method = "kendall")
cs <- cor(R[c(1,2), ], use = "pairwise.complete.obs", method = "spearman")

c <- as.data.frame(matrix(0, nrow = nrow(df), ncol = nrow(df)))
for (i in 1:1) {
    for (j in 1:nrow(df)) {
        x <- as.numeric(df[i, ])
        y <- as.numeric(df[j, ])
        c[i, j] <- cov(x, y, use = "pairwise.complete.obs", method = "pearson")
    }
}

### Imputation
library(imputation)
imp_knn <- cv.kNNImpute(df, k.max = 2)

### Model Assessment

###
### recommenderlab
###
# Assume we have data matrix X
library(recommenderlab)
# Create rating object, look at rating distribution
R <- as(X, "realRatingMatrix")
hist(getRatings(R), breaks = "FD")
# Normalize / bin R 
R <- normalize(R)
#R <- binarize(R, minRating = 4)
# List of possible models
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# Run item-wise (movie-wise) cf/knn
r <- Recommender(R, method = "IBCF", param = list(k = 20, na_as_zero = FALSE, minRating = 0))
#r <- Recommender(R, method = "UBCF")
names(getModel(r)); getModel(r)$k
# Make predictions (k = 30)
pred <- predict(r, newdata = R, type = "ratings")
predmat <- as(pred, "matrix")
predmat[!is.na(X)] <- X[!is.na(X)]
predmat <- force_bounds(predmat)
