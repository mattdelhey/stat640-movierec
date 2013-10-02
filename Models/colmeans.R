##########################
# Rice Stat 640, Fall 2013
# R script for Benchmark - MovieLens data set
# Predict by movie means
# Matt: added basic evaluation
##########################
library(plyr)

f_out <- "Submissions/colmeans_usermeans_centered.csv"
d_in <- "Data/train_ratings.csv"
trmat <- as.matrix(read.csv(d_in, header = TRUE))

# Create predicition matrix (n x p)
mmean <- apply(trmat, 2, mean, na.rm = TRUE)
predmat <- matrix(1, nrow(trmat)) %x% t(mmean)

# Find user difference from mean
mean_rating <- mean(as.vector(trmat), na.rm = TRUE)
umean <- apply(trmat, 1, mean, na.rm = TRUE)
dmean <- umean - mean_rating

## dmean2 <- rep(NA, nrow(trmat))
## for (i in 1:nrow(trmat)) {
##     ur_movies <- names(trmat[i, !is.na(trmat[i, ])])
##     mean_rating_subset <- mean(trmat[, ur_movies], na.rm = TRUE)
##     dmean2[i] <- (umean - mean_rating_subset)
## }

dmat <- matrix(dmean, nrow = nrow(trmat), ncol = ncol(trmat))
#dmat <- matrix(dmean2, nrow = nrow(trmat), ncol = ncol(trmat))

# Create new predmat
predmat <- predmat + dmat

# Keep predictions for eval
predmat_eval <- predmat
# Replace predictions with known review scores (for benchmark)
predmat[!is.na(trmat)] <- trmat[!is.na(trmat)]

# Create prediction (review id, pred)
n <- dim(predmat)[1]
p <- dim(predmat)[2]
bench <- cbind(c(1:(n*p)), c(predmat))
colnames(bench) <- c("ID", "Rating")
#write.csv(bench, file = f_out, quote = FALSE, row.names = FALSE)

# Generate training (true) values (review id, true)
train <- cbind(c(1:(n*p)), c(trmat))
train <- train[!is.na(train[, 2]), ]  # remove NA's

# Convert data-matricies to dataframes
bench_eval <- as.data.frame(cbind(c(1:(n*p)), c(predmat_eval)))
names(bench_eval) <- c("ID", "Pred.Rating")
train <- as.data.frame(train)
names(train) <- c("ID", "True.Rating")

# Define rmse function
rmse <- function(y, y_pred, rmsd = FALSE) {
    sqrt(mean((y - y_pred)^2, na.rm = TRUE))
}

# Evaluate on training data
bench_eval <- subset(bench_eval, ID %in% train$ID)
bench_eval$True.Rating <- train$True.Rating
train_rmse <- rmse(bench_eval$True.Rating, bench_eval$Pred.Rating)

# Not worth it to determine test error. 
