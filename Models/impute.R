# Attempt at matrix completion
library(softImpute)

# i/o params
f_out <- "Submissions/softimpute_blend.csv"
d_in <- "Data/train_ratings.csv"

# Working directory (for cloud)
setwd("~/stat640-movierec")
source("Models/functions.R")

# Load in data
trmat <- as.matrix(read.csv(d_in, header = TRUE))

###
### Model Fitting
###
# Center data
X <- biScale(trmat, maxit = 50, thresh = 1e-10)

# Find best lambda
lam <- lambda0(X, lambda = 0, maxit = 1000,
               trace.it = FALSE, thresh = 1e-06)

# Fit & Complete
X_fit <- softImpute(X, rank.max = min(dim(X)), lambda = lam,
                    type = "svd", thresh = 1e-06, maxit = 1000)
X_out <- complete(trmat, X_fit)

# Fix values less than 1, greater than 5
X_out <- force_bounds(X_out)

# Write to file
n <- dim(X_out)[1]
p <- dim(X_out)[2]
bench <- cbind(c(1:(n*p)), c(X_out))
colnames(bench) <- c("ID", "Rating")
write.csv(bench, file = f_out, quote = FALSE, row.names = FALSE)

### Imputing
#library(imputation)
#X_fit2 <- cv.SVDImpute(X, k = dim(X)[2])

###
### Model Validation
###
## X_val <- trmat[!is.na(trmat)]
## X_val <- complete(trmat, X_fit)

## # Generate training (true) values (review id, true)
## train <- cbind(c(1:(n*p)), c(trmat))
## train <- train[!is.na(train[, 2]), ]  # remove NA's

## # Convert data-matricies to dataframes
## bench_eval <- as.data.frame(cbind(c(1:(n*p)), c(predmat_eval)))
## names(bench_eval) <- c("ID", "Pred.Rating")
## train <- as.data.frame(train)
## names(train) <- c("ID", "True.Rating")

## # Evaluate on training data
## bench_eval <- subset(bench_eval, ID %in% train$ID)
## bench_eval$True.Rating <- train$True.Rating
## train_rmse <- rmse(bench_eval$True.Rating, bench_eval$Pred.Rating)
