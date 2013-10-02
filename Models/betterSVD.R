## Low-rank Matrix Completion
library(SparseM)
library(Matrix)
library(irlba)

## We want the data in its original matrix form
trmat <- read.csv("Data/train_ratings.csv")
trmat <- as.matrix(trmat)

## 0s in place of NA
trmat2 <- trmat
trmat2[is.na(trmat2)] <- 0

## Write new matrix
write.csv(trmat2, "Data/train_ratings2.csv")
trmat2 <- read.csv("Data/train_ratings2.csv")

##
trmat2 <- as.matrix(trmat2)

## Create Sparse matrix
sparseMat <- Matrix(trmat2)
nnzero(sparseMat)

sparsetest <- Matrix(trmat)

## SVD it
decompose <- irlba(sparseMat, nu = 50, nv = 50)
decomposetest <- irlba(sparsetest)















predmat2 <- complete(as.matrix(trmat), lol)
predmat2[which(predmat2 < 1)] <- 1
predmat2[which(predmat2 > 5)] <- 5
n <- dim(predmat2)[1]
p <- dim(predmat2)[2]
svd2 <- cbind(c(1:(n*p)), c(predmat2))
colnames(svd2) <- c("ID", "Rating")

##
write.csv(svd2, "Submissions/svd2.csv")

##
svd2 <- read.csv("Submissions/svd2.csv")
svd2 <- as.matrix(svd2)
svd2[, 3] <- round(svd2[, 3], 3)
svd2 <- svd2[, 2:3]

write.csv(svd2, "Submissions/svd2.csv", row.names = FALSE)