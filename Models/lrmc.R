## Low-rank Matrix Completion
library(SparseM)
library(Matrix)
library(irlba)

## We want the data in its original matrix form
trmat <- read.csv("Data/train_ratings.csv")

## 0s in place of NA
trmat2 <- trmat
trmat2[is.na(trmat2)] <- 0

## Write new matrix
write.csv(trmat2, "Data/train_ratings2.csv")
trmat2 <- read.csv("Data/train_ratings2.csv")

##
trmat <- as.matrix(trmat)
trmat2 <- as.matrix(trmat2)

## Get the NA for later
nas <- which(is.na(trmat))

## Create Sparse Matrix
trmat2 <- Matrix(as.matrix(trmat2))
trmat <- Matrix(trmat)

## svd
udv <- svd(trmat2)

##irlba
partialSvd <- irlba(trmat2, 300, 300)

## Generate predictions on missing values
u <- partialSvd$u
d <- partialSvd$d
v <- partialSvd$v

## same
u <- udv$u
d <- diag(udv$d)
v <- udv$v

quack <- u %*% d %*% Conj(t(v))

## Fill in our matrix through irlba
numRows <- nrow(trmat2)
numCol <- ncol(trmat2)

trmat3 <- as.matrix(trmat2)

for(i in 1:numRows) {
  for(j in 1:numCol) {
    if(trmat3[i, j] == 0) {
    trmat3[i, j] <- sum(u[i, ] * v[j, ])
    }
  }
}


## Generate prediction matrix
predmat <- as.matrix(trmat3)
n <- dim(predmat)[1]
p <- dim(predmat)[2]
svd1<- cbind(c(1:(n*p)), c(predmat))
colnames(svd1) <- c("ID", "Rating")

## Write predictions
write.csv(svd1, "Submissions/svd1.csv")

##
library(softImpute)
lol <- softImpute(as.matrix(trmat), rank.max = 200, lambda = 150)

predmat2 <- complete(as.matrix(trmat), lol)
predmat2[which(predmat2 < 1)] <- 1
predmat2[which(predmat2 > 5)] <- 5
n <- dim(predmat2)[1]
p <- dim(predmat2)[2]
svd2 <- cbind(c(1:(n*p)), c(predmat2))
colnames(svd2) <- c("ID", "Rating")

##
write.csv(svd2, "Submissions/svd3.csv", row.names = FALSE)

##
svd2 <- read.csv("Submissions/svd2.csv")
svd2 <- as.matrix(svd2)
svd2[, 3] <- round(svd2[, 3], 3)
svd2 <- svd2[, 2:3]

write.csv(svd2, "Submissions/svd2.csv", row.names = FALSE)