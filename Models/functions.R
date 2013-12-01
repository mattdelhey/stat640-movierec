force_bounds <- function(X, lower_bound = 1, upper_bound = 5) {
    X[X > upper_bound] <- upper_bound
    X[X < lower_bound] <- lower_bound
    return(X)
}

rmse <- function(y, y_pred, rmsd = FALSE) {
    sqrt(mean((y - y_pred)^2, na.rm = TRUE))
}

write_predmat <- function(X_out, f_out, write = TRUE, ret = FALSE) {
    n <- dim(X_out)[1]
    p <- dim(X_out)[2]
    bench <- cbind(c(1:(n*p)), c(X_out))
    colnames(bench) <- c("ID", "Rating")
    if (write) {
        write.csv(bench, file = f_out, quote = FALSE, row.names = FALSE)
    }
    if (ret) {
        return(bench)
    }
}

cosineDist <- function(x){
  as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}
