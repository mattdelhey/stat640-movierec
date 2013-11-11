force_bounds <- function(X, lower_bound = 1, upper_bound = 5) {
    X[X > upper_bound] <- upper_bound
    X[X < lower_bound] <- lower_bound
    return(X)
}

rmse <- function(y, y_pred, rmsd = FALSE) {
    sqrt(mean((y - y_pred)^2, na.rm = TRUE))
}

cosineDist <- function(x){
  as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}


