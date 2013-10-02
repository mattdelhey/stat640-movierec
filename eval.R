rmse <- function(y, y_pred, rmsd = FALSE) {
    sqrt(mean((y - y_pred)^2, na.rm = TRUE))
}

sample_rows <- function(n, p = 0.9) {
    nkeep <- round(n * p)
    sample(rep(c(TRUE, FALSE), c(nkeep, n - nkeep)))
}

cv_rmse <- function(mod, data, n = 10, p = 0.9) {
    replicate(n, {
        # Generate train rows
        train_rows <- sample_rows(nrow(data), p)
        #train model on new train data
        rmse(y,
    })
}
