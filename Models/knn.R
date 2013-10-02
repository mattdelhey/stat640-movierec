### Load in raw data
d_in <- "Data/train_ratings.csv"
df <- read.csv(d_in, header = TRUE)
#c_in <- "Data/Clean/"
#load(paste(c_in, "train_clean.RData", sep = ""))
#load(paste(c_in, "test_clean.RData", sep = ""))

### Create correlations
fd <- t(df)
c <- cor(t(df), use = "pairwise.complete.obs", method = "pearson")
#ck <- cor(t(df), use = "pairwise.complete.obs", method = "kendall")
#cs <- cor(t(df), use = "pairwise.complete.obs", method = "spearman")

c <- as.data.frame(matrix(0, nrow = nrow(df), ncol = nrow(df)))
for (i in 1:1) {
    for (j in 1:nrow(df)) {
        x <- as.numeric(df[i, ])
        y <- as.numeric(df[j, ])
        c[i, j] <- cov(x, y, use = "pairwise.complete.obs", method = "pearson")
    }
}

