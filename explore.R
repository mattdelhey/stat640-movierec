### Define data location
d_in <- "Data/train_ratings.csv"

### Load in as matrix / df
trmat <- as.matrix(read.csv(d_in, header = TRUE))
trdf <- as.data.frame(trmat)

### Raw list of ratings of all movies
names(trdf) <- NULL
ratings <- unlist(trdf)
# Plot distribution
qplot(ratings) + ggtitle("Distribution of all ratings")
# Calculate rating proportions (out of all ratings)
table(ratings)/length(ratings[!is.na(ratings)])

# Mean ratings for each movie
mmean <- apply(trmat, 2, mean, na.rm = TRUE)
qplot(mmean) + ggtitle("Distribution of mean movie ratings")
