## Attempt to cluster either users or movies in order to train smaller models
library(plyr)
library(ggplot2)
library(reshape2)

## Load the cleaned data
load("Data/Clean/train_clean.RData")
load("Data/Clean/test_clean.RData")

## Naive k-means
lol <- randomForest(Rating ~ Year + Genre + Gender + Age, data = train)