library(randomForest)
library(stringr)
library(reshape2)

# Read in clean data
tidy <- load("Data/Clean/tidy")
train <- load("Data/Clean/train")

# Create dummy vars (instead of factors due to limit of 32 catagorical vars)
y <- model.matrix(~ ., data = tidy)

# Train on all data
lm <- lm(Rating ~ Movie.ID, data = train)
svm <- svm(Rating ~ Movie.ID, data = train)
rf <- randomForest(Rating ~ User.ID + Movie.ID, data = train[!is.na(train$Rating), ],  ntree = 50)
rf <- train(Rating ~ User.ID + Movie.ID, data = train[!is.na(train$Rating), ]
