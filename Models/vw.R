### Convert R data into vw format

# Define i/o parameters
c_in   <- "Data/Clean/"
vw_out <- "Data/VW/"

##############
############## PRE-VW
##############

# Load in train / test data
load(paste(c_in, "train_clean.RData", sep = ""))
load(paste(c_in, "test_clean.RData", sep = ""))

create_vw <- function(df, f_out, test = FALSE) {
    # Append NULL labels for testing data
    if (test) {
        df$Rating <- 0
    }
    
    # Concat to each response:
    #  1. "1.0" to indicate a weight of 1
    #  2. ID as the response tag (well maybe not...)
    df$Rating <- paste(df$Rating, "1.0", df$ID)
    
    # 1. Order columns with response (Rating) first
    # 2. Remove Title and ID columns/features
    df <- df[, ]
    
    # Quickly convert Gender to numeric
    df$Gender <- as.numeric(df$Gender)
    
    # Concat a label to each feature (manual for now)
    #features <- 2:dim(df)[2]
    #feature_names <- names(df[, features])
    df$User.ID  <- paste("User.ID", df$User.ID, "")
    df$Movie.ID <- paste("Movie.ID", df$Movie.ID, "")
    df$Year     <- paste("Year", df$Year, "")
    df$Genre    <- paste("Genre", df$Genre, "")
    df$Gender   <- paste("Gender", df$Gender, "")
    df$Age      <- paste("Age", df$Age, "")
    df$Occupation <- paste("Occupation", df$Occupation, "")

    # Features: User.ID, Movie.ID
    df <- df[, c("Rating", "User.ID", "Movie.ID")]
    
    # Write df / test to vw format
    write.table(df, quote = FALSE, sep = "|",
                col.names = FALSE, row.names = FALSE,
                file = paste(vw_out, f_out, sep = ""))
}

create_vw(train, f_out = "train_small.vw")
create_vw(test,  f_out = "test_small.vw", test = TRUE)

##############
############## POST-VW
##############
library(plyr)

logistic <- read.table("Data/VW/preds_logistic.txt", header = FALSE)
names(logistic) <- c("Rating", "ID")
logistic <- logistic[, c("ID", "Rating")]
write.csv(logistic, file = "Submissions/vw_class_logistic.csv", row.names = FALSE)

squared <- read.table("Data/VW/preds_squared.txt", header = FALSE)
names(squared) <- c("Rating", "ID")
squared <- squared[, c("ID", "Rating")]
jt <- train[, c("ID", "Rating")]
j <- arrange(join(squared, jt, type = "full", match = "first"), ID)
write.csv(j, file = "Submissions/vw_class_squared.csv", row.names = FALSE)

reg <- read.table("Data/VW/preds_reg_squared.txt", header = FALSE)
names(reg) <- c("Rating", "ID")
reg <- reg[, c("ID", "Rating")]
jt <- train[, c("ID", "Rating")]
j <- arrange(join(reg, jt, type = "full", match = "first"), ID)
write.csv(j, file = "Submissions/vw_reg_squared.csv", row.names = FALSE)
