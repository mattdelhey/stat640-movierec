library(plyr)
library(stringr)
library(reshape2)

# Define i/o parameters
d_in  <- "Data/train_ratings.csv"
id_in <- "Data/ids_movielens.csv"
u_in  <- "Data/userInfo.csv"
m_in  <- "Data/movieInfo.csv"
clean_out <- "Data/Clean/"

# Read in data basic data
df <- read.csv(d_in, header = TRUE)
id <- read.csv(id_in, header = TRUE)

###
### Configure tidy df (submission format; train_small):
### ID | User.ID | Movie.ID | Rating
df$User.ID <- 1:nrow(df)
tidy <- melt(df, id = "User.ID")
tidy$ID <- 1:nrow(tidy)

# Fix names and column ordering
names(tidy) <- c("User.ID", "Movie.ID", "Rating", "ID")
tidy <- tidy[, c("ID", "User.ID", "Movie.ID", "Rating")]

# Fix factors (keep ratings as INT)
levels(tidy$Movie.ID) <- str_replace(levels(tidy$Movie.ID), "X", "")
tidy$User.ID <- as.factor(tidy$User.ID)
tidy$ID <- as.factor(tidy$ID)

###
### Now let's deal with some of the extra info:
### Train large: add data on about Users & Movies [row-major format]
u <- read.csv(u_in, header = TRUE)
m <- read.csv(m_in, header = TRUE)

# A bit of cleaning first. Create Year column and remove from title.
m$Year <- str_extract(str_extract(m$Title, "[(0-9)]{6,6}"), "[0-9]{4,4}")
m$Year <- as.integer(as.numeric(m$Year))  # Keep year as int.

# Only take first genre and number of genres
m$Genre <- as.factor(m$Genre.1)
m <- subset(m, select = -c(Genre.1, Genre.2, Genre.3, Genre.4, Genre.5, Genre.6))

# Fix up user info
u$Age <- as.factor(u$Age)
u$Occupation <- as.factor(u$Occupation)

### Join with tidy
tidy <- join(tidy, m, type = "left", match = "all")
tidy <- join(tidy, u, type = "left", match = "all")

### Define train / test
train <- subset(tidy, !is.na(Rating))  # "data we have ratings for"
test  <- subset(tidy, is.na(Rating))    # "data we need to predict"

### Save as R Objects
save(train, file = paste(clean_out, "train_clean.RData", sep = ""))
save(test,  file = paste(clean_out, "test_clean.RData", sep = ""))
