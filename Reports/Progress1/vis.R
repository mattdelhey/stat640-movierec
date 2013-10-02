library(ggplot2)
err <- read.csv("../../Submissions/whitehouse_progress_movielens.csv")

ggplot(data = err) +
    geom_bar(aes(y = Training.Err))
