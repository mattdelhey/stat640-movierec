library(ggplot2)
library(lubridate)
setwd("~/stat640-movierec")
err <- read.csv("Submissions/charlemagne_and_the_carolingian_empire_progress_movielens.csv", stringsAsFactors = FALSE)

library(RColorBrewer)
pal <- brewer.pal(3, "Paired")

err$Date <- as.Date(err$Date, "%m-%d")
err <- err[order(err$Date), ]

ggplot(data = err, aes(x = reorder(Method, Method,
                           function(x) { which(x == Method)})), stat = "identity") +
    geom_point(aes(y = Leaderboard.Error), color = pal[3]) +
    geom_point(aes(y = Test.Error), color = pal[2]) +
    geom_point(aes(y = Training.Error), color = pal[1]) +
    geom_line(aes(y = Leaderboard.Error, group = 1), color = pal[3]) +
    geom_line(aes(y = Test.Error, group = 2), color = pal[2]) +
    geom_line(aes(y = Training.Error, group = 3), color = pal[1]) +
    ylab("Error") + xlab("Algorithm") + coord_cartesian(ylim = c(.8, 1)) +
    ggtitle("Performance of Submissions in the Data Mining Competition") +    

ggsave("Reports/Final/vis.pdf", height = 5, width = 11)
