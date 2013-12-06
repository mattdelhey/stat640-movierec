library(ggplot2)
err <- read.csv("Submissions/pp1.csv")

library(RColorBrewer)
pal <- brewer.pal(3, "BuPu")

ggplot(data = err, aes(x = Method), stat = "identity") +
    geom_bar(aes(y = Leaderboard.Error), fill = pal[3]) +
    geom_bar(aes(y = Test.Error), fill = pal[2]) +
    geom_bar(aes(y = Training.Error), fill = pal[1]) +
    ylab("Error") + xlab("Algorithm") + coord_cartesian(ylim = c(.8, 1)) +
    theme_bw()

ggsave("Reports/Progress1/vis.pdf", height = 3, width = 7)
