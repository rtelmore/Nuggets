## Ryan Elmore
## Make figs

source("2015/src/daily-update.R")
source("2015/src/nuggets-to-date.R")
library(ggplot2)

## Nuggets trend
plot(c(1, kGames), c(0, 1), type = "n", xlab = "game number",
     ylab = "playoff indicator",
     main = "Denver Playoff Indicator by Game")
grid()
for(l in 1:kGames){
  lines(c(l, l), c(Nuggets[l, 4] + Nuggets[l, 5]*2, 
                   Nuggets[l, 4] - Nuggets[l, 5]*2),
        lty = 1, lwd = 2, col = "navy")
}
points(1:kGames, Nuggets[, 4][[1]], pch = 16, cex = 2, col = "grey")
points(1:kGames, Nuggets[, 4][[1]], pch = 16, cex = 1, col = "forestgreen")
abline(h = .5, lty = 3, lwd = 2)

## Overall Power Rankings
pdf(file = paste("2015/fig/NBA-PR-", Sys.Date(), ".pdf", sep = ""),
    height = 8.5,
    width = 11)

plot(c(1, 30), c(0, 1), type = "n", xlab = "team",
     ylab = "power ranking", xaxt = "n",
     main = paste("Ryan's NBA Power Rankings: ", Sys.time(), sep = ""))
abline(h = seq(0, 1, by = .2), lty = 3, col = "lightgray")
abline(v = 1:30, lty = 3, col = "lightgray")
abline(h = .5, lty = 2)
abline(v = (1:30)[pwr_rank$team == "DEN"], lty = 3, col = "darkred", lwd = 1.5)
for(l in 1:30){
  lines(c(l, l), c(pwr_rank[l, 4] + pwr_rank[l, 5]*2, 
                   pwr_rank[l, 4] - pwr_rank[l, 5]*2),
        lty = 1, lwd = 2, col = "navy")
}
points(1:30, pwr_rank[1:30, 4][[1]], pch = 16, cex = 2, col = "grey")
points((1:30)[pwr_rank$conf == 1], pwr_rank[pwr_rank$conf == 1, 4][[1]], 
       pch = 16, cex = 1, col = "forestgreen")
points((1:30)[pwr_rank$conf == 2], pwr_rank[pwr_rank$conf == 2, 4][[1]], 
       pch = 16, cex = 1, col = "darkorange")
axis(1, at = 1:30, labels = pwr_rank$team, cex.axis = .65, las = 2)
legend("topright", legend = c("West", "East"), pch = c(16, 16), 
       col = c("darkorange", "forestgreen"), bg = "white")
dev.off()



nGames <- max(results$game)
p <- ggplot(data = filter(results, conf == 1),
            aes(x = game, y = prob, col = team, group = team, 
                ymin = prob - 2*se, ymax = prob + 2*se))
p + geom_line() +
  facet_wrap(~ team, nrow = 5, ncol = 3) +
  scale_y_continuous("playoff indicator") +
  scale_x_discrete("Games") +
  theme(legend.position = "none") +
  geom_errorbar()

p <- ggplot(data = filter(results, conf == 2),
            aes(x = game, y = prob, col = team, group = team, 
                ymin = prob - 2*se, ymax = prob + 2*se))
p + geom_line() +
  facet_wrap(~ team, nrow = 5, ncol = 3) +
  scale_y_continuous("playoff indicator") +
  scale_x_discrete("Games") +
  theme(legend.position = "none") +
  geom_errorbar(width=1)


# scale_x_discrete(breaks = seq(0, nGames, by = 5), "Games") +
  