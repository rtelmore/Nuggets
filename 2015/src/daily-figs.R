## Ryan Elmore
## Make figs

source("2015/src/daily-update.R")
source("2015/src/nuggets-to-date.R")
library(ggplot2)
library(boot)

## Nuggets trend
pdf(file = paste("2015/fig/Nugs-PR-", Sys.Date(), ".pdf", sep = ""),
    height = 8.5,
    width = 11)
plot(c(1, kGames), c(0, 1), type = "n", xlab = "game number",
     ylab = "playoff indicator",
     main = "Denver Playoff Indicator by Game")
grid()
for(l in 1:kGames){
  lines(c(l, l), c(inv.logit(Nuggets[l, 4] + Nuggets[l, 5]*2),
                   inv.logit(Nuggets[l, 4] - Nuggets[l, 5]*2)),
        lty = 1, lwd = 2, col = "navy")
}
points(1:kGames, inv.logit(Nuggets[, 4]), pch = 16, cex = 2, 
       col = "grey")
points(1:kGames, inv.logit(Nuggets[, 4]), pch = 16, cex = 1, 
       col = "forestgreen")
abline(h = .5, lty = 3, lwd = 2)
dev.off()

## Overall Power Rankings
pdf(file = paste("2015/fig/NBA-PR-", Sys.Date(), "-b.pdf", sep = ""),
    height = 8.5,
    width = 11)

plot(c(1, 30), c(0, 1), type = "n", xlab = "team",
     ylab = "power ranking", xaxt = "n",
     main = paste("Ryan's NBA Power Rankings: ", Sys.Date(), sep = ""))
abline(h = seq(0, 1, by = .2), lty = 3, col = "lightgray")
abline(v = 1:30, lty = 3, col = "lightgray")
abline(h = .5, lty = 2)
abline(v = (1:30)[pwr_rank$team == "DEN"], lty = 3, col = "darkred", lwd = 1.5)
for(l in 1:30){
  lines(c(l, l), c(inv.logit(c(pwr_rank[l, "fit"] + pwr_rank[l, "se"]*2)$fit), 
                   inv.logit(c(pwr_rank[l, "fit"] - pwr_rank[l, "se"]*2)$fit)),
        lty = 1, lwd = 2, col = "navy")
}
points(1:30, inv.logit(pwr_rank %>% .$fit), pch = 16, cex = 2, col = "grey")
points((1:30)[pwr_rank$conf == 1], 
       inv.logit(pwr_rank %>% .$fit)[pwr_rank$conf == 1], 
       pch = 16, cex = 1, col = "forestgreen")
points((1:30)[pwr_rank$conf == 2], 
       inv.logit(pwr_rank %>% .$fit)[pwr_rank$conf == 2], 
       pch = 16, cex = 1, col = "darkorange")
axis(1, at = 1:30, labels = pwr_rank$team, cex.axis = .65, las = 2)
legend("topright", legend = c("West", "East"), pch = c(16, 16), 
       col = c("darkorange", "forestgreen"), bg = "white")
dev.off()



nGames <- max(results$game)
p <- ggplot(data = filter(results, conf == 1),
            aes(x = game, y = inv.logit(fit), col = team, group = team, 
                ymin = inv.logit(fit - 2*se), ymax = inv.logit(fit + 2*se)))
p <- ggplot(data = filter(results, conf == 1),
            aes(x = game, y = inv.logit(fit), group = team, 
                ymin = inv.logit(fit - 2*se), ymax = inv.logit(fit + 2*se)))
p + geom_line() +
  geom_errorbar(width = .25) +
  facet_wrap(~ team, nrow = 5, ncol = 3) +
  scale_y_continuous("power ranking") +
  scale_x_continuous(breaks = seq(0, nGames, 2), "games") +
  theme(legend.position = "none") +
  theme_bw()
ggsave(file = paste("2015/fig/Eest-PR-", Sys.Date(), ".pdf", sep = ""),
       height = 11,
       width = 8.5)

p <- ggplot(data = filter(results, conf == 2),
            aes(x = game, y = inv.logit(fit), group = team, 
                ymin = inv.logit(fit - 2*se), 
                ymax = inv.logit(fit + 2*se)))
p + geom_line() +
  geom_errorbar(width = .25) +
  facet_wrap(~ team, nrow = 5, ncol = 3) +
  scale_y_continuous("power ranking") +
  scale_x_continuous(breaks = seq(0, nGames, 2), "games") +
  theme(legend.position = "none") +
  theme_bw()
ggsave(file = paste("2015/fig/West-PR-", Sys.Date(), ".pdf", sep = ""),
       height = 11,
       width = 8.5)

  