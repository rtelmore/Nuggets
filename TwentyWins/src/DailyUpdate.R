args <- commandArgs(trailingOnly = TRUE)

.project.path <- "~/Side_Projects/Nuggets/TwentyWins/"

## Dependencies:
source(paste(.project.path, "src/load_DailyUpdate.R", sep=""))

## Nuggets to date
kGames <- dim(team.stats.2014[['DEN']])[1]

nugs.predictions <- matrix(0, nc = 2, nr = kGames)
for(game in 1:kGames){
  tmp <- PlayoffProbabilitiesByCurrentRecord("DEN", kGames = game)
  nugs.predictions[game, ] <- c(tmp$fit, tmp$se)
}

if(args[1] == TRUE){
  pdf(paste(.project.path, "fig/nuggets_probs_", as.Date(Sys.Date()), ".pdf", sep = ""), h = 7, w = 7)
  plot(c(1, kGames), c(0, 1), type = "n", xlab = "game number",
      ylab = "probability of making playoffs",
      main = paste("Denver Playoff Probability by Game: ", Sys.time(), sep = ""))
  grid()
  for(l in 1:kGames){
   lines(c(l, l), c(nugs.predictions[l, 1] + nugs.predictions[l, 2]*2, 
                    nugs.predictions[l, 1] - nugs.predictions[l, 2]*2),
         lty = 1, lwd = 2, col = "navy")
  }
  points(1:kGames, nugs.predictions[, 1], pch = 16, cex = 2, col = "grey")
  points(1:kGames, nugs.predictions[, 1], pch = 16, cex = 1, col = "forestgreen")
  abline(h = .5, lty = 3, lwd = 2)
  dev.off()
}

pdf(paste(.project.path, 
          "fig/league_probs_", 
          as.Date(Sys.Date()), 
          ".pdf", 
          sep = ""), 
    h = 7, w = 7)
plot(c(1, 30), c(0, 1), type = "n", xlab = "team",
    ylab = "probability of making playoffs", xaxt = "n",
    main = paste("Playoff Probability by Team: ", Sys.time(), sep = ""))
abline(h = .5, lty = 2)
abline(v = 1:30, lty = 3, col = "lightgray")
abline(v = (1:30)[league$team == "DEN"], lty = 3, col = "darkred", lwd = 1.5)
for(l in 1:30){
 lines(c(l, l), c(league[l, 2] + league[l, 3]*2, 
                  league[l, 2] - league[l, 3]*2),
       lty = 1, lwd = 2, col = "navy")
}
points(1:30, league[1:30, 2], pch = 16, cex = 2, col = "grey")
points((1:30)[league$conf == "E"], league[league$conf == "E", 2], 
       pch = 16, cex = 1, col = "forestgreen")
points((1:30)[league$conf == "W"], league[league$conf == "W", 2], 
       pch = 16, cex = 1, col = "darkorange")
axis(1, at = 1:30, labels = league$team, cex.axis = .65, las = 2)
legend("topright", legend = c("West", "East"), pch = c(16, 16), 
       col = c("darkorange", "forestgreen"), bg = "white")
dev.off()

league$date <- rep(as.Date(Sys.Date()), 30)
season.to.date <- rbind(season.to.date, league)
save("season.to.date", file = paste(.project.path, "data/season.to.date.RData", sep = ""))

## West
p <- ggplot(data = season.to.date[season.to.date$conf == "W", ], 
            aes(x = date, y = prob, col = team))
p + geom_line() +
  facet_wrap(~ team, nrow = 5, ncol = 3) +
  scale_y_continuous("Probability of Making Playoffs") +
  scale_x_date("Date") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file = paste(.project.path, "fig/league_W_facet_", as.Date(Sys.Date()), ".pdf", sep = ""), h = 7, w = 7)

# p + geom_line() +
#   scale_y_continuous("Probability of Making Playoffs") +
#   scale_x_date("Date")
# ggsave(file = paste(.project.path, "fig/league_W_", as.Date(Sys.Date()), ".pdf", sep = ""), h = 7, w = 7)


## East
p <- ggplot(data = season.to.date[season.to.date$conf == "E", ], 
            aes(x = date, y = prob, col = team))
p + geom_line() +
  facet_wrap(~ team, nrow = 5, ncol = 3) +
  scale_y_continuous("Probability of Making Playoffs") +
  scale_x_date("Date") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file = paste(.project.path, "fig/league_E_facet_", as.Date(Sys.Date()), ".pdf", sep = ""), h = 7, w = 7)

playoff.sim <- PlayoffsTheMonteCarloWay(league, k = 10000)
playoff.sim <- playoff.sim[order(-playoff.sim$p), ]

playoff.season.to.date <- rbind(playoff.season.to.date, playoff.sim)
save("playoff.season.to.date", 
     file = paste(.project.path, "data/playoff.season.to.date.RData", sep = ""))

pdf(paste(.project.path, 
          "fig/league_probs_sim_", 
          as.Date(Sys.Date()), 
          ".pdf", 
          sep = ""), 
    h = 7, w = 7)
plot(c(1, 30), c(0, 1), type = "n", xlab = "team",
    ylab = "probability of making playoffs", xaxt = "n",
    main = paste("Simulated Playoff Probability by Team: ", Sys.time(), sep = ""))
for(l in 1:30){
 lines(c(l, l), c(playoff.sim$p[l] + sqrt(playoff.sim$p[l]*(1-playoff.sim$p[l])/10000)*2, 
                  playoff.sim$p[l] - sqrt(playoff.sim$p[l]*(1-playoff.sim$p[l])/10000)*2),
       lty = 1, lwd = 2, col = "navy")
}
abline(h = .5, lty = 2)
abline(v = 1:30, lty = 3, col = "lightgray")
abline(v = (1:30)[rownames(playoff.sim) == "DEN"], lty = 3, col = "darkred", lwd = 1.5)
points(1:30, playoff.sim$p, pch = 16, cex = 2, col = "grey")
points((1:30)[playoff.sim$conf == "E"], playoff.sim[playoff.sim$conf == "E", "p"], 
       pch = 16, cex = 1, col = "forestgreen")
points((1:30)[playoff.sim$conf == "W"], playoff.sim[playoff.sim$conf == "W", "p"], 
       pch = 16, cex = 1, col = "darkorange")
axis(1, at = 1:30, labels = rownames(playoff.sim), cex.axis = .65, las = 2)
legend("topright", legend = c("West", "East"), pch = c(16, 16), 
       col = c("darkorange", "forestgreen"), bg = "white")
dev.off()

# p + geom_line() +
#   scale_y_continuous("Probability of Making Playoffs") +
#   scale_x_date("Date")
# ggsave(file = paste(.project.path, "fig/league_E_", as.Date(Sys.Date()), ".pdf", sep = ""), h = 7, w = 7)
