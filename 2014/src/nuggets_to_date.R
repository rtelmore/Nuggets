## Nuggets to date
kGames <- dim(team_stats_2015[['DEN']])[1]

Nuggets <- matrix(0, nc = 2, nr = kGames) 
for(game in 1:kGames){
  tmp <- PlayoffProbabilitiesByCurrentRecord("DEN", 
                                             kGames = game, 
                                             playoffs_df,
                                             team_stats_2015[["DEN"]])
  Nuggets[game, ] <- c(tmp$fit, tmp$se)
}

## Plot
# pdf(paste(.project.path, "fig/nuggets_probs_", as.Date(Sys.Date()), ".pdf", sep = ""),
#      , h = 7, w = 7)
# 
# plot(c(1, kGames), c(0, 1), type = "n", xlab = "Game Number",
#     ylab = "Power Ranking",
#     main = paste("Denver Power Ranking by Game: ", Sys.time(), sep = ""))
# grid()
# for(l in 1:kGames){
#  lines(c(l, l), c(Nuggets[l, 1] + Nuggets[l, 2]*2, 
#                   Nuggets[l, 1] - Nuggets[l, 2]*2),
#        lty = 1, lwd = 2, col = "navy")
# }
# points(1:kGames, Nuggets[, 1], pch = 16, cex = 2, col = "grey")
# points(1:kGames, Nuggets[, 1], pch = 16, cex = 1, col = "forestgreen")
# abline(h = .5, lty = 3, lwd = 2)
# dev.off()