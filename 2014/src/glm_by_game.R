playoffs_df <- read.csv(paste(.project.path, "data/playoffs.csv", sep = ""),
                        header = F)
playoffs_df <- as.tbl(playoffs_df)
                        
index <- laply(team_stats, .fun = function(x) !is.null(x))
N <- sum(index)

results_df <- data.frame(Record = 1:N,
                         AvgDiff = 1:N,
                         MedDiff = 1:N,
                         Away = 1:N,
                         FinRecord = 1:N,
                         BackToBack = 1:N,
                         Playoffs = playoffs_df$V3[index],
                         Previous = playoffs_df$V4[index])

games_glm <- list()
games <- 1:82
for (i in 1:length(games)){
  for (j in 1:N) {
    results_df[j, 1:6] <- ProcessSeasonStatsByGame(team_stats_complete[[j]], 
                                                   game = games[i])
    # print(results_df[j, ])
  }
  games_glm[[i]] <- glm(Playoffs ~ Record + AvgDiff + Away + Previous + BackToBack,
                        family = binomial,
                        data = results_df)
}
saveRDS(games_glm, file = paste(.project.path, "data/game_glm.rds", sep = ""))

