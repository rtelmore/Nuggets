results.df <- data.frame(Record = 1:357,
                         AvgDiff = 1:357,
                         MedDiff = 1:357,
                         Away = 1:357,
                         FinRecord = 1:357,
                         BackToBack = 1:357,
                         Playoffs = playoffs.df$V3[index],
                         Previous = playoffs.df$V4[index])

games.glm <- list()
games <- 1:82
for (i in 1:length(games)){
  for (j in 1:357) {
    results.df[j, 1:6] <- ProcessSeasonStatsByGame(team.stats.sub[[j]], 
                                                   game = games[i])
  }
  games.glm[[i]] <- glm(Playoffs ~ Record + AvgDiff + Away + Previous + BackToBack,
                        family = binomial,
                        data = results.df)
}
save("games.glm", file = paste(.project.path, "data/glms.RData", sep = ""))


games.glm <- list()
games <- 1:82
for (i in 1:length(games)){

  for (j in 1:357) {
    results.df[j, 1:6] <- ProcessSeasonStatsByGame(team.stats.sub[[j]], 
                                                   game = games[i])
  }
  games.glm[[i]] <- glm(Playoffs ~ Record + AvgDiff + Away + Previous + BackToBack,
                        family = binomial,
                        data = results.df)
}

save("games.glm", file = paste(.project.path, "data/glms.RData", sep = ""))
