## validate after 20 games
## fit model to subset of the data (train = 250, test = 107)
set.seed(19838)
kTrials <- 1000
games <- c(5, 10, 20, 30, 40, 50, 60)
kGames <- length(games)
results_full <- matrix(0, nc = kGames, nr = kTrials)
results_red <- matrix(0, nc = kGames, nr = kTrials)

for (game in 1:7){
  print(games[game])
  for (j in 1:N) {
    results_df[j, 1:6] <- ProcessSeasonStatsByGame(team_stats_complete[[j]], 
                                                   game = games[game])
  }

  # glm_full <- glm(Playoffs ~ Record + AvgDiff + Away + Previous + BackToBack,
  #                family = binomial,
  #                data = results_df)
  # 
  # glm_red <- glm(Playoffs ~ Record + AvgDiff + Previous,
  #                family = binomial,
  #                data = results_df)

  for (i in 1:kTrials){
    new_index <- rep(F, N)
    new_index[sample(1:N, size = 300, rep = F)] <- TRUE

    results_df_train <- results_df[new_index, ]
    results_df_test <- results_df[!new_index, ]
    glm_full <- glm(Playoffs ~ Record + AvgDiff + Away + Previous + BackToBack,
                   family = binomial,
                   data = results_df_train)

    results_full_pred <- predict(glm_full, 
                                newdata = results_df_test,
                                type = "response")
    results_full[i, game] <- mean((round(results_full_pred) == results_df_test[, "Playoffs"]), 
                              na.rm = T)
    glm_red <- glm(Playoffs ~ Record + AvgDiff + Previous,
                   family = binomial,
                   data = results_df_train)

    results_red_pred <- predict(glm_red, 
                                newdata = results_df_test,
                                type = "response")
    results_red[i, game] <- mean((round(results_red_pred) == results_df_test[, "Playoffs"]), 
                                 na.rm = T)
  }  
} 
apply(results_red, 2, summary)
apply(results_full, 2, summary)
saveRDS("results_red", file = paste(.project.path, "data/results_red.rds", sep = ""))
saveRDS("results_full", file = paste(.project.path, "data/results_full.rds", sep = ""))

