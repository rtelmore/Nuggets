team_stats_2015 <- list()
teams[4] <- "CHO"
for(team in teams){
  tmp_stats <- GetNBASeasonTeamByYear(team, "2015")
  team_stats_2015[[team]] <- filter(tmp_stats, !is.na(Tm))
}

league <- data.frame(team = teams, prob = 1:30, se = 1:30, games = 1:30,
                     conf = c("E", "E", "E", "E", "E", "E", "W", "W", "E", "W", 
                              "W", "E", "W", "W", "W", "E", "E", "W", "W", "E", 
                              "W", "E", "E", "W", "W", "W", "W", "E", "W", "E"))
for (team in teams){
  tmp <- PlayoffProbabilitiesByCurrentRecord(team, kGames = NULL,
                                             playoffs_df, 
                                             team_stats_2015[[team]])
  league[league$team == team, 2:4] <- tmp
}

league <- league[order(-league[, 2]), ]

team_playoffs <- PlayoffsFromPowerRankings(league)

# playoffs_df <- read.csv(paste(.project.path, "data/playoffs.csv", sep = ""),
#                         header = F)
playoffs_df <- as.tbl(playoffs_df)

kTotalGames <- sum(ldply(team_stats_2015, function(x) dim(x)[[1]])$V1)

results <- data.frame(team = 1:kTotalGames,
                      conf = 1:kTotalGames,
                      game = 1:kTotalGames,
                      prob = 1:kTotalGames, 
                      se   = 1:kTotalGames)
counter = 1
for(team in teams){
  conf <- league$conf[league$team == team]
  kGames <- dim(team_stats_2015[[team]])[1]
  for(game in 1:kGames){
    tmp <- PlayoffProbabilitiesByCurrentRecord(team, 
                                               kGames = game, 
                                               playoffs_df,
                                               team_stats_2015[[team]])
    results[counter, ] <- c(team, conf, game, tmp$fit, tmp$se)
    counter = counter + 1
  }  
}

results$game <- as.numeric(results$game)
results$prob <- as.numeric(results$prob)
results$se <- as.numeric(results$se)
results <- as.tbl(results)
