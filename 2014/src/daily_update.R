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
