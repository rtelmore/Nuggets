index <- 1
team_stats <- vector("list", 390)
team_matrix <- matrix(NA, nc = 2, nr = length(years)*length(teams))

for (year in years) {
  for (team in teams) {
    if (team == "NOP") {
      if (year <= 2013) {
        team <- "NOH"
        if (year <= 2002) team <- "CHH"
        if (year %in% c(2006, 2007)) team <- "NOK"
      }
    }
    if (team == "OKC") {
      if (year <= 2008) team <- "SEA"
    }
    if (team == 'NJN' & year >= 2013) team <- "BRK"
    if (team == "CHA" & year <= 2004) team_stats[[index]] <- NULL
    else {
      print(c(team, year, index))
      team_stats[[index]] <- GetNBASeasonTeamByYear(team, year)
    } 
  team_matrix[index, ] <- c(year, team)
  index <- index + 1   
  }
}

index <- laply(team_stats, .fun = function(x) !is.null(x))
team_stats_complete <- team_stats[index]

saveRDS(team_stats, file = "../data/team_stats.rds")
saveRDS(team_stats, file = "../data/team_stats_complete.rds")
saveRDS(team_matrix, file = "../data/team_matrix.rds")
