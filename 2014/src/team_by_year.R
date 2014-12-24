counter <- 1
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
    if (team == 'BRK' & year < 2013) team <- "NJN"
    if (team == "CHA" & year <= 2004) team_stats[[counter]] <- NULL
    else {
      print(c(team, year, counter))
      team_stats[[counter]] <- GetNBASeasonTeamByYear(team, year)
    } 
  team_matrix[counter, ] <- c(year, team)
  counter <- counter + 1   
  }
}

index <- laply(team_stats, .fun = function(x) !is.null(x))
team_stats_complete <- team_stats[index]

# saveRDS(team_stats, file = "../data/team_stats.rds")
# saveRDS(team_stats_complete, file = "../data/team_stats_complete.rds")
# saveRDS(team_matrix, file = "../data/team_matrix.rds")
team_stats <- readRDS(team_stats, file = "../data/team_stats.rds")
team_stats_complete <- readRDS(file = "../data/team_stats_complete.rds")
team_matrix <- readRDS(file = "../data/team_matrix.rds")
