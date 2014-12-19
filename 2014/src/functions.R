## Ryan Elmore
## Date: 18 Dec 2014
## Include project-specific functions in this file


GetNBASeasonTeamByYear <- function(team, year){
  require(lubridate)
  require(dplyr)
  url <- paste(.base.api, team, "/", year, "_games.html", sep="")
  stats <- readHTMLTable(url)[['teams_games']][c(1, 2, 6:8, 10:14)]
  stats <- as.tbl(stats[-c(21, 42, 63, 84), ])
  stats[, c(1, 6:9)] <- apply(stats[, c(1, 6:9)], 2, as.numeric)
  colnames(stats)[3] <- "Away_Indicator"
  stats <- mutate(stats, Diff = Tm - Opp, 
                         AvgDiff = cumsum(Diff)/G,
                         Away = cumsum(Away_Indicator == '@'),
                         DaysBetweenGames = c(NA, as.vector(diff(mdy(Date)))))
  return(stats)
}

ProcessSeasonStatsByGame <- function(team, game){
  kGames <- dim(team)[1]
  kRecord <- team[game, 'W'] - team[game, 'L']
  fRecord <- team[kGames, 'W'] - team[kGames, 'L']
  btob <- ifelse(game > 1, sum(team[2:game, 'DaysBetweenGames'] == 1), 0)
  return(list(Record = kRecord,
              AvgDiff = team[game, 'AvgDiff'],
              MedDiff = median(team[1:game, 'Diff']),
              Away = team[game, 'Away'],
              FinRecord = fRecord,
              BackToBack = btob))
}
