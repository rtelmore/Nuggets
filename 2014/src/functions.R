## Ryan Elmore
## Date: 18 Dec 2014
## Include project-specific functions in this file


GetNBASeasonTeamByYear <- function(team, year){
  html <- paste(.base.api, team, "/", year, "_games.html", sep="")
  stats <- readHTMLTable(html)[['teams_games']][c(1, 2, 4:6, 8:12)]
  stats <- stats[-c(21, 42, 63, 84), ]
  stats[, c(1, 6:9)] <- apply(stats[, c(1, 6:9)], 2, as.numeric)
  stats$Diff <- stats$Tm - stats$Opp
  stats$AvgDiff <- cumsum(stats$Diff)/stats$G
  stats$Away <- cumsum(stats[, 3] == '@')
  stats$BackToBack <- c(NA, as.vector(diff(mdy(stats$Date))))
  return(stats)
}
