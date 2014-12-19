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
                         BackToBack = c(NA, as.vector(diff(mdy(Date)))))
  stats$Diff <- stats$Tm - stats$Opp
  stats$AvgDiff <- cumsum(stats$Diff)/stats$G
  stats$Away <- cumsum(stats[, 3] == '@')
  stats$BackToBack <- c(NA, as.vector(diff(mdy(stats$Date))))
  return(stats)
}
