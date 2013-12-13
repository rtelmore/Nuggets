## Ryan Elmore
## Date:
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

ProcessSeasonStatsByGame <- function(team, game){
  kGames <- dim(team)[1]
  kRecord <- team[game, 'W'] - team[game, 'L']
  fRecord <- team[kGames, 'W'] - team[kGames, 'L']
  return(list(Record = kRecord,
              AvgDiff = team[game, 'AvgDiff'],
              MedDiff = median(team[1:game, 'Diff']),
              Away = team[game, 'Away'],
              FinRecord = fRecord,
              BackToBack = sum(team[2:game, 'BackToBack'] == 1)))
}

GetNBASeasonTeamCurrentYear <- function(team){
  require(lubridate)
  html <- paste(.base.api, team, "/", "2014", "_games.html", sep="")
  stats <- readHTMLTable(html)[['teams_games']][c(1, 2, 4:6, 8:12)]
  stats <- stats[-c(21, 42, 63, 84), ]
  stats <- stats[mdy(stats$Date) < as.POSIXct(Sys.Date()), ]
  stats[, c(1, 6:9)] <- apply(stats[, c(1, 6:9)], 2, as.numeric)
  stats$Diff <- stats$Tm - stats$Opp
  stats$AvgDiff <- cumsum(stats$Diff)/stats$G
  stats$Away <- cumsum(stats[, 3] == '@')
  stats$BackToBack <- c(0, as.vector(diff(mdy(stats$Date))))
  return(stats)
}

PlayoffProbabilitiesByCurrentRecord <- function(team, kGames = NULL){
  stats <- team.stats.2014[[team]]
  if(is.null(kGames)){
    kGames <- dim(stats)[1]    
  }
  stats.process <- ProcessSeasonStatsByGame(stats, kGames)
  stats.process$Playoffs <- NA
  if(team == "NOP") team <- "NOH"
  stats.process$Previous <- playoffs.df[playoffs.df$V1 == '2013' & playoffs.df$V2 == team, 'V3']
  # print(stats.process)
  team.pred <- predict(games.glm[[kGames]], newdata = data.frame(stats.process),
                       type = "response", se = T)[1:2]
  team.pred$kGames <- kGames
  return(team.pred)
}

RandomTeamRanking <- function(team){
  return(runif(n = 1, min = team[1] - 5*team[2], max = team[1] + 5*team[2]))
  # , sd = 2*team[2]))
}

PlayoffsTheMonteCarloWay <- function(rankings, kSims = 10000){
  sim.playoffs <- matrix(NA, nc = 30, nr = kSims)
  rankings <- rankings[order(rankings$team), ]
  rankings$divison <- c("E", "A", "A", "E", "C", "C", "W", "N", "C", "P", "W", "C",
                        "P", "P", "W", "E", "C", "N", "W", "A", "N", "E", "A", "P",
                        "N", "P", "W", "A", "N", "E")
  new.ranks <- rankings[, 2:3]
  for (sim in 1:kSims){
    new.p <- apply(new.ranks, 1, RandomTeamRanking)
    new.p[new.p > 1] <- 1
    new.p[]
    rankings$prob <- new.p
    league <- rankings[order(rankings$div, -rankings$prob), ]
    playoffs <- league$team[seq(1, 26, by = 5)]
    leftovers <- league[!(league$team %in% playoffs), ]
    leftovers <- leftovers[ order(leftovers$conf, -leftovers$prob), ]
    playoffs <- c(playoffs, leftovers$team[c(1:5, 13:17)])    
    sim.playoffs[sim, ] <- rankings$team %in% playoffs
  }
  colnames(sim.playoffs) <- rankings$team
  tmp <- apply(sim.playoffs, 2, mean)
  tmp2 <- apply(sim.playoffs, 2, sd)
  return(data.frame(p = tmp, v = tmp2, conf = rankings$conf, div = rankings$div))
  # league$team %in% playoffs  
}
