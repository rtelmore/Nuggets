## Ryan Elmore
## Date: 18 Dec 2014
## Include project-specific functions in this file


GetNBASeasonTeamByYear <- function(team, year){
  require(lubridate)
  require(dplyr)
  require(XML)
  url <- paste(.base.api, team, "/", year, "_games.html", sep="")
  stats <- readHTMLTable(url)[['teams_games']][c(1, 2, 6:8, 10:14)]
  stats <- stats[-c(21, 42, 63, 84), ]
  stats[, c(1, 6:9)] <- apply(stats[, c(1, 6:9)], 2, as.numeric)
  colnames(stats)[3] <- "Away_Indicator"
  stats <- tbl_df(stats)
  stats <- mutate(stats, Diff = Tm - Opp, 
                         AvgDiff = cumsum(Diff)/G,
                         Away = cumsum(Away_Indicator == '@'),
                         DaysBetweenGames = c(NA, as.vector(diff(mdy(Date)))))
  return(stats)
}

ProcessSeasonStatsByGame <- function(team, game){
  kGames <- dim(team)[1]
  kRecord <- collect(team[game, 'W'] - team[game, 'L'], W)[[1]]
  fRecord <- collect(team[kGames, 'W'] - team[kGames, 'L'], W)[[1]]
  btob <- ifelse(game > 1, sum(team[2:game, 'DaysBetweenGames'] == 1), 0)
  return(list(Record = kRecord,
              AvgDiff = collect(team[game, 'AvgDiff'])[[1]],
              MedDiff = median(collect(select(team[1:game, ], Diff))[[1]]),
              Away = collect(team[game, 'Away'])[[1]],
              FinRecord = fRecord,
              BackToBack = btob))
}


PlayoffProbabilitiesByCurrentRecord <- function(team, kGames = NULL, playoffs, stats){
  if(is.null(kGames)){
    kGames <- dim(stats)[1]    
  }
  stats_process <- ProcessSeasonStatsByGame(stats, kGames)
  stats_process$Playoffs <- NA
  if(team == "NOP") team <- "NOH"
  if(team == "CHO") team <- "CHA"
  stats_process$Previous <- as.numeric(filter(playoffs, V1 == '2014', V2 == team) %>%
                            select(V3))
  team_pred <- predict(games_glm[[kGames]], newdata = data.frame(stats_process),
                       type = "link", se = T)[1:2]
  team_pred$kGames <- kGames
  return(team_pred)
}

PlayoffsFromPowerRankings <- function(rankings){
  rankings <- rankings[order(rankings$team), ]
  rankings$division <- c("E", "A", "A", "E", "C", "C", "W", "N", "C", "P", "W", "C",
                        "P", "P", "W", "E", "C", "N", "W", "A", "N", "E", "A", "P",
                        "N", "P", "W", "A", "N", "E")
  league <- rankings[order(rankings$division, -rankings$prob), ]
  playoffs <- league$team[seq(1, 26, by = 5)]
  leftovers <- league[!(league$team %in% playoffs), ]
  leftovers <- leftovers[order(leftovers$conf, -leftovers$prob), ]
  playoffs <- c(playoffs, leftovers$team[c(1:5, 13:17)])    
  rankings$playoffs <- rep(0, 30)
  rankings$playoffs[rankings$team %in% playoffs] <- 1
  return(data.frame(team = rankings$team,
                    conf = rankings$conf, 
                    division = rankings$division,
                    playoffs = rankings$playoffs))
}

RandomTeamRanking <- function(team){
  return(runif(n = 1, min = team[1] - 5*team[2], max = team[1] + 5*team[2]))
}

PlayoffsTheMonteCarloWay <- function(rankings, kSims = 10000){
  sim.playoffs <- matrix(NA, nc = 30, nr = kSims)
  rankings <- rankings[order(rankings$team), ]
  rankings$divison <- c("E", "A", "A", "E", "C", "C", "W", "N", "C", "P", "W", "C",
                        "P", "P", "W", "E", "C", "N", "W", "A", "N", "E", "A", "P",
                        "N", "P", "W", "A", "N", "E")
  new.ranks <- rankings[, 4:5]
  for (sim in 1:kSims){
    new.p <- apply(new.ranks, 1, RandomTeamRanking)
    new.p[new.p > 1] <- 1
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
}
