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
                       type = "response", se = T)[1:2]
  team_pred$kGames <- kGames
  return(team_pred)
}

PlayoffsFromPowerRankings <- function(rankings){
  sim.playoffs <- matrix(NA, nc = 30, nr = kSims)
  rankings <- rankings[order(rankings$team), ]
  rankings$divison <- c("E", "A", "A", "E", "C", "C", "W", "N", "C", "P", "W", "C",
                        "P", "P", "W", "E", "C", "N", "W", "A", "N", "E", "A", "P",
                        "N", "P", "W", "A", "N", "E")
  new.ranks <- rankings[, 2:3]
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
  # league$team %in% playoffs  
}

# RandomTeamRanking <- function(team){
#   return(runif(n = 1, min = team[1] - 5*team[2], max = team[1] + 5*team[2]))
# }
# 
# PlayoffsTheMonteCarloWay <- function(rankings, kSims = 10000){
#   sim.playoffs <- matrix(NA, nc = 30, nr = kSims)
#   rankings <- rankings[order(rankings$team), ]
#   rankings$divison <- c("E", "A", "A", "E", "C", "C", "W", "N", "C", "P", "W", "C",
#                         "P", "P", "W", "E", "C", "N", "W", "A", "N", "E", "A", "P",
#                         "N", "P", "W", "A", "N", "E")
#   new.ranks <- rankings[, 2:3]
#   for (sim in 1:kSims){
#     new.p <- apply(new.ranks, 1, RandomTeamRanking)
#     new.p[new.p > 1] <- 1
#     rankings$prob <- new.p
#     league <- rankings[order(rankings$div, -rankings$prob), ]
#     playoffs <- league$team[seq(1, 26, by = 5)]
#     leftovers <- league[!(league$team %in% playoffs), ]
#     leftovers <- leftovers[ order(leftovers$conf, -leftovers$prob), ]
#     playoffs <- c(playoffs, leftovers$team[c(1:5, 13:17)])    
#     sim.playoffs[sim, ] <- rankings$team %in% playoffs
#   }
#   colnames(sim.playoffs) <- rankings$team
#   tmp <- apply(sim.playoffs, 2, mean)
#   tmp2 <- apply(sim.playoffs, 2, sd)
#   return(data.frame(p = tmp, v = tmp2, conf = rankings$conf, div = rankings$div))
#   # league$team %in% playoffs  
# }
