# saveRDS(team_stats, file = "data/team_stats_2015.rds") 
# saveRDS(team_stats_complete, file = "data/team_stats_complete_2015.rds") 
# saveRDS(team_matrix, file = "data/team_matrix_2015.rds")
source("2015/src/functions.R")
library(dplyr)
library(plyr)

## API
.base.api <- "http://www.basketball-reference.com/teams/"

## Data
team_stats <- readRDS(file = "2015/data/team_stats_2015.rds")
team_stats_complete <- readRDS(file = "2015/data/team_stats_complete_2015.rds")
team_matrix <- readRDS(file = "2015/data/team_matrix_2015.rds")
games_glm <- readRDS(file = "2015/data/game_glm_2015.rds")

teams <- c("ATL", "BOS", "BRK", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
           "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
           "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

team_stats_2016 <- list()
teams[4] <- "CHO"
for(team in teams){
  tmp_stats <- GetNBASeasonTeamByYear(team, "2016")
  team_stats_2016[[team]] <- filter(tmp_stats, !is.na(Tm))
}

league <- data.frame(team = teams, prob = 1:30, se = 1:30, games = 1:30,
                     conf = c("E", "E", "E", "E", "E", "E", "W", "W", "E", "W", 
                              "W", "E", "W", "W", "W", "E", "E", "W", "W", "E", 
                              "W", "E", "E", "W", "W", "W", "W", "E", "W", "E"))
playoffs_df <- read.csv("2015/data/playoffs.csv", header = F)
playoffs_df <- as.tbl(playoffs_df)

for (team in teams){
  tmp <- PlayoffProbabilitiesByCurrentRecord(team, kGames = NULL,
                                             playoffs_df, 
                                             team_stats_2016[[team]])
  league[league$team == team, 2:4] <- tmp
}

league <- league[order(-league[, 2]), ]

team_playoffs <- PlayoffsFromPowerRankings(league)


kTotalGames <- sum(ldply(team_stats_2016, function(x) dim(x)[[1]])$V1)

results <- data.frame(team = 1:kTotalGames,
                      conf = rep(NA, kTotalGames),
                      game = 1:kTotalGames,
                      prob = 1:kTotalGames, 
                      se   = 1:kTotalGames)
counter = 1
for(team in teams){
  conf <- league$conf[league$team == team]
  kGames <- dim(team_stats_2016[[team]])[1]
  for(game in 1:kGames){
    tmp <- PlayoffProbabilitiesByCurrentRecord(team, 
                                               kGames = game, 
                                               playoffs_df,
                                               team_stats_2016[[team]])
    results[counter, ] <- c(team, conf, game, tmp$fit, tmp$se)
    counter = counter + 1
  }  
}

results$game <- as.numeric(results$game)
results$prob <- as.numeric(results$prob)
results$se <- as.numeric(results$se)
results <- as.tbl(results)
pwr_rank <- group_by(results, team) %>% 
            filter(game == max(game)) %>%
            ungroup() %>%
            arrange(desc(prob))

mc_rankings <- PlayoffsTheMonteCarloWay(pwr_rank, kSims = 1000)
