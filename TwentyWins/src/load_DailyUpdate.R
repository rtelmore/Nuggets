## Ryan Elmore
## Date:
## Prelims

## Set working directory
setwd(paste(.project.path, "src/", sep = ""))

## Load project-specific functions
source(paste(.project.path, "src/functions.R", sep=""))

## Load libraries
library(ggplot2)
library(XML)
library(plyr)
library(lubridate)
library(ggplot2)

## API
.base.api <- "http://www.basketball-reference.com/teams/"
.teams <- c("ATL", "BOS", "BRK", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
            "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
            "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

load(paste(.project.path, "data/team.RData", sep = ""))
index <- laply(team.stats, .fun = function(x) !is.null(x))
team.stats.sub <- team.stats[index]

for (i in 1:length(team.stats.sub)){
  team.stats.sub[[i]]$BackToBack <- c(0, as.vector(diff(mdy(team.stats.sub[[i]]$Date))))
}

playoffs.df <- read.csv(paste(.project.path, "data/playoffs.csv", sep = ""), 
                        header = F)
team.stats.2014 <- list()
for(team in .teams){
  tmp.stats <- GetNBASeasonTeamCurrentYear(team)
  team.stats.2014[[team]] <- tmp.stats[!is.na(tmp.stats$Tm), ]
}

kMin <- length(team.stats.2014[['MIN']][, ".1"])
team.stats.2014[['MIN']][kMin, "W"] <- sum(team.stats.2014[['MIN']][, ".1"] == "W")
team.stats.2014[['MIN']][kMin, "L"] <- sum(team.stats.2014[['MIN']][, ".1"] == "L")
team.stats.2014[['MIN']][kMin, "AvgDiff"] <- mean(team.stats.2014[['MIN']][, "Diff"])
kSAS <- length(team.stats.2014[['SAS']][, ".1"])
team.stats.2014[['SAS']][kSAS, "W"] <- sum(team.stats.2014[['SAS']][, ".1"] == "W")
team.stats.2014[['SAS']][kSAS, "L"] <- sum(team.stats.2014[['SAS']][, ".1"] == "L")
team.stats.2014[['SAS']][kSAS, "AvgDiff"] <- mean(team.stats.2014[['SAS']][, "Diff"])


load(file = paste(.project.path, "data/glms.RData", sep = ""))

league <- data.frame(team = .teams, prob = 1:30, se = 1:30, games = 1:30,
                     conf = c("E", "E", "E", "E", "E", "E", "W", "W", "E", "W", 
                              "W", "E", "W", "W", "W", "E", "E", "W", "W", "E", 
                              "W", "E", "E", "W", "W", "W", "W", "E", "W", "E"))
for (team in .teams){
  tmp <- PlayoffProbabilitiesByCurrentRecord(team)
  league[league$team == team, 2:4] <- tmp
}

league <- league[order(-league[, 2]), ]

rm(team.stats)

load(file = paste(.project.path, "data/season.to.date.RData", sep = ""))
# team.stats.2014[['SAS']][, "AvgDiff" ] <- mean(team.stats.2014[['SAS']][, "Diff" ])
