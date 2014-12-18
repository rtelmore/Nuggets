## Set working directory
setwd(paste(.project.path, "src/", sep = ""))

## Load project-specific functions
source(paste(.project.path, "src/functions.R", sep=""))

## Load libraries
library(ggplot2)
library(XML)
library(plyr)
library(lubridate)

## API
.base.api <- "http://www.basketball-reference.com/teams/"
.teams <- c("ATL", "BOS", "BRK", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
            "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
            "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

# Load data  
# data.str <- paste(.project.path, "data/some_data_file.dat", sep = "")
# OKC was SEA in 2008 and prior
# NOH was NOK in 2006 and 2007 and CHH < 2002
years <- 2002:2013
teams <- c("ATL", "BOS", "NJN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
           "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOH", "NYK",
           "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

index <- 1
team.stats <- list()
team.matrix <- matrix(NA, nc = 2, nr = length(years)*length(teams))

for (year in years) {
  for (team in teams) {
    if (team == "NOH") {
      if (year <= 2002) team <- "CHH"
      if (year %in% c(2006, 2007)) team <- "NOK"
    }
    if (team == "OKC") {
      if (year <= 2008) team <- "SEA"
    }
    if (team == 'NJN' & year >= 2013) team <- "BRK"
    if (team == "CHA" & year <= 2004) team.stats[[index]] <- NULL
    else {
      print(c(team, year, index))
      team.stats[[index]] <- GetNBASeasonTeamByYear(team, year)
    } 
  team.matrix[index, ] <- c(year, team)
  index <- index + 1   
  }
}

# save("team.stats", "team.matrix", 
#      file = paste(.project.path, "data/team.RData", sep = ""))
