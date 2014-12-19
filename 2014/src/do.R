## Ryan Elmore
## Date:
## Description:

.project.path <- "/Users/relmore/Side_Projects/Nuggets/2014/"
setwd(paste(.project.path, "src/", sep = ""))

## Load project-specific functions
source(paste(.project.path, "src/functions.R", sep=""))

## Load libraries
library(ggplot2)
library(XML)
library(plyr)
library(lubridate)
library(reshape2)

## API
.base.api <- "http://www.basketball-reference.com/teams/"

# OKC was SEA in 2008 and prior
# NOP was NOH prior to 2014; NOH was NOK in 2006 and 2007 and CHH < 2002
years <- 2002:2014
teams <- c("ATL", "BOS", "NJN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
           "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
           "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

## 1. Get Team info by Year
source("team_by_year.R")

## 2. Build models
source("glm_by_game.R")

## 3. Look at coefficients
glm_coefs <- ldply(games_glm, function(x) coef(x)[-1])

glm_coefs$index <- 1:82

glm_coefs_melt <- melt(glm_coefs, id = "index")

p <- ggplot(data = glm_coefs_melt, aes(x = index, y = exp(value), col = variable))
p + geom_line(size = 1) +
  scale_x_continuous("number of games") +
  scale_y_continuous("change in odds ratio") +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))

## and p-values
glm_p_vals <- data.frame(Record = 1:80,
                         AvgDiff = 1:80,
                         Away = 1:80,
                         Previous = 1:80,
                         BackToBack = 1:80)
for (i in 1:80){
  glm_p_vals[i, ] <- coef(summary(games_glm[[i+1]]))[-1, 4]  
}
glm_p_vals <- as.tbl(glm_p_vals)

glm_p_vals$index <- 2:81

glm_p_melt <- melt(glm_p_vals, id = "index")

p <- ggplot(data = glm_p_melt, aes(x = index, y = value, col = variable))
p + geom_line(size = 1) +
  scale_x_continuous("number of games") +
  scale_y_continuous("p-value") +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))
