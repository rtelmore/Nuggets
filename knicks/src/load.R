## Ryan Elmore
## Date:
## Prelims

## Set working directory
setwd(paste(.project_path, "src/", sep = ""))

## Load libraries
library(dplyr)
library(plyr)
library(ggplot2)

## Load data
team_stats <- readRDS("../../2014/data/team_stats.rds")
playoffs_df <- readRDS("../../2014/data/playoffs_df.rds")
