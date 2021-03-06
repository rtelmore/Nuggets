## Ryan Elmore
## 5 Aug 2015
## playoffs-by-date.R

library(ggplot2)
library(wesanderson)
library(dplyr)
library(lubridate)
library(tidyr)

results_full <- readRDS(file = "2014/data/results_full.rds")
results_red <- readRDS(file = "2014/data/results_red.rds")

NBAStandingsByDate <- function(date_string){
  ## date is a string of the form "YYYY-MM-DD"
  require(rvest)
  require(lubridate)
  date_string <- ymd(date_string)
  .base.url <- "http://www.basketball-reference.com/friv/standings.cgi?"
  y <- year(date_string)
  m <- month(date_string)
  d <- day(date_string)
  url <- paste("http://www.basketball-reference.com/friv/standings.cgi?",
               "month=", m,
               "&day=", d,
               "&year=", y,
               "&lg_id=NBA", sep = "")
  r <- html(url)
  east <- html_table(r, fill = T)[[2]]
  west <- html_table(r, fill = T)[[3]]
  return(list(East = east, West = west))
}

NBAPlayoffsFromStandings <- function(standings){
  require(dplyr)
  require(stringr)
  standings <- tbl_df(standings[-c(1, 7, 13), ]) %>%
    mutate(Div = rep(1:3, each = 5))
  names(standings)[c(1, 4)] <- c("Team", "Win_Pct")  
  playoffs <- standings %>% 
    group_by(Div) %>% 
    filter(Win_Pct == max(Win_Pct)) %>% 
    ungroup() %>%
    select(Team)
  others <- standings %>% 
    filter(!(Team %in% playoffs$Team)) %>%
    arrange(desc(Win_Pct)) %>% top_n(n = 5, Win_Pct) %>%
    select(Team)
  return(rbind(playoffs, others) %>%
           mutate(Playoffs = str_detect(Team, "\\*")))
}

NumberInPlayoffs <- function(playoffs_df){
  require(stringr)
  total <- playoffs_df %>%
    mutate(tmp = str_detect(Team, "\\*")) %>%
    summarize(total = sum(tmp))
  return(total$total[1])
}

ten <- c("2004-11-21", "2005-11-18", "2006-11-22", "2007-11-17",
         "2008-11-16", "2009-11-13", "2010-11-15", "2012-11-17",
         "2013-11-18", "2014-11-17")
twenty <- c("2004-12-12", "2005-12-09", "2006-12-13", "2007-12-06",
            "2008-12-04", "2009-12-05", "2010-12-07", "2012-12-07",
            "2013-12-07", "2014-12-07")
thirty <- c("2005-01-02", "2005-12-29", "2007-01-05", "2007-12-30",
            "2008-12-26", "2009-12-25", "2010-12-28", "2012-12-26",
            "2013-12-30", "2014-12-26")
forty <- c("2005-01-23", "2006-01-18", "2007-01-21", "2008-01-15",
           "2009-01-15", "2010-01-17", "2011-01-16", "2013-01-15",
           "2014-01-19", "2015-01-17")
## Nuggets at 10, 20, 30, 40, 50 games for 5 season (back to 2005)
## 2015: Nov 17, Dec 7, Dec 26, Jan 17, Feb 4, Mar 3, Mar 20
## 2014: Nov 18, Dec 7, Dec 30, Jan 19, Feb 10, Mar 5, Mar 23
## 2013: Nov 17, Dec 7, Dec 26, Jan 15, Feb 7, Mar 1, Mar 21
## 2011: Nov 15, Dec 7, Dec 28, Jan 16, Feb 4, Feb 25, Mar 19
## 2010: Nov 13, Dec 5, Dec 25, Jan 17, Feb 5, Mar 1, Mar 20
## 2009: Nov 16, Dec 4, Dec 26, Jan 15, Feb 6, Mar 1, Mar 20
## 2008: Nov 17, Dec 6, Dec 30, Jan 21, Feb 10, Mar 5, Mar 23
## 2007: Nov 22, Dec 13, Jan 5, Jan 26, Feb 12, Mar 9, Mar 28
## 2006: Nov 18, Dec 9, Dec 29, Jan 18, Feb 6, Mar 4, Mar 24
## 2005: Nov 21, Dec 12, Jan 2, Jan 23, Feb 11, Mar 7, Mar 30

fifty <- c("2005-02-11", "2006-02-06", "2007-02-12", "2008-02-10",
           "2009-02-06", "2010-02-05", "2011-02-04", "2013-02-07",
           "2014-02-10", "2015-02-04")
sixty <- c("2005-03-07", "2006-03-04", "2007-03-09", "2008-03-05",
           "2009-03-01", "2010-03-01", "2011-02-25", "2013-03-01",
           "2014-03-05", "2015-03-03")

result <- matrix(NA, nc = 2, nr = 10)
results <- 1:6

for(i in 1:10){
  standings <- NBAStandingsByDate(ten[i])
  east <- NBAPlayoffsFromStandings(standings$East)
  west <- NBAPlayoffsFromStandings(standings$West)
  result[i, ] <- c(dim(east)[[1]] + dim(west)[[1]], 
                   sum(east$Playoffs) + sum(west$Playoffs))
}

# results <- numeric()
results[1] <- sum(result[, 2])/sum(result[, 1])

for(i in 1:10){
  standings <- NBAStandingsByDate(twenty[i])
  east <- NBAPlayoffsFromStandings(standings$East)
  west <- NBAPlayoffsFromStandings(standings$West)
  result[i, ] <- c(dim(east)[[1]] + dim(west)[[1]], 
                   sum(east$Playoffs) + sum(west$Playoffs))
}

# results <- numeric()
results[2] <- sum(result[, 2])/sum(result[, 1])

for(i in 1:10){
  standings <- NBAStandingsByDate(thirty[i])
  east <- NBAPlayoffsFromStandings(standings$East)
  west <- NBAPlayoffsFromStandings(standings$West)
  result[i, ] <- c(dim(east)[[1]] + dim(west)[[1]], 
                   sum(east$Playoffs) + sum(west$Playoffs))
}

# results <- numeric()
results[3] <- sum(result[, 2])/sum(result[, 1])

for(i in 1:10){
  standings <- NBAStandingsByDate(forty[i])
  east <- NBAPlayoffsFromStandings(standings$East)
  west <- NBAPlayoffsFromStandings(standings$West)
  result[i, ] <- c(dim(east)[[1]] + dim(west)[[1]], 
                   sum(east$Playoffs) + sum(west$Playoffs))
}

# results <- numeric()
results[4] <- sum(result[, 2])/sum(result[, 1])

for(i in 1:10){
  standings <- NBAStandingsByDate(fifty[i])
  east <- NBAPlayoffsFromStandings(standings$East)
  west <- NBAPlayoffsFromStandings(standings$West)
  result[i, ] <- c(dim(east)[[1]] + dim(west)[[1]], 
                   sum(east$Playoffs) + sum(west$Playoffs))
}

# results <- numeric()
results[5] <- sum(result[, 2])/sum(result[, 1])

for(i in 1:10){
  standings <- NBAStandingsByDate(sixty[i])
  east <- NBAPlayoffsFromStandings(standings$East)
  west <- NBAPlayoffsFromStandings(standings$West)
  result[i, ] <- c(dim(east)[[1]] + dim(west)[[1]], 
                   sum(east$Playoffs) + sum(west$Playoffs))
}

# results <- numeric()
results[6] <- sum(result[, 2])/sum(result[, 1])

p <- results_full %>% 
  gather(games, total) %>% 
  filter(games != 5) %>%
  ggplot(aes(games, total, fill = games))

p + geom_boxplot(fatten = 1, lwd = 1) +
  scale_y_continuous("percentage of correct predictions", 
                     limits = c(.5, 1)) +
  scale_x_discrete("games") + 
  scale_fill_manual(values = c("#a6cee3", 
                               "#1f78b4",
                               "#b2df8a",
                               "#33a02c",
                               "#fb9a99",
                               "#e31a1c")) +
  geom_point(data = data.frame(games = as.factor(seq(10, 60, by = 10)), 
                               total = results), 
             colour = "grey60", size = 4) +
  geom_point(data = data.frame(games = as.factor(seq(10, 60, by = 10)), 
                               total = results), 
             colour = "navy", size = 2) +
  guides(fill = FALSE, col = FALSE) +
  theme_bw()
ggsave("~/conferences/nessis-15/standings.png")

p <- results_red %>% 
  gather(games, total) %>% 
  filter(games != 5) %>%
  ggplot(aes(games, total, fill = games))

p + geom_boxplot(fatten = 1, lwd = 1) +
  scale_y_continuous("percentage of correct predictions", 
                     limits = c(.5, 1)) +
  scale_x_discrete("games") + 
  scale_fill_manual(values = c("#a6cee3", 
                               "#1f78b4",
                               "#b2df8a",
                               "#33a02c",
                               "#fb9a99",
                               "#e31a1c")) +
  geom_point(data = data.frame(games = as.factor(seq(10, 60, by = 10)), 
                               total = results), 
             colour = "grey60", size = 4) +
  geom_point(data = data.frame(games = as.factor(seq(10, 60, by = 10)), 
                               total = results), 
             colour = "navy", size = 2) +
  guides(fill = FALSE, col = FALSE) +
  theme_bw()
