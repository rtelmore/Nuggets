## Ryan Elmore
## 5 Aug 2015
## playoffs-by-date.R

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
               "&year", y,
               "&lg_id=NBA", sep = "")
  r <- html(url)
  east <- html_table(r, fill = T)[[2]]
  west <- html_table(r, fill = T)[[3]]
  return(list(East = east, West = west))
}

NBAPlayoffsFromStandings <- function(standings){
  require(dplyr)
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

## Nuggets at 10, 20, 30, 40, 50 games for 5 season (back to 2005)
## 2014: Nov 18, Dec 7, Dec 30, Jan 19, Feb 10, Mar 5, Mar 23
## 2013: Nov 17, Dec 7, Dec 26, Jan 15, Feb 7, Mar 1, Mar 21
## 2011: Nov 15, Dec 7, Dec 28, Jan 16, Feb 4, Feb 25, Mar 19
## 2010: Nov 13, Dec 5, Dec 25, Jan 17, Feb 5, Mar 1, Mar 20
## 2009: Nov 16, Dec 4, Dec 26, Jan 15, Feb 6, Mar 1, Mar 20
## 2008: Nov 17, Dec 6, Dec 30, Jan 21, Feb 10, Mar 5, Mar 23
## 2007: Nov 22, Dec 13, Jan 5, Jan 26, Feb 12, Mar 9, Mar 28
## 2006: Nov 18, Dec 9, Dec 29, Jan 18, Feb 6, Mar 4, Mar 24
## 2005: Nov 21, Dec 12, Jan 2, Jan 23, Feb 11, Mar 7, Mar 30
