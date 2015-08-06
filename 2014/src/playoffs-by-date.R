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
  return(rbind(playoffs, others))
  }
