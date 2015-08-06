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
  playoffs <- standings[seq(1, 26, by = 5)]
  leftovers <- league[!(league$team %in% playoffs), ]
  leftovers <- leftovers[ order(leftovers$conf, -leftovers$prob), ]
  playoffs <- c(playoffs, leftovers$team[c(1:5, 13:17)])    
}
