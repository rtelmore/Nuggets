## Ryan Elmore
## 5 Aug 2015
## playoffs-by-date.R

NBAPlayoffTeamsByDate <- function(date_string){
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
#   records <- readHTMLTable(url)[['teams_games']][c(1, 2, 6:8, 10:14)]
  r <- html(url)
  east <- html_table(r, fill = T)[[2]]
  east_playoffs <- 
    east <- east[-c(1, 7, 13), ]
  }
