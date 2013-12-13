## Ryan Elmore
## Simple web app for Logistic Regression

## Prelims
.base.api <- "http://www.basketball-reference.com/teams/"
load(file = "glms.RData")

GetNBASeasonTeamByYear <- function(team, year){
  require(XML)
  require(lubridate)
  html <- paste(.base.api, team, "/", year, "_games.html", sep="")
  stats <- readHTMLTable(html)[['teams_games']][c(1, 2, 4:6, 8:12)]
  stats <- stats[-c(21, 42, 63, 84), ]
  stats[, c(1, 6:9)] <- apply(stats[, c(1, 6:9)], 2, as.numeric)
  stats$Diff <- stats$Tm - stats$Opp
  stats$AvgDiff <- cumsum(stats$Diff)/stats$G
  stats$Away <- cumsum(stats[, 3] == '@')
  stats$BackToBack <- c(NA, as.vector(diff(mdy(stats$Date))))
  return(stats)
}

## Get uptodate Nuggets info
Nugs <- GetNBASeasonTeamByYear("DEN", 2014)
Nugs <- Nugs[-c(21, 42, 63, 84), ]

shinyServer(function(input, output) {
  output$LOGISTICPLOT <- renderPlot({
    index <- mdy(Nugs$Date) < as.POSIXct(input$DATE)
    games <- sum(index)
    backtoback <- sum(c(0, as.vector(diff(mdy(Nugs$Date))))[index] == 1)
    away <- cumsum(Nugs[index, 3] == '@')
    model <- games.glm[[games]]
    coef.model <- coef(model)
    playoff.probs <- 1/(1 + exp(-(coef.model[1] + 
                                coef.model[2]*(-20:20) + 
                                coef.model[3]*input$POINTDIFF +
                                coef.model[4]*away[games] +
                                coef.model[5] +
                                coef.model[6]*backtoback)))
                                
    plot(seq(-20, 20, by = 1), playoff.probs, 
         ylab = "Playoff Probability", 
         xlab = paste("Wins - Losses After ", games, " Games", sep = ""), 
         type="l", col="red", lty=2, lwd=2, ylim=c(0,1))
    abline(h = 0.5, lty = 3, col = "navy")
    prob.at.input <- 1/(1 + exp(-(coef.model[1] + 
                                coef.model[2]*(input$WINS) + 
                                coef.model[3]*(input$POINTDIFF) +
                                coef.model[4]*away[games] +
                                coef.model[5] +
                                coef.model[6]*backtoback)))

    points(input$WINS, prob.at.input, pch=18, cex=2)
    
    text.pos <- ifelse(prob.at.input <= .5, 3, 1)
    text(input$WINS,  prob.at.input, round(prob.at.input, 2), 
         pos = text.pos, offset = .6, cex = 1.3)
  })
  
})
