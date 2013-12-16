## Ryan Elmore
## Date:
## Description:

.project.path <- "~/Side_Projects/Nuggets/TwentyWins/"

## Dependencies:
source(paste(.project.path, "src/load.R", sep=""))

results.df <- data.frame(Record = 1:357,
                         AvgDiff = 1:357,
                         MedDiff = 1:357,
                         Away = 1:357,
                         FinRecord = 1:357,
                         BackToBack = 1:357,
                         Playoffs = playoffs.df$V3[index],
                         Previous = playoffs.df$V4[index])

for (i in 1:357) {
  results.df[i, c(1:6)] <- ProcessSeasonStatsByGame(team.stats.sub[[i]], game = 20)
}

results.glm <- glm(Playoffs ~ Record + AvgDiff + Previous + BackToBack,
                   family = binomial,
                   data = results.df)


records <- seq(-18, 18, len = 100)
tmp <- exp(rep(results.glm$coef[1], 100) + 
               seq(-18, 18, len = 100)*results.glm$coef[2] +
               rep(0, 100)*results.glm$coef[3])

playoffs.prob <- tmp/(1 + tmp)  

pdf(paste(.project.path, "fig/prob_playoffs_20.pdf", sep = ""), h = 7, w = 7)
plot(records, playoffs.prob, xlab = "wins - losses",
     ylab = "probability of making the playoffs", lwd = 2, type = "l",
     ylim = c(0, 1))  
cols <- c("forestgreen", "darkorange", "navy", "darkred")
diffs <- c(-4, -2, 2, 4)
for(i in 1:4) {
  tmp <- exp(rep(results.glm$coef[1], 100) + 
                 records*results.glm$coef[2] +
                 rep(diffs[i], 100)*results.glm$coef[3])

  playoffs.prob <- tmp/(1 + tmp)  
  lines(records, playoffs.prob, col = cols[i], lwd = 2)
}
grid()
legend("bottomright", legend = c("-4", "-2", "0", "2", "4"),
       col = c("forestgreen", "darkorange", "black", "navy", "darkred"),
       lwd = rep(1,5))

## at 10 games
for (i in 1:357) {
  results.df[i, 1:5] <- ProcessSeasonStatsByGame(team.stats.sub[[i]], game = 10)
}

results.glm <- glm(Playoffs ~ Record + AvgDiff,
                   family = binomial,
                   data = results.df)


records <- seq(-18, 18, len = 100)
tmp <- exp(rep(results.glm$coef[1], 100) + 
               seq(-18, 18, len = 100)*results.glm$coef[2] +
               rep(0, 100)*results.glm$coef[3])

playoffs.prob <- tmp/(1 + tmp)  

pdf(paste(.project.path, "fig/prob_playoffs_10.pdf", sep = ""), h = 7, w = 7)
plot(records, playoffs.prob, xlab = "wins - losses",
     ylab = "probability of making the playoffs", lwd = 2, type = "l",
     ylim = c(0, 1))  
cols <- c("forestgreen", "darkorange", "navy", "darkred")
diffs <- c(-4, -2, 2, 4)
for(i in 1:4) {
  tmp <- exp(rep(results.glm$coef[1], 100) + 
                 records*results.glm$coef[2] +
                 rep(diffs[i], 100)*results.glm$coef[3])

  playoffs.prob <- tmp/(1 + tmp)  
  lines(records, playoffs.prob, col = cols[i], lwd = 2)
}
grid()
legend("bottomright", legend = c("-4", "-2", "0", "2", "4"),
       col = c("forestgreen", "darkorange", "black", "navy", "darkred"),
       lwd = rep(1,5))

## at 30 games
for (i in 1:357) {
 results.df[i, 1:5] <- ProcessSeasonStatsByGame(team.stats.sub[[i]], game = 30)
}

results.glm <- glm(Playoffs ~ Record + AvgDiff,
                  family = binomial,
                  data = results.df)


records <- seq(-18, 18, len = 100)
tmp <- exp(rep(results.glm$coef[1], 100) + 
              seq(-18, 18, len = 100)*results.glm$coef[2] +
              rep(0, 100)*results.glm$coef[3])

playoffs.prob <- tmp/(1 + tmp)  

pdf(paste(.project.path, "fig/prob_playoffs_30.pdf", sep = ""), h = 7, w = 7)
plot(records, playoffs.prob, xlab = "wins - losses",
    ylab = "probability of making the playoffs", lwd = 2, type = "l",
    ylim = c(0, 1))  
cols <- c("forestgreen", "darkorange", "navy", "darkred")
diffs <- c(-4, -2, 2, 4)
for(i in 1:4) {
 tmp <- exp(rep(results.glm$coef[1], 100) + 
                records*results.glm$coef[2] +
                rep(diffs[i], 100)*results.glm$coef[3])

 playoffs.prob <- tmp/(1 + tmp)  
 lines(records, playoffs.prob, col = cols[i], lwd = 2)
}
grid()
legend("bottomright", legend = c("-4", "-2", "0", "2", "4"),
      col = c("forestgreen", "darkorange", "black", "navy", "darkred"),
      lwd = rep(1,5))
