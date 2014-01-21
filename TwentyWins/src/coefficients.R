.project.path <- "~/Side_Projects/Nuggets/TwentyWins/"

## Dependencies:
source(paste(.project.path, "src/load.R", sep=""))
load("../data/glms.RData")

library(reshape2)

game.coefs <- ldply(games.glm, function(x) coef(x)[-1])

game.coefs$index <- 1:82

game.coefs.melt <- melt(game.coefs, id = "index")

p <- ggplot(data = game.coefs.melt, aes(x = index, y = exp(value), col = variable))
p + geom_line() +
  scale_x_continuous("number of games") +
  scale_y_continuous("change in odds ratio")

ggsave("../fig/coefficients.png")

png(filename = "../fig/record-coef.png", hei = 480, wid = 480)
plot(1:82, exp(game.coefs$Record), 
     xlab = "number of games played", 
     ylab = "increase in odds ratio",
     main = "Effect of Wins - Losses", 
     pch = 16, 
     col = "navy", 
     type = "b")
grid()

png(filename = "../fig/previous-coef.png", hei = 480, wid = 480)
plot(1:82, exp(game.coefs$Previous), 
     xlab = "number of games played", 
     ylab = "increase in odds ratio",
     main = "Effect of Previous Year's Success", 
      pch = 16, 
      col = "navy", 
      type = "b")
grid()

