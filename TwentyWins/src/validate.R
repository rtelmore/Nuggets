
results.glm <- glm(Playoffs ~ Record + AvgDiff,
                   family = binomial,
                   data = results.df)

## validate after 20 games
## fit model to subset of the data (train = 250, test = 107)
set.seed(19838)
kTrials <- 1000
kGames <- 6
results.matrix <- matrix(0, nc = kGames, nr = kTrials)
games <- c(5, 10, 20, 30, 40, 50)

for (g in 1:kGames){

  for (j in 1:357) {
    results.df[j, 1:5] <- ProcessSeasonStatsByGame(team.stats.sub[[j]], game = games[g])
  }

  results.glm <- glm(Playoffs ~ Record + AvgDiff + Previous,
                     family = binomial,
                     data = results.df)

  for (i in 1:kTrials){
    index <- rep(F, 357)
    index[sample(1:357, size = 250, rep = F)] <- TRUE

    results.df.sub <- results.df[index, ]
    results.df.test <- results.df[!index, ]
    results.glm.sub <- glm(Playoffs ~ Record + AvgDiff + Previous,
                           family = binomial,
                           data = results.df.sub)

    results.sub.pred <- predict(results.glm.sub, 
                                newdata = results.df.test,
                                type = "response")
    results.matrix[i, g] <- mean((round(results.sub.pred) == results.df.test[, "Playoffs"]), na.rm = T)
  }  
} 
apply(results.matrix, 2, summary)

set.seed(19838)
kTrials <- 1000
kGames <- 6
results.matrix <- matrix(0, nc = kGames, nr = kTrials)
games <- c(5, 10, 20, 30, 40, 50)

for (g in 1:kGames){

  for (j in 1:357) {
    results.df[j, 1:6] <- ProcessSeasonStatsByGame(team.stats.sub[[j]], game = games[g])
  }

  results.glm <- glm(Playoffs ~ Record + AvgDiff + Away + Previous + BackToBack,
                     family = binomial,
                     data = results.df)

  for (i in 1:kTrials){
    index <- rep(F, 357)
    index[sample(1:357, size = 250, rep = F)] <- TRUE

    results.df.sub <- results.df[index, ]
    results.df.test <- results.df[!index, ]
    results.glm.sub <- glm(Playoffs ~ Record + AvgDiff + Away + Previous + BackToBack,
                           family = binomial,
                           data = results.df.sub)

    results.sub.pred <- predict(results.glm.sub, 
                                newdata = results.df.test,
                                type = "response")
    results.matrix[i, g] <- mean((round(results.sub.pred) == results.df.test[, "Playoffs"]), na.rm = T)
  }  
} 

apply(results.matrix, 2, summary)

#           [,1]   [,2]   [,3]   [,4]   [,5]   [,6]
# Min.    0.5794 0.7103 0.7196 0.7196 0.7570 0.7477
# 1st Qu. 0.7009 0.7757 0.7944 0.7944 0.8411 0.8505
# Median  0.7290 0.7944 0.8224 0.8131 0.8598 0.8692
# Mean    0.7262 0.7954 0.8195 0.8160 0.8629 0.8704
# 3rd Qu. 0.7477 0.8224 0.8411 0.8411 0.8879 0.8879
# Max.    0.8411 0.8785 0.9159 0.9159 0.9533 0.9533

#           [,1]   [,2]   [,3]   [,4]   [,5]   [,6]
# Min.    0.5849 0.7103 0.7196 0.7196 0.7570 0.7547
# 1st Qu. 0.7009 0.7757 0.8019 0.8019 0.8491 0.8585
# Median  0.7290 0.7944 0.8224 0.8208 0.8679 0.8692
# Mean    0.7283 0.7974 0.8219 0.8181 0.8654 0.8730
# 3rd Qu. 0.7547 0.8224 0.8411 0.8411 0.8879 0.8962
# Max.    0.8491 0.8785 0.9245 0.9159 0.9533 0.9533


          [,1]   [,2]   [,3]   [,4]   [,5]   [,6]
Min.    0.5327 0.6075 0.6916 0.7196 0.7664 0.7664
1st Qu. 0.6636 0.7103 0.7850 0.7944 0.8411 0.8505
Median  0.6916 0.7383 0.8037 0.8131 0.8598 0.8692
Mean    0.6873 0.7392 0.8063 0.8108 0.8622 0.8651
3rd Qu. 0.7103 0.7664 0.8318 0.8318 0.8879 0.8879
Max.    0.7850 0.8598 0.8972 0.9065 0.9533 0.9439

#           [,1]   [,2]   [,3]   [,4]   [,5]   [,6]
# Min.    0.5327 0.6075 0.6916 0.7196 0.7664 0.7664
# 1st Qu. 0.6636 0.7103 0.7850 0.7944 0.8411 0.8505
# Median  0.6916 0.7383 0.8037 0.8131 0.8598 0.8692
# Mean    0.6873 0.7392 0.8063 0.8108 0.8622 0.8651
# 3rd Qu. 0.7103 0.7664 0.8318 0.8318 0.8879 0.8879
# Max.    0.7850 0.8598 0.8972 0.9065 0.9533 0.9439
