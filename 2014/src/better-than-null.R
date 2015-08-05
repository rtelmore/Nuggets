## Ryan Elmore
## Null Model

library(dplyr)
playoffs <- read.csv(file = "2014/data/playoffs.csv", sep = ",", header = F)
playoffs <- tbl_df(playoffs) %>%
            na.omit() %>%
            mutate(correct = V3 == V4)

playoffs %>% 
  summarize(result = sum(correct)/386)

results_full <- readRDS(file = "2014/data/results_full.rds")
results_red <- readRDS(file = "2014/data/results_red.rds")
results_full$`5` %>%
  density() %>%
  plot()

sum(results_red$`10` > .7305699)/1000

plot(density(as.vector(results_full[, 1])))
