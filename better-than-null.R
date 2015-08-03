## Ryan Elmore
## Null Model

library(dplyr)
playoffs <- read.csv(file = "2014/data/playoffs.csv", sep = ",", header = F)
playoffs <- tbl_df(playoffs) %>%
            na.omit() %>%
            mutate(correct = V3 == V4)

playoffs %>% 
  summarize(result = sum(correct)/386)

