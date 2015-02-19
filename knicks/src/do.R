## Ryan Elmore
## Date:
## Description:

.project_path <- "/Users/relmore/Side_Projects/Nuggets/knicks/"

## Dependencies:
source(paste(.project_path, "src/load.R", sep=""))


names(playoffs_df) <- c("year", "team", "playoffs", "prior")
record <- ldply(lapply(lapply(team_stats, 
                       function(x) c(x[dim(x)[1], "W"], x[dim(x)[1], "L"])), 
                       function(x) if (is.null(x)) list(W = NA, L = NA) else x),
                as.data.frame)
                
full_df <- tbl_df(cbind(playoffs_df, record)) %>% 
           mutate(N = W + L, diff = W - L, win_pct = W/N)
           

new_df <- tbl_df(cbind(full_df[1:360, ], full_df[31:390, 8:9])) %>%
          na.omit()
          
names(new_df)[10:11] <- c("diff_2", "win_pct_2")
fit <- lm(win_pct_2 ~ win_pct, data = new_df)

new_df <- tbl_df(cbind(new_df, predict(fit, interval="prediction", level = .75)))

p <- ggplot(new_df, aes(x = win_pct, y = win_pct_2)) 

p + geom_ribbon(aes(ymin = lwr, ymax = upr, fill='prediction'), alpha=0.3) +
    geom_rect(aes(xmin = .15, xmax = .25, ymin = 0, ymax = 1), fill = "#fc8d59", alpha = .05) +
    geom_smooth(method="lm", aes(fill='confidence'), alpha=0.3) +
    geom_smooth(method="lm", se = FALSE, color='blue') +
    geom_point() +
    scale_fill_manual('Interval', values = c('#ffffbf', '#91bfdb')) +
    scale_x_continuous('win %age current year') +
    scale_y_continuous('win %age the following year') + 
    geom_point(aes(x = win_pct, y = win_pct_2), 
               data = filter(new_df, team == "NYK"),
               size = 4, col = "navy") + 
    geom_point(aes(x = win_pct, y = win_pct_2), 
              data = filter(new_df, team == "NYK"),
              size = 2, col = "orange")
               

    
ggsave("../fig/knicks.pdf")
