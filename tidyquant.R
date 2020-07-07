library(pacman)
p_load(tidyverse, tidyquant, timetk, tsibble, scales, highcharter, 
       broom, PerformanceAnalytics, tibbletime)

# GET DATA  ###################################################################
prices <- tq_get(c('MPNGF', 'JD', 'PDD', 'ITM'), 
                 from = '2005-01-01',
                 get = 'stock.prices')

# ADD ROLLING AVERAGES  #######################################################
sma_50 <- rollify(mean, window = 50)
sma_200 <- rollify(mean, window = 200)

prices <- prices %>%
  group_by(symbol) %>% 
  mutate(Ra_50 = sma_50(close),
         Ra_200 = sma_200(close))