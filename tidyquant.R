library(pacman)
p_load(tidyverse, tidyquant, timetk, tsibble, scales, highcharter, 
       broom, PerformanceAnalytics, tibbletime, ggplot2)

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

# MOVING AVERAGES PLOTS  ######################################################
prices %>% 
  #filter(symbol == 'MPNGF') %>% 
  ggplot() +
  geom_line(aes(date, close), alpha = 0.3) + 
  geom_line(aes(date, Ra_50), color = 'blue', lwd = 1) + 
  geom_line(aes(date, Ra_200), color = 'red') + 
  facet_wrap(~ symbol, scales = 'free') + 
  theme_light() + 
  labs(title = "Martin's Stocks", 
       subtitle = 'Close price with 50 (blue) and 200 (red) simple moving averages')