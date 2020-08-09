library(pacman)
p_load(tidyverse, tidyquant, timetk, tsibble, scales, highcharter, 
       broom, PerformanceAnalytics, tibbletime, ggplot2)

# GET DATA  ###################################################################
prices <- tq_get(c('ACRS','AIRG','AMSC','AQMS','ASG','ASLN','AUG','AYRO','BKN',
                   'BLNK','CBAT','CYCN','DSKE','DZSI','ECF','EIGR','FCAP',
                   'FRSX','GALT','GF'), 
                 from = '2010-01-01',
                 get = 'stock.prices')

# tiingo
tq_get('AAPL', get = 'tiingo')


# alpha vantage
tq_get('AAPL', get = 'alphavantager', av_fun = 'TIME_SERIES_DAILY')
tq_get('AAPL', get = 'alphavantager', av_fun = 'INCOME_STATEMENT')

# ADD ROLLING AVERAGES  #######################################################
sma_50 <- rollify(mean, window = 50)
sma_200 <- rollify(mean, window = 200)

prices <- prices %>%
  group_by(symbol) %>% 
  mutate(Ra_50 = sma_50(close),
         Ra_200 = sma_200(close))

# MOVING AVERAGES PLOTS  ######################################################
prices %>% 
  #filter(symbol == 'AADR') %>% 
  ggplot() +
  geom_line(aes(date, close), alpha = 0.3) + 
  geom_line(aes(date, Ra_50), color = 'blue', lwd = 1) + 
  geom_line(aes(date, Ra_200), color = 'red') + 
  facet_wrap(~ symbol, scales = 'free') + 
  theme_light() + 
  labs(title = 'Close price with 50 (blue) and 200 (red) simple moving averages')

# HIGHCHARTER  ################################################################
prices %>% 
  filter(symbol == 'MPNGF') %>% 
  hchart('line', hcaes(date, close))

highchart() %>% 
  hc_add_series(data = prices %>% 
                  filter(symbol == 'JD'),
                'line', hcaes(date, close)) %>% 
  hc_add_series(data = prices %>% 
                  filter(symbol == 'JD'),
                'line', hcaes(date, Ra_200))

