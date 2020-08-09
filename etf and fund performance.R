library(pacman)
p_load(tidyverse, tidyquant, timetk, tsibble, scales, highcharter, 
       broom, PerformanceAnalytics, tibbletime, ggplot2)

# GET DATA  ###################################################################
etf_list <- readxl::read_excel("~/finance/long-term investing.xlsx", 
                                              sheet = "ETF2")

funds <- c('SPY', 'SPYD',  # s&p 500
           'VOO', 'VTWO', 'VXUS', 'VTV', 'VHT', 'VYM', 'VNQ', 'VTI', 'VERX', # vanguard
           'FTAL.L', 'ISF.L', 'UKDV.L', 'VMID.L', 'CSPI', 'HMWO.L', 'GBDV', 
           'EMIM.L', 'AGBP', 'FQAL')
fund_data <- tq_get(etf_list$Symbol,
                 from = '2005-01-01',
                 get = 'stock.prices')

# alpha vantage
tq_get('AAPL', get = 'alphavantager', av_fun = 'TIME_SERIES_DAILY')
tq_get('QQQ', get = 'alphavantager', av_fun = 'INCOME_STATEMENT')

fund_data <- fund_data %>% 
  group_by(symbol) %>% 
  mutate(year_close_diff = close - lag(close, n = 365),
         year_pcnt_diff_365 = year_close_diff / lag(close, n = 365))

fund_data %>% 
  filter(lubridate::day(date) == 1) %>% 
  ggplot(aes(date, year_pcnt_diff_365)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 0, colour = 'red') +
  facet_wrap(~ symbol) + 
  theme_light()
