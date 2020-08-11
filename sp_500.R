# S&P 500
library(pacman)
p_load(rvest, tidyquant, dplyr, ggplot2, tidyr, tibbletime)

# GET AND PREPARE THE DATA  ###################################################
# get the 500 companies list
sp500 <- 
  read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% 
  html_nodes('#constituents') %>% 
  html_table()
sp500 <- sp500[[1]] %>% 
  select(symbol = Symbol, security = Security, founded = Founded,
         sector = `GICS Sector`, industry = 'GICS Sub Industry')

# get the ohlc data
sp500_ohlc <- tq_get(sp500$symbol, from = '2006-01-01', complete_cases = TRUE)
sp500_data <- sp500_ohlc %>% 
  left_join(sp500, by = 'symbol')

# simple moving average functions
# we'll use these to create columns for these in the data
sma_50_fun <- rollify(mean, window = 50)
sma_200_fun <- rollify(mean, window = 200)

sp500_data %>% 
  count(symbol) %>% 
  filter(n < 200)

# 1 CARR     100
# 2 OTIS     100
# 3 VIAC     171

# we will exclude these from the analyses as they don't have enough data 
# for the rolling 200 day average

# now we add some columns which we'll use later to analyse and compare companies
sp500_data <- sp500_data %>% 
  filter(!symbol %in% c('CARR', 'OTIS', 'VIAC')) %>% 
  group_by(symbol) %>% 
  mutate(day_diff = (close - lag(close, 1)) / lag(close, 1), 
         day_result = ifelse(close >= lag(close, 1), 'up', 'down'),
         year_diff = (close - lag(close, 365)) / lag(close, 365),
         sma_50 = sma_50_fun(close),
         sma_200 = sma_200_fun(close),
         signal = ifelse(sma_50 >= sma_200, 'buy', 'sell')) %>% 
  mutate(signal = ifelse(signal == lag(signal, 1), NA, signal)) %>% 
  ungroup()

sp500_data %>% filter(symbol == 'AAPL') %>% 
  ggplot(aes(date, close, colour = signal)) +
  geom_line()

sp500_yearly_gains <- 
  sp500_data %>% 
  filter(!is.na(day_result), symbol == 'A') %>% 
  group_by(symbol, year = lubridate::year(date), day_result) %>% 
  summarise(avg_diff = mean(day_diff)) %>% 
  spread(day_result, avg_diff)

sp500_day_results <- 
  sp500_data %>% 
  filter(!is.na(day_result)) %>% 
  count(symbol, year = lubridate::year(date), day_result) %>% 
  spread(day_result, n) %>%
  mutate(gain_pcnt = up / (up + down))

sp500_signal_count <- 
  sp500_data %>% 
  filter(!is.na(signal)) %>% 
  count(symbol, signal) %>% 
  spread(signal, n) %>%
  mutate(total_signals = buy + sell) %>% 
  arrange(desc(total_signals))

# LOOK FOR SECTOR TRENDS  #####################################################
sp500_data %>% 
  # filter(date >= as.Date('2014-01-01')) %>%  # optional date filter
  filter(`sector` == 'Materials') %>% 
  ggplot(aes(date, close)) +
  geom_line(alpha = 0.3, colour = 'grey20') + 
  facet_wrap(~ symbol, scales = 'free')