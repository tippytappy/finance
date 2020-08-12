# S&P 500
# this script 
# - pulls the symbols for the S&P 500
# - pulls ohlc data for them since 2006
# - produces buy and sell signals based upon 50 and 200 day SMAs
# - looks for companies with a low number of signals, the last being 'buy'

# SET UP  #####################################################################
library(pacman)
p_load(rvest, tidyquant, dplyr, ggplot2, tidyr, tibbletime)

# GET AND AUGMENT THE DATA  ###################################################
# get the 500 companies list from wikipedia
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
sma_50_fun <- rollify(mean, window = 50)
sma_200_fun <- rollify(mean, window = 200)

# look for records with less than 200 records
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

# 

sp500_yearly_gains <- 
  sp500_data %>% 
  filter(!is.na(day_result), symbol == 'A') %>% 
  group_by(symbol, sector, year = lubridate::year(date), day_result) %>% 
  summarise(avg_diff = mean(day_diff)) %>% 
  spread(day_result, avg_diff)

sp500_day_results <- 
  sp500_data %>% 
  filter(!is.na(day_result)) %>% 
  count(symbol, year = lubridate::year(date), day_result) %>% 
  spread(day_result, n) %>%
  mutate(gain_pcnt = up / (up + down))

# I'm looking for companies with a low number of signals
# i.e. companies whose charts aren't too volatile.
# Let's find the total number of signals for each company
sp500_signal_count <- 
  sp500_data %>% 
  filter(!is.na(signal)) %>% 
  count(symbol, sector) %>% 
  rename(total_signals = n) %>% 
  ungroup()

# and let's determine whether the last signal was a buy or sell
sp500_last_signal <- 
  sp500_data %>% 
  select(symbol, date, signal) %>% 
  filter(!is.na(signal)) %>% 
  arrange(desc(date)) %>% 
  group_by(symbol) %>% 
  slice_min(1) %>% 
  ungroup() %>% 
  rename(last_signal = signal)

# now we put the two things together to look for 
# steady charts where the most recent signal was buy

sp500_steadies <- 
  sp500_signal_count %>% 
  left_join(sp500_last_signal, by = 'symbol') %>% 
  filter(total_signals < 10, last_signal == 'buy')

# now let's plot those
sp500_data %>% 
  filter(symbol %in% sp500_steadies$symbol) %>% 
  sma_chart()

# actually all I've done is choose young stocks with not much history
# let's work out the average time between signals
sp500_signal_separation <- 
  sp500_data %>% 
  select(symbol, date, signal) %>% 
  filter(!is.na(signal)) %>% 
  group_by(symbol) %>% 
  mutate(days_between_signal = difftime(date, lag(date, 1), units = 'days')) %>% 
  group_by(symbol) %>% 
  summarise(avg_days_between_signals = mean(days_between_signal, na.rm = TRUE)) %>% 
  ungroup()

sp500_steadies <- 
  sp500_signal_separation %>% 
  left_join(sp500_last_signal, by = 'symbol') %>% 
  filter(avg_days_between_signals > 350, last_signal == 'buy')

sp500_steadies <- 
  sp500_signal_separation %>% 
  left_join(sp500_last_signal, by = 'symbol') %>% 
  filter(last_signal == 'buy') %>% 
  arrange(desc(avg_days_between_signals))


sp500_data %>% 
  filter(symbol %in% sp500_steadies$symbol[1:20]) %>% 
  sma_chart()