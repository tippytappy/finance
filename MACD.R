# MACD  #######################################################################

# Hypothesis:
# Stocks with a pattern of regular rises and falls could be profitable
# E.g. a sine wave; buy in the valley, sell at the peak. Repeat.
# Being able to find stocks like that could yield modest, low-risk returns
# if managed closely.

# Look for stocks...
# - with a reasonably dependable rise-fall pattern
# - with a reasonable difference between max positive and negative MACD/signal
# - large, infrequent crosses, not lots of inconsequential crosses
# - that aren't too new and/or
# - are well-established, e.g. in the S&P 500, FTSE.


# Testing the theory:
# Find tickers to analyse
# Use MACD to add buy and sell indicators to the data
# backtest
# - calculate the difference between the buy-sell price to get returns

# SET UP  #####################################################################
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(highcharter)
library(riingo)
library(rvest)
library(tidyr)
library(purrr)
source('finance_functions.R')

# GET S&P 500 symbols  ########################################################
# get the 500 companies list from wikipedia
sp500 <- 
  read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% 
  html_nodes('#constituents') %>% 
  html_table()
sp500 <- sp500[[1]] %>% 
  select(symbol = Symbol, security = Security, founded = Founded,
         sector = `GICS Sector`, industry = 'GICS Sub-Industry')

# GET COMPANY DATA  ###########################################################
# make a list of ticker symbols to get data for
companies <- sp500$symbol[1:100]

# get a table of available symbols from tiingo
tiingo_symbols <- supported_tickers()

# find S&P symbols with enough data
companies <- sp500 %>%
  inner_join(tiingo_symbols, by = c('symbol' = 'ticker')) %>% 
  filter(startDate <= as.POSIXct('2017-01-01')) %>%
  pull(symbol)

# get the data from tiingo
companies %>% getSymbols.tiingo(api.key = Sys.getenv('tiingo_key'), 
                                from = '2017-01-01',
                                env = '.GlobalEnv',
                                adjust = TRUE) # adjust for dividends and splits

# CROSSES  ####################################################################
# make a function to do add the indicators
# the function takes a vector of symbols so we can use the companies vector
ma_fast <- 13
ma_slow <- 48

add_indicators <- function(x) {
  # make the tidy data
  df <- x %>%
    merge_multiple_xts() %>% 
    pivot_xts('close') 
  # add the indicators
  df %>% 
    select(close) %>% 
    MACD(nFast = ma_fast, nSlow = ma_slow) %>% 
    data.frame() %>%
    bind_cols(df) %>% 
    select(date, symbol, close, everything()) %>% 
    mutate(indicator = ifelse(macd > signal, 'buy', 'sell'),
           crosses = ifelse(!indicator == lag(indicator, 1), 'cross', NA))
}

# test with one symbols
'ABT' %>% add_indicators()

# make a data set of multiple symbols and their crosses
crosses <- companies %>% purrr::map_df(~ add_indicators(.x))

# SUMMARY STATS  ##############################################################
price_differences <- 
  crosses %>% 
  #filter(date > as.POSIXct('2020-01-01')) %>% 
  filter(crosses == 'cross') %>% 
  mutate(priceDiff = ifelse(indicator == 'sell', close - lag(close, 1), NA)) %>% 
  filter(!is.na(priceDiff))

summary <- 
  price_differences %>% 
  group_by(symbol) %>% 
  summarise(cross_count = n(),
            avg_price_diff = mean(priceDiff, na.rm = TRUE)) %>% 
  ungroup()

# need to modify the summary
# at the moment the price difference crosses symbols
# e.g. first of next minus last of previous

# things to add to the summary
# count of gains
# count of losses
# average gain
# average loss

# quick plots  #################################################
test_indicators_multiple %>%
    filter(symbol == 'ABBV') %>%
    filter(date >= as.POSIXct('2020-01-01')) %>% 
  ggplot() + 
  geom_path(aes(date, close, colour = factor(indicator), group = 1), lwd = 1) + 
  scale_colour_hue(direction = -1) +
  geom_line(aes(date, macd)) + 
  geom_line(aes(date, signal)) + 
  geom_hline(yintercept = 0, colour = 'blue')

price_differences %>% 
  filter(symbol == 'CMG') %>%
  filter(date > as.POSIXct('2020-01-01')) %>% 
  ggplot(aes(date, priceDiff)) + 
  geom_col()

chart_symbol('MMM', fast_n = ma_fast, slow_n = ma_slow)