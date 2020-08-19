# S&P 500  ####################################################################
# this script 
# - pulls the symbols for the S&P 500
# - pulls ohlc data for them since 2006
# - produces buy and sell signals based upon 50 and 200 day SMAs
# - looks for companies with a low number of signals, the last being 'buy'

# SET UP  #####################################################################
library(pacman)
p_load(rvest, tidyquant, dplyr, ggplot2, tidyr, tibbletime)

# set the alphavantage api key
av_key <- av_api_key(Sys.getenv('av_key'))

# SIMPLE MOVING AVERAGES CHART  ###############################################
sma_chart <- function(x) {
  x %>% 
  ggplot(aes(date, close)) +
    geom_line(colour = 'grey50', alpha = 0.5) + 
    geom_ma(n = 50, colour = 'darkgreen', lwd = 1) + 
    geom_ma(n = 200, colour = 'darkorange', lwd = 1) +
    facet_wrap(~ symbol, scales = 'free') + 
    theme_light() + 
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(colour = 'black'))
}

# SIGNALS CHART  ##############################################################
signals_chart <- function(x) {
  ggplot() +
    geom_line(data = x, aes(date, close), colour = 'grey50', 
              alpha = 0.5) + 
    geom_line(data = x, aes(date, sma_50), colour = 'darkgreen', lwd = 1) + 
    geom_line(data = x, aes(date, sma_200), colour = 'darkorange', lwd = 1) +
    geom_point(data = x %>% filter(!is.na(signal)),
               aes(date, sma_50, colour = signal), size = 2) + 
    scale_color_manual(values=c("blue", "red")) + 
    facet_wrap(~ symbol, scales = 'free') + 
    theme_light() + 
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(colour = 'black'))
}

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
sp500_data <- tq_get(sp500$symbol, from = '2006-01-01', complete_cases = TRUE)
sp500_data <- sp500_ohlc %>% 
  left_join(sp500, by = 'symbol')

# simple moving average functions
sma_50_fun <- rollify(mean, window = 50)
sma_200_fun <- rollify(mean, window = 200)

# look for records with less than 200 records
sp500_under_200 <- 
  sp500_data %>% 
  count(symbol) %>% 
  filter(n < 200)

# we will exclude these from the analyses as they don't have enough data 
# for the rolling 200 day average

# now we add some columns which we'll use later to analyse and compare companies
sp500_data <- sp500_data %>% 
  filter(!symbol %in% sp500_under_200$symbol) %>% 
  group_by(symbol) %>% 
  mutate(day_diff = (close - lag(close, 1)) / lag(close, 1), 
         day_result = ifelse(close >= lag(close, 1), 'up', 'down'),
         year_diff = (close - lag(close, 252)) / lag(close, 252),
         sma_50 = sma_50_fun(close),
         sma_200 = sma_200_fun(close),
         signal = ifelse(sma_50 >= sma_200, 'buy', 'sell')) %>% 
  mutate(signal = ifelse(signal == lag(signal, 1), NA, signal)) %>% 
  ungroup()

# SECTOR ANALYSIS  ############################################################
# let's start with a chart of the average close price by sector
sp500_data %>% 
  group_by(sector, date) %>%
  summarise(avg_close = mean(close, na.rm = TRUE)) %>% 
  ggplot(aes(date, avg_close)) +
  geom_line() + 
  facet_wrap(~ sector, scales = 'free')

# based on the charts I like the look of
#  - information technology
#  - consumer discretionary
#  - health care

# those sectors didn't seem to take as much of a hit in 2008
# at least when averaging the close of the companies in those sectors

# and a table of how many companies are in each sector
sp500 %>% count(sector) %>% arrange(desc(n))

# 1             Industrials 73
# 2  Information Technology 71
# 3              Financials 66
# 4             Health Care 62
# 5  Consumer Discretionary 61
# 6        Consumer Staples 33
# 7             Real Estate 31
# 8               Materials 28
# 9               Utilities 28
# 10 Communication Services 26
# 11                 Energy 26

# the sectors we've chosen have lots of companies
# do their industries have any trends?

# since we'll be doing the same analysis a few times let's write
# functions to avoid copy-pasting
# first a chart to plot the different industries in a chose sector
industries_chart <- function(x) {
  sp500_data %>% 
    filter(sector %in% x) %>%
    group_by(industry, date) %>%
    summarise(avg_close = mean(close)) %>% 
    ggplot(aes(date, avg_close)) +
    geom_line() + 
    facet_wrap(~ industry, scales = 'free') 
}

# next a chart for the companies within a chosen sector and industry
companies_chart <- function(x, y) {
  sp500_data %>% 
    filter(sector %in% x, industry %in% y) %>%
    ggplot(aes(date, close)) +
    geom_line() + 
    facet_wrap(~ industry + symbol, scales = 'free', ) 
}

# now we have those let's start with information technology

# SECTOR FOCUS: INFORMATION TECHNOLOGY  #######################################
sector_1 <- 'Information Technology'

# # first we plot the industry averages
industries_chart(sector_1)

# there are 13 industry headings
# I like the look of
# - application software
# - systems software

# within those two industries are there any stand out companies?
companies_chart(sector_1, 
                c('Application Software', 'Systems Software'))

# of these I like
# - MSFT
# - ADBE
# - TYL

# SECTOR FOCUS: CONSUMER DISCRETIONARY  #######################################
sector_2 <- 'Consumer Discretionary'

# again we start with the industry averages
industries_chart(sector_2)

# then look at the companies in the attractive industries
companies_chart(sector_2, 
                c('Home Building', 'Home Improvement Retail',
                  'Internet & Direct Marketing Retail'))

# of these I like
# - HD
# - AMZN


# SECTOR FOCUS: HEALTH CARE  #################################################
sector_3 <- 'Health Care'

# industry averages
industries_chart(sector_3)

# companies in the attractive industries
companies_chart(sector_3, 
                c('Health Care Equipment', 'Health Care Supplies',
                  'Life Sciences Tools & Services'))

# of these I like
# - BIO
# - TMO
# - DXCM
# - DHR
# - RMD
# - WST


# COMPANY FOCUS
liked_companies <- c('MSFT', 'ADBE', 'TYL', 'HD', 'AMZN',
                     'BIO', 'TMO', 'DXCM', 'DHR', 'RMD', 'WST')

# let's plot the simple moving averages charts for the companies we liked
sp500_data %>% 
  filter(symbol %in% liked_companies) %>% 
  sma_chart()
  
yearly_trend_chart <- function(x) {
  x %>% 
  ggplot(aes(date, year_diff)) +
    geom_line(colour = 'grey80') + 
    geom_smooth(se = FALSE, method = 'lm', colour = 'grey32', lwd = 1.2) +
    geom_hline(yintercept = 0, colour = 'red', lwd = 0.5) + 
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~ symbol, scales = 'free') + 
    theme_light() + 
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(colour = 'black')) + 
    labs(x = '', y = '1 year growth')
}


# STEADIES  ###################################################################
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
  filter(last_signal == 'buy') %>% 
  arrange(desc(avg_days_between_signals))

# plot the 20 companies with the highest numbers of days between signals
sp500_data %>% 
  filter(symbol %in% sp500_steadies$symbol[1:20]) %>% 
  sma_chart()

# RISING YEARLY PERCENT DIFFERENCE  ###########################################
# and let's look for companies whose percentage difference between
# values a year apart is rising.
# the idea here is to see the likelihood I'll be better off after a year
sp500_data %>% 
  filter(symbol %in% liked_companies) %>% 
  ggplot(aes(date, year_diff)) +
  geom_line(colour = 'grey80') + 
  geom_smooth(se = FALSE, method = 'lm') +
  geom_hline(yintercept = 0, colour = 'red', lwd = 0.5) +
  facet_wrap(~ symbol, scales = 'free') + 
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(colour = 'black'))

# get the linear model coefficient for the year_diff column
# this tells us whether the 1 year difference between close prices
# is rising or falling
library(tidyr)
library(purrr)
sp500_yearly_trends <- 
  sp500_data %>% 
  filter(!is.na(year_diff)) %>% 
  select(symbol, date, year_diff) %>% 
  nest(data = -symbol) %>% 
  mutate(model = map(.x = data, ~ lm(formula = year_diff ~ date, data = .x)),
         year_diff_coeff = map(.x = model, ~ coefficients(.x)[[2]]))

# turn this into a self-contained function
# takes a tidyquant data frame
# returns a data frame of symbol and yearly gain lm coefficient
yearly_gain_coeff <- function(x) {
  df <- x %>% 
    mutate(year_diff = (close - lag(close, 252) / lag(close, 252))) %>% 
    filter(!is.na(year_diff)) %>% 
    select(symbol, date, year_diff) %>% 
    nest(data = -symbol) %>% 
    mutate(model = map(data, ~ lm(formula = year_diff ~ date, data = .x)),
           year_diff_coeff = map(model, ~ coefficients(.x)[[2]]))
  data.frame(symbol = df$symbol, 
             year_diff_coeff = unlist(df$year_diff_coeff))
}

yearly_gain_coeff(equity_screener_data)

equity_screener_data %>% 
  #filter(symbol == 'SE') %>% 
  sma_chart()
  

sp500_data %>% 
  filter(symbol %in% 
           head(sp500_yearly_trends$symbol, 20)) %>% 
  yearly_trend_chart()

# let's add the sp500 date so we can filter this list
# by industry or sector
sp500_yearly_trends <- 
  sp500_yearly_trends %>%
  left_join(sp500, by = 'symbol')

# some of the results look suspect, e.g. DOW; it has hardly any records
# let's add the number of days for which each stock has data
# so we can filter by the number of days as well

sp500_yearly_trends <- 
  sp500_yearly_trends %>%
  left_join(sp500_data %>% 
              group_by(symbol) %>% 
              summarise(no_records = n()), by = 'symbol')

# now we can look for rising stocks with at least 5 years' data
sp500_yearly_trends %>% 
  filter(year_diff_coeff * 252 > 0.03, no_records >= 252 * 5)

sp500_yearly_trends %>% 
  filter(no_records >= 252 * 5) %>% 
  select(symbol, year_diff_coeff) %>% 
  head(10)
