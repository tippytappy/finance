# SET UP  #####################################################################
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(highcharter)
source('finance_functions.R')

# DATA  #######################################################################
# add a method for automatically getting symbols
# e.g. the yahoo screener
# high yield bonds
# hy_bonds <- get_screen_symbols(6, 20)

# Martin's companies
companies <- c('LIT', 'BOTZ', 'WCLD', 'WTAI.L')

# high 5 year return ETFs
companies <- c('SLVP','SILJ','CHIQ','SPYG','ENZL','XT','PPLC','PBD','ROBO',
               'CIBR','TDIV','PTNQ','ICLN','FNI','FAN')

# get ohlc data using quantmod
getSymbols(companies)

# combine the companies' data to make analysis easier
returns <- companies %>% get_returns()
closes <- companies %>% merge_multiple_xts()
volumes <- companies %>% merge_multiple_xts('volume')


# PLOTS  ######################################################################
# all companies closing price for the last 5 years
closes['2015/2020'] %>% 
  pivot_xts('close') %>% 
  ggplot(aes(date, close)) + 
  geom_line(colour = 'grey60') + 
  facet_wrap(~ symbol, scales = 'free')

# one or more companies in an interactive chart
highchart(type = 'stock') %>% 
  hc_add_series(SLVP, name = 'SLVP') %>% 
  hc_add_series(CHIQ)

# single company advanced chart with indicators
highchart(type = "stock") %>% 
  hc_yAxis_multiples(create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)) %>% 
  hc_add_series(SLVP, yAxis = 0, name = "SPY") %>% 
  hc_add_series(SMA(Cl(SLVP), n = 5), yAxis = 0, name = "Fast MA") %>% 
  hc_add_series(SMA(Cl(SLVP), n = 100), yAxis = 0, name = "Slow MA") %>% 
  hc_add_series(SLVP$SLVP.Volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>% 
  hc_add_series(RSI(Cl(SLVP)), yAxis = 2, name = "Osciallator", color = hex_to_rgba("green", 0.7)) %>%
  hc_add_series(xts(rep(70, NROW(SLVP)), index(SLVP)), color = hex_to_rgba("red", 0.7), yAxis = 2, name = "Sell level") %>% 
  hc_add_series(xts(rep(30, NROW(SLVP)), index(SLVP)), color = hex_to_rgba("blue", 0.7), yAxis = 2, name = "Buy level") %>% 
  hc_tooltip(valueDecimals = 2) %>% 
  hc_size(height = 700)


# STATISTICS  #################################################################
# distribution
returns['2015/2020'] %>% 
  pivot_xts('return') %>% 
  ggplot(aes(x = return)) +
  geom_density() +
  geom_vline(xintercept = 0, colour = 'red') +
  facet_wrap(~ symbol) + 
  labs(y = '')

# stats
mean.geometric(returns)
table.Distributions(returns)
SkewnessKurtosisRatio(returns)

table.Autocorrelation(returns)
closes['2019/2020'] %>% CalculateReturns()
returns['2015/2020'] %>% Return.annualized()
SLVP %>% to.yearly() %>% Return.calculate()

# RETURNS ##############################################################
# plot
get_returns(companies) %>% 
  pivot_xts('return') %>% 
  ggplot(aes(date, return)) + 
  geom_line(colour = 'grey60') +
  geom_smooth(method = 'lm', colour = 'black') + 
  facet_wrap(~ symbol)

# lm coefficient
get_returns(companies) %>% 
  pivot_xts('return') %>% 
  get_coeffs() %>% 
  arrange(desc(coeff))

data(mana)

# DIVIDENDS  ##################################################################
# getting their data from tiingo gives us their dividend
library(riingo)
riingo_set_token(Sys.getenv('tiingo_key'))
dividends <- 
  companies %>% 
  riingo_prices(start_date = "2010-01-01") %>% 
  arrange(ticker) %>% 
  mutate(date = ymd(date)) %>% 
  rename(symbol == ticker)

# dividend yield
dividend_yield <- dividends %>% 
  group_by(symbol) %>% 
  filter(divCash > 0) %>% 
  mutate(year = year(date)) %>% 
  group_by(year, symbol) %>% 
  mutate(div_total = sum(divCash)) %>% 
  slice(1) %>% 
  mutate(div_yield = div_total/close) %>% 
  ungroup()

# plot dividend yield and trend
dividend_yield %>% 
  ggplot(aes(date, div_yield)) + 
  geom_line(colour = 'grey60') +
  geom_smooth(method = 'lm', colour = 'black', se = FALSE) + 
  facet_wrap(~ symbol)

# quantify dividend trend
dividend_yield %>% 
  get_coeffs(ycol = 'div_yield') %>% 
  arrange(desc(coeff)) %>% 
  arrange(desc(coeff))

# PORTFOLIO ANALYSIS  #########################################################
