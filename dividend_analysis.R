# SET UP  #####################################################################
library(tidyverse)
library(tidyquant)
library(riingo)
library(purrr)
riingo_set_token(Sys.getenv('tiingo_key'))
source('finance_functions.R')

# DATA  #######################################################################
# list the companies in the S&P500
sp500_companies <- 
  tq_index("SP500") 

# this tells us every ticker supported by tiingo
tiingo_tickers <- 
  supported_tickers() %>% 
  pull(ticker)

# we'll work with only the top 30 SPY companies
sp500_top30 <-
  sp500_companies %>% 
  arrange(desc(weight)) %>%
  # We'll run this on the top 30, easily extendable to whole 500
  slice(1:30) %>% 
  filter(symbol %in% tiingo_tickers) %>% 
  pull(symbol)

# getting their data from tiingo gives us their dividend
sp500_top30_data <- 
  sp500_top30 %>% 
  riingo_prices(start_date = "2000-01-01") %>% 
  arrange(ticker) %>% 
  mutate(date = ymd(date))

# calculate dividend yield; sum of quarterly divs / close at first div
sp500_top30_yield <- sp500_top30_data %>% 
  group_by(ticker) %>% 
  filter(divCash > 0) %>% 
  mutate(year = year(date)) %>% 
  group_by(year, ticker) %>% 
  mutate(div_total = sum(divCash)) %>% 
  slice(1) %>% 
  mutate(div_yield = div_total/close) %>% 
  ungroup()

# plot the dividends and trend
sp500_top30_yield %>% 
  filter(year >= 2010) %>%
  left_join(sp500_index, by = c('ticker' = 'symbol')) %>% 
  ggplot(aes(year, div_yield)) +
  geom_line() +
  facet_wrap(~ ticker, scales = 'free_y') + 
  scale_x_continuous(breaks = c(2010, 2020), labels = c('2010', '2020')) + 
  scale_y_continuous(labels = scales::percent) + 
  theme_light() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  geom_smooth(method = 'lm', se = FALSE)


# model dividend yield against year and return the coefficients for each ticker
sp500_top30_yield %>% 
  coeffs(id = 'ticker', ycol = 'div_yield')
