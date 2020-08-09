# SET UP  #####################################################################
library(pacman)
p_load(readxl, dplyr, quantmod, ggplot2, tidyquant, tidyr)

# TRADES  #####################################################################
nt_shares <- read_excel("~/finance/share trading/naked trader.xlsx", 
                        sheet = "shares", guess_max = 30) %>% 
  mutate(type = 'shares')
nt_shorts <- read_excel("~/finance/share trading/naked trader.xlsx", 
                        sheet = "shorts", guess_max = 30) %>% 
  mutate(type = 'shorts')
nt_longs <- read_excel("~/finance/share trading/naked trader.xlsx", 
                        sheet = "longs", guess_max = 30) %>% 
  mutate(type = 'longs')

# combine the tables and add metrics to measure effectiveness
nt_all <- nt_shares %>% 
  bind_rows(nt_shorts) %>% 
  bind_rows(nt_longs) %>% 
  mutate(type = factor(type, levels = c('shares', 'shorts', 'longs')),
         stake = Price * Qty,
         pcnt_change = (Sell-Price) / Price,
         held_for = difftime(`Sell Date`, `Buy Date`, units = 'days'),
         result_type = case_when(
           `P/L` >= 0 ~ 'profit',
           is.na(`Sell Date`) ~ 'holding',
           `P/L` < 0 ~ 'loss')) %>% 
  select(type, stock = Stock, symbol = Epic, 
         buy_date = `Buy Date`, sell_date = `Sell Date`, held_for, 
         quantity = Qty, target = Target, stop = Stop,
         price = Price, sell = Sell, pcnt_change,
         stake, profit = `P/L`, result_type)

nt_complete <- nt_all %>% 
  filter(complete.cases(nt_all), held_for >= 0) 

rm(nt_shares, nt_longs, nt_shorts)

# GET DATA  ###################################################################
# we'll look at the companies he's holding just for the sake of testing
nt_holding <-
  nt_all %>% 
  filter(is.na(`sell_date`)) %>% 
  pull(symbol) %>%
  unique() %>%
  toupper()

# yahoo appends '.L' to uk shares so we have to try the epic and amended epic
nt_holding <- append(nt_holding, paste0(nt_holding, '.L')) %>% 
  sort()

# we'll use quantmod to get the fundamentals
# getQuote returns various fundamental information; this is what we'll get
qm_info <- c(
  'Name',
  'Market',
  'Market Capitalization',
  'Shares Outstanding',
  'Average Daily Volume',
  'Earnings/Share',
  'EPS Forward',
  'P/E Ratio',
  'Price/EPS Estimate Next Year',
  'Book Value',
  'Price/Book',
  'Dividend/Share',
  'Dividend Yield',
  'Ex-Dividend Date',
  '50-day Moving Average',
  'Percent Change From 50-day Moving Average 40',
  '200-day Moving Average',
  'Percent Change From 200-day Moving Average'
)

# get fundamentals
nt_holding_fundamentals <- getQuote(c(nt_holding), 
                                   what = yahooQF(qm_info)) %>% 
  tibble::rownames_to_column('symbol') %>% 
  filter(Market == 'gb_market')

# get ohlc
nt_holding_ohlc <- 
  tq_get(nt_holding_fundamentals$symbol[1:5])
nt_holding_perf <- nt_holding_ohlc %>% 
  group_by(symbol) %>% 
  mutate(day_diff = (close - lag(close, 1)) / lag(close, 1),
         year_diff = (close - lag(close, 365)) / lag(close, 365))

# historic closing charts]}
nt_holding_perf %>% 
  ggplot(aes(x = date, y = close)) +
  geom_line() +
  facet_wrap(~ symbol, scales = 'free')

# year difference percentage charts
nt_holding_perf %>% 
  filter(abs(day_diff) < 0.9,
         !is.na(year_diff)) %>% 
  ggplot(aes(x = date, y = year_diff)) +
  geom_line() +
  facet_wrap(~ symbol, scales = 'free')

# yearly gains summary
nt_holding_perf %>% 
  filter(abs(day_diff) < 0.9,
         !is.na(year_diff)) %>% 
  group_by(symbol) %>% 
  mutate(result = ifelse(year_diff >= 0, 'gain', 'loss')) %>% 
  count(symbol, result) %>% 
  spread(result, n)
  
# daily gains cumulative distribution plot
nt_holding_perf %>% 
  filter(abs(day_diff) < 0.9) %>% 
  ggplot(aes(day_diff, color = symbol)) +
  scale_y_continuous(breaks = seq.int(0, 1, .1)) + 
  geom_hline(yintercept = 0.5, colour = 'red') +
  stat_ecdf()

# histograms
nt_holding_perf %>% 
  filter(abs(day_diff) < 0.9) %>% 
  ggplot(aes(x = day_diff)) +
  geom_histogram(bins = 50) + 
  facet_wrap(~ symbol)



# unique symbols
nt_symbols <- unique(nt_all$symbol)
nt_symbols <- paste0(nt_symbols, '.L')
nt_fundamentals <- getQuote(nt_symbols[1:5], what = yahooQF(qm_info))

nt_fundamentals <- map(1:50, ~ getQuote(nt_symbols[.x], what = yahooQF(qm_info))) %>% 
  bind_rows()

# PERFORMANCE  ################################################################
# yearly performance stats
nt_summary <- nt_complete %>%
  mutate(year = lubridate::year(buy_date)) %>% 
  group_by(type, year) %>% 
  summarise(median_quantity = median(quantity), 
            median_stake = median(stake),
            total_profit = sum(profit), 
            median_profit = median(profit),
            max_profit = max(profit),
            median_held = median(held_for), 
            min_held = min(held_for),
            median_pcnt_change = median(pcnt_change))

# exclude incomplete records
nt_complete %>% 
  ggplot(aes(stake)) +
  geom_histogram(bins = 100)
