library(BatchGetSymbols)

# set tickers
tickers <- c('MSFT','GOOGL','JPM','GE')

# set dates
first_date <- Sys.Date()-10*365
last_date <- Sys.Date()
thresh_bad_data <- 0.95   # sets percent threshold for bad data
bench_ticker <- '^GSPC'   # set benchmark as ibovespa
cache_folder <- 'data/BGS_Cache' # set folder for cache

batch_symbols <- BatchGetSymbols(tickers = nt_holding[1:2],
                         first.date = first_date,
                         last.date = last_date,
                         bench.ticker = bench_ticker,
                         thresh.bad.data = thresh_bad_data)
batch_symbols[[2]]
