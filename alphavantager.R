library(alphavantager)
library(jsonlite)

# set the api key
av_key <- av_api_key(Sys.getenv('alphavantage_key'))

# get time series data
av_get('AAPL', 'TIME_SERIES_DATA')

# function to extract fundamentals
get_fundamentals <- function(symbol, info_type = 'income', report_type = 'annual') {
  url_start <- 'https://www.alphavantage.co/query?function='
  inc <- 'INCOME_STATEMENT'
  bal <- 'BALANCE_SHEET'
  cash <- 'CASH_FLOW'
  url_symbol <- '&symbol='
  url_key <- '&apikey='
  api_url <- paste0(url_start, 
                    ifelse(info_type == 'income', 
                      paste0(inc, url_symbol, symbol, url_key, av_key),
                      ifelse(info_type == 'balance',
                             paste0(bal, url_symbol, symbol, url_key, av_key),
                             paste0(cash, url_symbol, symbol, url_key, av_key))))
  d <- jsonlite::fromJSON(api_url)
  report <- if(report_type == 'annual') {
    d[['annualReports']]} else {
    d[['quarterlyReports']]
    }
  report <- cbind(symbol, report)
  report
}

# use map to get fundamentals for more than one stock
library(purrr)
company_fundamentals <- map(c('AAPL', 'MSFT', 'AMZN', 'GOOG'), ~ get_fundamentals(.x)) %>% 
  bind_rows()

# plot some data
library(ggplot2)
company_fundamentals %>% 
  ggplot(aes(as.POSIXct(fiscalDateEnding), as.numeric(incomeTaxExpense))) + 
  geom_line() +
  facet_wrap(~ symbol) + 
  labs(x = '') + 
  theme_light()
