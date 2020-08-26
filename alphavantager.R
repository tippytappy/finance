# SET UP  #####################################################################
library(alphavantager)
library(jsonlite)
library(dplyr)
library(purrr)

# set the api key
av_key <- av_api_key(Sys.getenv('av_key'))


# GET TIME SERIES DATA  #######################################################
# get time series data for 1 symbol
av_get('AAPL', 'TIME_SERIES_DATA')

# COMPANY OVERVIEWS  ##########################################################
# function to get company overview
get_overview <- function(symbol) {
  av_overview <- fromJSON(paste0('https://www.alphavantage.co/query?function=OVERVIEW&symbol=', 
                             symbol, '&apikey=', av_key))
  data.frame(t(sapply(av_overview,c)))
}

# get multiple overviews
company_overviews <- map(c('MSFT', 'AAPL'), ~ get_overview(.x)) %>% 
  bind_rows()


# FUNDAMENTALS  ###############################################################
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
  if(is.data.frame(report)) { 
    report
  } else {
    NULL
  }
}

# use map to get fundamentals for more than one stock
library(purrr)
income_statements <- map(sp500$symbol[1:20], 
                                ~ get_fundamentals(.x, 'income')) %>%
  bind_rows()

balance_sheets <- map(c('AAPL', 'MSFT', 'AMZN', 'GOOG'), 
                                ~ get_fundamentals(.x, 'balance')) %>% 
  bind_rows()

cashflow_statements <- map(sp500$symbol[1:20], 
                                 ~ get_fundamentals(.x, 'cash')) %>% 
  bind_rows()


# PLOT THE DATA  ##############################################################
# plot some data
library(ggplot2)
company_fundamentals_inc %>% 
  ggplot(aes(as.POSIXct(fiscalDateEnding), as.numeric(incomeTaxExpense))) + 
  geom_line() +
  facet_wrap(~ symbol) + 
  labs(x = '') + 
  theme_light()

