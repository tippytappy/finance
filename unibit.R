# getting historic fundamentals using unibit

library(jsonlite)
set_ev()
key <- Sys.getenv('unibit_key')

paste(sp500_yearly_trends$symbol[1:20], collapse = ',')

col_fun <- function(x, y) {
  x$company <- y
  x
}

unibit_companies <- sp500_yearly_trends$symbol[1:20]

# unibit function
unibit_financials <- function(companies, financial = 'BS') {
  # make the url
  url <- paste0('https://api.unibit.ai/v2/company/coreFinancials?tickers=',
                  paste(companies, collapse = ','),
                  '&statement=',
                  financial,
                  '&startDate=2015-01-01&endDate=2020-01-01&interval=A&accessKey=',
                  key)
  jsonlite::read_json(url, simplifyVector = TRUE)[['result_data']]
}

# get the financials
unibit_bs <- unibit_financials('MSFT', 'BS')
unibit_is <- unibit_financials('AAPL', 'IS')
unibit_cf <- unibit_financials('AAPL', 'CF')

data.frame(source = 'balance sheet', item = names(unibit_bs$MSFT)) %>% 
  bind_rows(data.frame(source = 'income statement', item = names(unibit_is$AAPL))) %>%
  bind_rows(data.frame(source = 'cash flow', item = names(unibit_cf$AAPL))) %>% xlo()


# need to make a columns vector because it seems like
# sometimes the results data frames have their columns
# in different orders
unibit_cols <- names(unibit_test[[1]])

# get the data
unibit_is <- read_json(unibit_url, simplifyVector = TRUE)[['result_data']]

map2_df(unibit_test, unibit_companies, col_fun) %>% 
  select(company, report_date, all_of(unibit_cols))


bind_rows(unibit_test$result_data) %>% View()
library(purrr)
# unibit_df <- 
unibit_output %>% 
  map_df(~ .x)

z <- unibit_output[1]
z2 <- z[[1]]
  
?unstack()
