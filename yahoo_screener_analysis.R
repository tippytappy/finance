# SET UP  #####################################################################
library(rvest)

#  GET THE DATA  ##############################################################
# yahoo's pre-defined screeners
screeners <- c(
  'aggressive_small_caps',
  'conservative_foreign_funds',
  'day_gainers',
  'day_losers',
  'growth_technology_stocks',  # 5
  'high-yield_bond',
  'portfolio_anchors',
  'small_cap_gainers',
  'solid_large_growth_funds',
  'solid_midcap_growth_funds',  # 10
  'top_mutual_funds',
  'undervalued_growth_stocks',
  'undervalued_large_caps')


get_screener_data <- function(screen_num = 12, num_records = 200, ...) {
  screener_url <- paste0('https://uk.finance.yahoo.com/screener/predefined/',
                            screeners[screen_num],
                         '?offset=0&count=',
                         num_records)
  cat(paste0('getting symbols from \n', screener_url))
  screener_symbols <- 
    read_html(screener_url) %>% 
    rvest::html_nodes('.simpTblRow') %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_text() %>%
    tq_get()
}

# example: get 30 solid mid-cap growth funds
screener_data <- get_screener_data(10, 30)
