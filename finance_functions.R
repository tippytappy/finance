# GET SYMBOLS FROM YAHOO SCREENERS  ###########################################
get_screener_symbols <- function(screen_num = 12, num_records = 200, ...) {
  screeners <- c(
    'aggressive_small_caps',
    'conservative_foreign_funds',
    'day_gainers',
    'day_losers',
    'growth_technology_stocks',  # 5
    'high_yield_bond',
    'portfolio_anchors',
    'small_cap_gainers',
    'solid_large_growth_funds',
    'solid_midcap_growth_funds',  # 10
    'top_mutual_funds',
    'undervalued_growth_stocks',
    'undervalued_large_caps')
  screener_url <- paste0('https://uk.finance.yahoo.com/screener/predefined/',
                         screeners[screen_num],
                         '?offset=0&count=',
                         num_records)
  cat(paste0('getting symbols from \n', screener_url))
  read_html(screener_url) %>% 
    rvest::html_nodes('.simpTblRow') %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_text()
}
# MERGE MULTIPLE XTS  #########################################################
merge_multiple_xts <- function(symbols, col = 'close') {
  len <- length(symbols)
  li <- vector(mode = 'list', length = len)
  col <- tolower(col)
  for (i in 1:len) {
    x <- eval(rlang::parse_expr(symbols[i]))
    x <- x[j = switch(col, open = 1, high = 2, low = 3, close = 4,
                      volume = 5, adjusted = 6)]
    names(x) <- symbols[i]
    li[[i]] <- x
  }
  Reduce(merge, li)
}

# returns an xts object of the required column type, e.g. 'close'
# from the xts objects in symbols
# symbols   a character vector
# col       a string matching the type of column required

# RETURNS FROM MUTLIPLE XTS  ##################################################
get_returns <- function(symbols, period = 'monthly') {
  len <- length(symbols)
  li <- vector(mode = 'list', length = len)
  for (i in 1:len) {
    x <- eval(rlang::parse_expr(symbols[i]))
    x <- quantmod::periodReturn(x, period = period)
    names(x) <- symbols[i]
    li[[i]] <- x
  }
  Reduce(merge, li)
}

# returns an xts object of returns for multiple symbols
# symbols   a character vector of symbols*
# period    'monthly', 'weekly' etc. as per periodReturn
# * the symbols data needs to already be downloaded

# XTS TO LONG FORMAT  #########################################################
pivot_xts <- function(x, colname) {
  data.frame(date = index(x), coredata(x)) %>%
    tidyr::gather(symbol, {{colname}}, -date)
}

# makes a long data frame from an xts object
# x       an xts object, e.g. OHLC or returns for multiple companies
# colname what to call the values column, e.g. 'returns'

# LINEAR MODEL COEFFICIENTS  ##################################################
get_coeffs <- function(x, id = 'symbol', xcol = 'date', ycol = 'return') {
  df <- x %>% 
    tidyr::nest(data = -id) %>% 
    dplyr::mutate(model = purrr::map(data, ~ lm(.x[[ycol]] ~ .x[[xcol]])),
                  coeff = purrr::map(model, ~ coefficients(.x)[[2]]))
  df[1] %>% 
    bind_cols(coeff = unlist(df[4]))
}

# takes a data frame
# returns a data frame of 
# - the id col, e.g. symbol
# - the coefficient of the required column, e.g. 'close'