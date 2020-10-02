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
  xml2::read_html(screener_url) %>% 
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

# DATA FRAME TO XTS  ##########################################################
# convert data frame to xts
closes_tq <- ohlc %>% 
  filter(date > as.POSIXct('2019-01-01')) %>% 
  #filter(ticker == 'AAPL') %>% 
  select(date, symbol, close) %>% 
  tbl2xts::tbl_xts(., cols_to_xts = close, spread_by = symbol)

# ADVANCED CHART  #############################################################
chart_symbol <- function(sym, fast_n = 50, slow_n = 200) {
  if (!require(highcharter)) {
    
    stop("please load highcharter to use this function")
    
  } else {
    x <- eval(rlang::parse_expr(sym))
    highchart(type = "stock") %>% 
      hc_yAxis_multiples(create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)) %>% 
      hc_add_series(x, yAxis = 0, name = sym) %>% 
      hc_add_series(EMA(Cl(x), fast_n), yAxis = 0, name = "Fast EMA") %>% 
      hc_add_series(EMA(Cl(x), slow_n), yAxis = 0, name = "Slow EMA") %>% 
      hc_add_series(x[, 5], color = "gray", yAxis = 1, name = "Volume", type = "column") %>% 
      hc_add_series(RSI(Cl(x)), yAxis = 2, name = "Osciallator", 
                    color = hex_to_rgba("green", 0.7)) %>%
      hc_add_series(xts(rep(70, NROW(x)), index(x)), 
                    color = hex_to_rgba("red", 0.7), yAxis = 2, name = "Sell level") %>% 
      hc_add_series(xts(rep(30, NROW(x)), index(x)), 
                    color = hex_to_rgba("blue", 0.7), yAxis = 2, name = "Buy level") %>% 
      hc_tooltip(valueDecimals = 2)
  }
}

# GET ETF INFORMATION FROM YAHOO  #############################################
get_etf_info <- function(x) {
  cat(paste('getting', x, '\n'))
  u1 <- 'https://finance.yahoo.com/quote/'
  u2 <- '?p='
  u3 <- '&.tsrc=fin-srch'
  u <- paste0(u1, x, u2, x, u3)
  y <- u %>% 
    read_html() %>% 
    html_nodes("#quote-summary") %>% 
    html_nodes("table") %>% 
    html_table()
  y <- y[[2]][1:7, ]
  y <- y %>% 
    dplyr::mutate(X2 = sub('B|M|%|N/A', '', X2),
                  X2 = as.numeric(X2)) %>% 
    tidyr::spread(X1, X2)
  cbind(sym = x, y)
}

# takes a symbol as a string
# returns a data frame