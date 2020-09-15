long_xts <- function(x, colname) {
  data.frame(date = index(x), coredata(x)) %>%
    tidyr::gather(symbol, {{colname}}, -date)
}

# makes a long data frame from an xts object
# x       an xts object, e.g. OHLC or returns for multiple companies
# colname what to call the values column, e.g. 'returns'

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

period_returns <- function(symbols, period = 'monthly') {
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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