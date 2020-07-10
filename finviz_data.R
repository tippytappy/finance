# GET DATA FROM FINVIZ
# create ways to get data for multiple stock symbols from finviz

# SET UP  #####################################################################
library(rvest)
library(dplyr)
library(magrittr)

# GET THE FUNDAMENTALS TABLE  #################################################
# use microsoft as a test

# create a function to pull fundamentals from finviz
get_finviz <- function(x) {
  finviz_url <- paste0('https://finviz.com/quote.ashx?t=', x)
  fvt <- read_html(finviz_url) %>% 
    html_nodes('table') %>% 
    extract2(9) %>% 
    html_table(fill = TRUE)
  fdf <- data.frame()
  fdf[1, 1:73] <- c(x, fvt[[2]], fvt[[4]], fvt[[6]], fvt[[8]], fvt[[10]], fvt[[12]])
  names(fdf) <- c('Symbol', fvt[[1]], fvt[[3]], fvt[[5]], fvt[[7]], fvt[[9]], fvt[[11]])
  fdf
}

library(purrr)
fv_data <- map_df(c('JD', 'AAPL'), ~ get_finviz(.x))
