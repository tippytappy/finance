# GET DATA FROM FINVIZ
# create ways to get data for multiple stock symbols from finviz

# SET UP  #####################################################################
library(rvest)
library(dplyr)
library(magrittr)

# GET THE FUNDAMENTALS TABLE  #################################################
# a function to pull fundamentals from finviz
get_finviz <- function(x) {
  finviz_url <- paste0('https://finviz.com/quote.ashx?t=', x)
  fvt <- read_html(finviz_url) %>% 
    html_nodes('table') %>% 
    magrittr::extract2(9) %>% 
    html_table(fill = TRUE)
  fdf <- data.frame()
  fdf[1, 1:73] <- c(x, fvt[[2]], fvt[[4]], fvt[[6]], fvt[[8]], fvt[[10]], fvt[[12]])
  names(fdf) <- c('Symbol', fvt[[1]], fvt[[3]], fvt[[5]], fvt[[7]], fvt[[9]], fvt[[11]])
  fdf
}

# map the function to get fundamentals for multiple companies
library(purrr)
fv_data <- map_df(c('AAPL', 'MSFT', 'AMZN', 'GOOG'), ~ get_finviz(.x))

# GET THE INSIDER TRADING TABLE ###############################################
fv_url <- 'https://finviz.com/quote.ashx?t=msft'
fv_data <- read_html(fv_url)
fv_insider <- fv_data %>% 
  html_nodes('table') %>% 
  extract2(34) %>% 
  html_table(fill = TRUE)

# GET THE NEWS TABLE ###############################################
fv_url <- 'https://finviz.com/quote.ashx?t=msft'
fv_data <- read_html(fv_url)
fv_news <- fv_data %>% 
  html_nodes('table') %>% 
  extract2(33) %>% 
  html_table(fill = TRUE)
