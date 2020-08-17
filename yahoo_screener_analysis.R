# yahoo screener
https://uk.finance.yahoo.com/screener/predefined/undervalued_growth_stocks

yahoo_screener_symbols <- 
  read_html('https://uk.finance.yahoo.com/screener/predefined/undervalued_growth_stocks?offset=25&count=250') %>% 
  html_nodes('.simpTblRow') %>% 
  html_nodes("a") %>% 
  html_text()

yahoo_screener_data <- tq_get(yahoo_screener_symbols[1:5]) 
yahoo_screener_data %>% 
  ggplot(aes(date, close)) + 
  geom_barchart(aes(high = high, low = low, open = open, close = close)) + 
  facet_wrap(~ symbol, scales = 'free')
