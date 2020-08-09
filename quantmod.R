# FINANCIAL ANALYSIS USING QUANTMOD
library(quantmod)
library(dplyr)

# COMPANIES  ##################################################################
# some companies just for the sake of testing
companies <- c('AAPL', 'MSFT', 'AMZN', 'GOOG')

# GET DATA  ###################################################################
# getQuote returns various fundamental information
info <- c(
  'Market Capitalization',
  'Shares Outstanding',
  'Average Daily Volume',
  'Earnings/Share',
  'EPS Forward',
  'P/E Ratio',
  'Price/EPS Estimate Next Year',
  'Book Value',
  'Price/Book',
  'Dividend/Share',
  'Dividend Yield',
  'Ex-Dividend Date',
  '50-day Moving Average',
  'Percent Change From 50-day Moving Average 40',
  '200-day Moving Average',
  'Percent Change From 200-day Moving Average'
  )

qm_fundamentals <- getQuote(c(companies), what = yahooQF(info))
qm_fundamentals <- qm_fundamentals %>% 
  tibble::rownames_to_column('symbol')

# yahooQF() lists the data you can get using getQuote

# 1:   Symbol                                       2:   Name                                      
# 3:   Name (Long)                                  4:   Quote Type                                
# 5:   Quote Source Name                            6:   Source Interval                           
# 7:   Currency                                     8:   Financial Currency                        
# 9:   Market                                      10:   Market State                              
# 11:   Exchange                                    12:   Exchange Full Name                        
# 13:   Exchange Timezone                           14:   Exchange TZ                               
# 15:   Exchange Data Delay                         16:   GMT Offset Millis                         
# 17:   Tradeable                                   18:   Ask                                       
# 19:   Bid                                         20:   Ask Size                                  
# 21:   Bid Size                                    22:   Last Trade (Price Only)                   
# 23:   Last Trade Time                             24:   Change                                    
# 25:   Open                                        26:   Days High                                 
# 27:   Days Low                                    28:   Volume                                    
# 29:   Change in Percent                           30:   Previous Close                            
# 31:   Change From 52-week Low                     32:   Percent Change From 52-week Low           
# 33:   Change From 52-week High                    34:   Percent Change From 52-week High          
# 35:   52-week Low                                 36:   52-week High                              
# 37:   50-day Moving Average                       38:   Change From 50-day Moving Average         
# 39:   Percent Change From 50-day Moving Average   40:   200-day Moving Average                    
# 41:   Change From 200-day Moving Average          42:   Percent Change From 200-day Moving Average
# 43:   Market Capitalization                       44:   P/E Ratio                                 
# 45:   Price/EPS Estimate Next Year                46:   Price/Book                                
# 47:   Book Value                                  48:   Average Daily Volume                      
# 49:   Shares Outstanding                          50:   Ex-Dividend Date                          
# 51:   Dividend Pay Date                           52:   Dividend/Share                            
# 53:   Dividend Yield                              54:   Earnings Timestamp                        
# 55:   Earnings Start Time                         56:   Earnings End Time                         
# 57:   Earnings/Share                              58:   EPS Forward                               
# 59:   Language                                    60:   Message Board ID                          
# 61:   Price Hint   

# getSymbols returns OHLC data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
quantmod::getSymbols('ACIU')
chartSeries(ACIU)
add_SMA(n = 50)
add_SMA(n = 200)

# CHARTING  ###################################################################

candleChart(ACIU, time.scale = )