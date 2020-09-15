# SET UP  #####################################################################
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(quantmod)
library(ROI)
library(dplyr)
source('finance_functions.R')

# DATA  #######################################################################
# get daily OHLC data
getSymbols('SPY', from = '2010-01-01')

# convert to monthly
sp500_monthly <- SPY %>% 
  to.monthly()

# calculate returns for the closing price
sp500_returns <- sp500_monthly$..Close %>% 
  Return.calculate()

# we have to remove the first row since it will be NA
sp500_returns <- sp500_returns[-1, ]
names(sp500_returns) <- 'spy'

# we can visualise the returns easily
plot.zoo(sp500_returns)

# Treasury yield data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# we'll use this as the risk-free rate
t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", 
                    from = '2010-01-01', 
                    auto.assign = FALSE)

# Subset on dates
t10yr <- t10yr["2010-01-01/2020-09-30"]

# convert to monthly
t10yr_monthly <- t10yr %>% 
  to.monthly()

# calculate returns and remove the first row
t10yr_returns <- t10yr_monthly$..Close %>% 
  Return.calculate()
t10yr_returns <- t10yr_returns[-1, ]
names(t10yr_returns) <- 'rfr'

# SHARPE RATIO  ###############################################################
# to calculate the Sharpe ratio we'll need
# excess return compared to the risk-free rate
# geometric mean
# std deviation

# calculate the annualized risk free rate
# not sure why we do this
annualized_rf <- (1 + t10yr_returns )^12 - 1

# calculate the series of excess portfolio returns
sp500_excess <- sp500_returns - t10yr_returns

# Compute the Sharpe ratio
sp500_sharpe <- mean.geometric(sp500_excess) / sd(sp500_returns)

# ANNUALISED SHARPE RATIO  ####################################################
sp500_sharpe_annual <- Return.annualized(sp500_returns) / StdDev.annualized(sp500_returns)

# this does it all in one go
table.AnnualizedReturns(sp500_returns)

# ROLLING ANUALISED SHARPE RATIO  #############################################
# Due to the dynamic nature of stock markets and constantly changing environment, 
# its performance analysis should give more weights to recent observations 
# than on distant observations. 
# The standard approach of doing this is the use of rolling estimation samples.

# How to choose the length of rolling samples
# Short samples: recent but volatile, good for detecting recent changes
# Long samples: less noise, good for detecting underlying long-term trend

returns_ann = Return.annualized(sp500_returns)
sd_ann = StdDev.annualized(sp500_returns)
sharpe_ann = SharpeRatio.annualized(sp500_returns, Rf = t10yr_returns)

# Plotting the 12-month rolling annualized mean
chart.RollingPerformance(R = sp500_returns, width = 12, 
                         FUN = "Return.annualized")
abline(h = Return.annualized(sp500_returns))

# 12 month rolling volatility (sd)
chart.RollingPerformance(R = sp500_returns, width = 12, 
                         FUN = "StdDev.annualized", Rf = t10yr_returns)

# 12 month rolling Sharpe ratio
chart.RollingPerformance(sp500_returns, width = 12,
                         FUN = "SharpeRatio.annualized",
                         Rf = t10yr_returns)

# plot all three metrics at once
charts.RollingPerformance(R = sp500_returns, width = 12, Rf = t10yr_returns) # Note the charts instead of chart.

# COMPARING RETURNS FOR DIFFERENT PERIODS  ####################################
sp500_2018 <- SPY$SPY.Close %>% 
  window(start = '2018-01-01', end = '2018-12-31') %>% 
  Return.calculate()

sp500_2019 <- SPY$SPY.Close %>% 
  window(start = '2019-01-01', end = '2019-12-31') %>% 
  Return.calculate()

# Plotting settings
par(mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
names(sp500_2018) <- "sp500_2018"
names(sp500_2019) <- "sp500_2019"

chart.Histogram(sp500_2018, methods = c('add.density', 'add.normal'), breaks = 50)
chart.Histogram(sp500_2019, methods = c('add.density', 'add.normal'), breaks = 50)

# BALANCING RISK AND REWARD
# When we use the sd to measure risk we assume portfolio returns have a normal distribution.
# In reality, returns are skewed to the left with fatter tails: negative returns are more likely.

# So we need additional risk measures.
# - Semi-deviation
# - Value at risk
# - 5% expected short fall

# Additional measures
# - Skewness
# - kurtosis

# Measure of the worst case risk
# We should also check the portfolio’s drawdowns, or peak-to-trough decline in cumulative returns.
# because the metrics discussed above do not do a great job at 
# describing the worst case risk of buying at a peak, and selling at a trough.

# Skewness
# Zero: symmetric
# Negative: large negative returns occur more often
# Positive: large positive returns occur more often
# 
# Kurtosis
# Zero: normal distribution
# Greater than zero: large returns of both positive and negative occur more often

skewness(sp500_returns)  # -0.4256975
kurtosis(sp500_returns)  # 1.134432

# negative value of skewness and positive value of kurtosis indicates that 
# large negative returns are more likely than large positive returns.

# DOWNSIDE RISK MEASURES  #####################################################
# When the return distribution is asymmetric (skewed), investors use 
# additional risk measures that focus on describing the potential losses.
# 
#  - Semi-deviation: the variability of returns below the mean return
#  - Value-at-Risk (or VaR): the 5% quantile of the return distribution, meaning that a more negative return can only happen with a probability of 5%. For example, you might ask: “what is the largest loss I could potentially take within the next quarter, with 95% confidence?”
#  - Expected Shortfall: the average of the 5% (p = 0.05) or 2.5% (p = 0.025) most negative returns

SemiDeviation(sp500_returns)
VaR(sp500_returns, p = 0.05)
ES(sp500_returns, p = 0.05)

# DRAWDOWNS  ##################################################################
# Volatility, semi-deviation, value-at-risk, and expected shortfall 
# describe risk over 1 period. 
# These aren't good for describing the worst case risk of buying at a peak, and selling at a trough.
# This sort of risk can be quantified by analyzing the portfolio’s drawdowns, 
# or peak-to-trough decline in cumulative returns.

table.Drawdowns(sp500_monthly)  # not working
chart.Drawdown(sp500_monthly)

# PERFORMANCE DRIVERS  ########################################################
# Find out how individual (expected) returns, volatilities and correlations 
# interact to determine the total portfolio performance.

# Portfolio drivers:
# - The assets’ individual performance
# - The choice of portfolio weights: individuals can obtain the highest risk-adjusted return, as measured by the Sharpe ratio, by optimizing the choice of weight.
# - The correlation between different asset returns:
#     the lower the correlation, the more successful the portfolio tends to be at
#     partially offsetting large losses in one asset with only a minor loss, or even a gain in another asset.
#     When the correlation is 1, there is no diversification potential.
#     When the correlation is negative, if one asset return is above average, and the other is almost always below average.
#     When the correlation is 0, the asset returns are linearly independent of each other. 
#       Note that interdependency can still exist on a non-linear level even when the correlation is 0.

# ASSET CORRELATION  ##########################################################
# this can be
# - static
# - rolling

# STATIC  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cor(sp500_returns, t10yr_returns)
#     rfr
# spy 0.455188

# scatter plot
chart.Scatter(sp500_returns, t10yr_returns)

# chart
merge(sp500_returns, t10yr_returns) %>% 
  chart.Correlation()

# ROLLING  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chart.RollingCorrelation(sp500_returns, t10yr_returns)


# RISK REWARD SCATTER PLOT
# Visualize relative attractiveness of the investments using a scatterplot of 
# the average returns against the portfolio volatilities.

returns <- merge(t10yr_returns, sp500_returns)
plot(apply(returns, 2, sd), apply(returns, 2, mean))
# find a better way to do this chart

# COVARIANCE MATRIX
# make some new data with assets similar to what we'd find in a portfolio
assets <- indexes[, 1:4]

# covariance matrix
asset_cov <- cov(assets)

# correlation
asset_cor <- cor(assets)

# ASSET RISK CONTRIBUTION  ####################################################
# Construct a risk budget to show each asset’s percent risk contribution 
# is in the total portfolio volatility. 
# This is to avoid the portfolio risk being concentrated on a few assets.

# Create portfolio weights
weights <- c(0.4, 0.4, 0.1, 0.1)

# Create volatility budget
vol_budget <- StdDev(assets, portfolio_method = "component", weights = weights)

# Make a table of weights and risk contribution
weights_percrisk <- cbind(weights, vol_budget$pct_contrib_StdDev)
colnames(weights_percrisk) <- c("weights", "perc vol contrib")

# OPTIMISING A PORTFOLIO  #####################################################
# So far portfolio weights have been fixed. 
# Now we will determine the optimal portfolio weights to achieve 
# a target return with minimum variance, while satisfying portfolio weights constraints.

# for this exercise we'll get OHLC data for multiple companies
# that we might want in our portfolio.
# Then we'll use a custom function to calculate the monthly returns.

# MAKE RETURNS DATA  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get the S&P 500 companies list from wikipedia
sp500 <- 
  read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% 
  html_nodes('#constituents') %>% 
  html_table()
sp500 <- sp500[[1]] %>% 
  select(symbol = Symbol, security = Security, founded = Founded,
         sector = `GICS Sector`, industry = 'GICS Sub Industry')

# pf = portfolio
pf <- sp500 %>% 
  mutate(founded = as.numeric(founded)) %>% 
  filter(founded < 2000) %>% 
  pull(symbol) %>% 
  head(10)

# get the OHLC data
getSymbols(pf, from = '2010-01-01')

# create a function which can take a vector of XTS OHLC object names and
# return an XTS object containing periodic returns for all companies.

periodReturns <- function(symbols, period = 'monthly') {
  len <- length(symbols)
  li <- vector(mode = 'list', length = len)
  for (i in 1:len) {
    x <- eval(rlang::parse_expr(symbols[i]))
    x <- periodReturn(x, period = period)
    names(x) <- symbols[i]
    li[[i]] <- x
  }
  Reduce(merge, li)
}

# use the new function to get the monthly returns of our test data
# monthly is the default period
pf_returns <- pf %>% 
  periodReturns()
pf_returns %>% head()

# plot the mean monthly return of the portfolio
pf_returns %>% rowMeans() %>% xts(order.by = time(pf_returns)) %>% plot.zoo()


# FIND THE MEAN-VARIANCE EFFICIENT PORTFOLIO  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A mean-variance efficient portfolio finds the weights that deliver the 
# lowest portfolio variance for a target return. 
# We can use portfolio.optim() function in the tseries package to find the weights.

library(tseries)
pf_optimised <- portfolio.optim(pf_returns)

# returns a list with 4 elements:
# $pw: weights of each asset within the portfolio,
# $px: portfolio return per each period,
# $pm: expected portfolio return (= target return) for the whole period,
# $ps: the standard deviation of the portfolio returns.

# display the weights
pf_weights <- pf_optimised$pw
names(pf_weights) <- colnames(pf_returns)
barplot(pf_weights)

# portfolio return for the whole period
pf_optimised$pm

# portfolio volatility
pf_optimised$ps

# CHANGING THE RETURN TARGET  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create a portfolio with a target return of average returns
pf_mean <- portfolio.optim(pf_returns, pm = mean(pf_returns))

# create a portfolio with a target return 10% higher than average returns
pf_10pcnt <- portfolio.optim(pf_returns, pm = 1.1 * mean(pf_returns))

# compare the sd of each
pf_mean$ps    # 0.04498795
pf_10pcnt$ps  # 0.04682246

(pf_10pcnt$ps - pf_mean$ps) / pf_mean$ps  # 0.04077773; volatility went up 4%

# IMPOSING WEIGHT CONSTRAINTS  ################################################
# Using weight constraints can prevent concentration risk,
# i.e. only a few assets making up a large percentage of the portfolio.
# Note - this can cause the portfolio to be more volatile!

# example: no asset can make up more than 20% of the portfolio by itself
pf_weight_constraint_20 <- portfolio.optim(pf_returns, 
                                           reshigh = rep(0.2, length(pf)))

# compare the weight-constrained portfolio / original portfolio weights
# slope plot of original vs constrained weights.
# for successive constraints we could use
# newggslopegraph() to show each set of weights
data.frame(symbol = pf, original_weights = pf_optimised$pw, 
           constrained_weights = pf_weight_constraint_20$pw) %>% 
  tidyr::gather(portfolio, weights, -symbol) %>% 
  mutate(weights = round(weights, 1),
         xpos = rep(1:2, each = 10)) %>% 
  ggplot(aes(xpos, weights)) + 
  geom_line() +
  geom_point() + 
  facet_wrap(~ symbol) + 
  scale_x_continuous(labels = c('original', 'constrained'), breaks = 1:2)

# THE EFFICIENT FRONTIER  #####################################################
# The efficient frontier finds the lowest variance portfolio
# for a vector of target returns.
# to create the vector we set
# MAX as the highest average asset return
# MIN as the return of the minimum variance portfolio
pf_targets <- seq(from = portfolio.optim(pf_returns)$pm, 
    to = max(pf_returns), 
    length.out = 50)

pf_targets <- seq(from = 0.01, 
                  to = 0.08, 
                  length.out = 50)

library(purrr)
pf_targets_results <- map(pf_targets, ~ portfolio.optim(pf_returns, pm = .x))

test_l <- 20
test <- vector(mode = 'list', length = test_l)
for(i in 1:test_l) {
  test[[i]] <- portfolio.optim(x = pf_returns, pm = pf_targets[i])
}


# PORTFOLIO OPTIMISATION WITH PORTFOLIOANALYTICS  #############################
library(PortfolioAnalytics)

pf_portspec <- portfolio.spec(assets = colnames(pf_returns))

pf_portspec <- pf_portspec %>% add.constraint(type = 'weight_sum',
                                              min_sum = 1, max_sum = 1)

pf_portspec <- pf_portspec %>% add.objective(type = 'return', name = 'mean')

pf_portspec_opt <- optimize.portfolio(R = pf_returns, portfolio = pf_portspec, optimize_method = 'ROI')