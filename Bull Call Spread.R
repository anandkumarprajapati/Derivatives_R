library(ggplot2)


prices <- seq(400, 550,1) # Vector of prices
k_low = 450  # Low Strike price for call
k_high = 500 # High Strike Price for call 

premium_low = 10
premium_high = 1

# Intrinsic Value and Payoff long call
intrinsicValueLongCall <- prices - k_low - premium_low
payoffLongCall <- pmax(-premium_low,intrinsicValueLongCall)

# Intrinsic Value and Payoff short call
intrinsicValueShortCall <- prices - k_high - premium_high
payoffShortCall <- pmin(premium_high,-intrinsicValueShortCall)

# Strategy Payoff
payoff <- rowSums(cbind(payoffLongCall,payoffShortCall))

# Generate a dataframe with the payoffLongCall, payoffShortCall and payoff vectors
# in order to plot the strategy payoffs using ggplot
results <- data.frame(cbind(payoffLongCall,payoffShortCall,payoff))

ggplot(results, aes(x=prices)) + 
  geom_line(aes(y = payoffLongCall, color = "LongCall")) + 
  geom_line(aes(y = payoffShortCall, color="ShortCall"))+
  geom_line(aes(y=payoff, color = 'Payoff')) +
  ggtitle("Bull Call Spread Payoff")  