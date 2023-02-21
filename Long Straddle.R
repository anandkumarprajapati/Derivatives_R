library(ggplot2)

prices <- seq(700,950,1) # Vector of prices
strike <- 850 # strike price for both put and call 
premium_call <- 20 # option price call
premium_put <- 10  # option price put 

# call option payoff at expiration 
intrinsicValuesCall <- prices - strike - premium_call
payoffLongCall <- pmax(-premium_call,intrinsicValuesCall)

# put option payoff at expiration
intrinsicValuesPut <- strike - prices - premium_put
payoffLongPut <- pmax(-premium_put,intrinsicValuesPut)

# The payoff of the Strategy is the sum of the call and put payoff. Need
# to sum wise element by element between the two vectors
payoff <- rowSums(cbind(payoffLongCall,payoffLongPut))

# Make a DataFrame with all the variable to plot it with ggplot
results <- data.frame(cbind(prices,payoffLongCall,payoffLongPut,payoff))

ggplot(results, aes(x=prices)) + 
  geom_line(aes(y = payoffLongCall, color = "LongCall")) + 
  geom_line(aes(y = payoffLongPut, color="LongPut"))+
  geom_line(aes(y=payoff, color = 'Payoff')) +
  scale_colour_manual("", 
                      breaks = c("LongCall", "LongPut", "Payoff"),
                      values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
  ggtitle("Long Straddle Payoff")  