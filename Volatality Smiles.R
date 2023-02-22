# Get the two files that have the data for calls and puts at expiration date 02 August 2019. The source of these files is Yahoo Finance. https://finance.yahoo.com/quote/SPY/options

calls <- read.csv("AAPL_calls.csv", stringsAsFactors = FALSE)
puts <- read.csv("AAPL_puts.csv ",stringsAsFactors = FALSE)

head(calls)
head(puts)


calls$Implied.Volatility <- as.numeric(gsub('%','',calls$Implied.Volatility))
puts$Implied.Volatility <- as.numeric(gsub('%','',puts$Implied.Volatility))

calls$Open.Interest <- as.numeric(gsub(',','',calls$Open.Interest))
puts$Open.Interest <- as.numeric(gsub(',','',puts$Open.Interest))

# Total of 4 graphs showing Implied Volatility and Open Interest patterns among Strikes Prices.

S = 209.68

par(mfrow=c(2,2))

plot(calls$Strike, calls$Implied.Volatility, type="p", col="red", pch = 19,
     main=c("AAPL 02 Aug 2019 Calls", "Volatility Smile"), xlab="Strike", ylab="Implied Vol")

abline(v=S, col="red", lty=2)

plot(puts$Strike, puts$Implied.Volatility, type="p", col="blue",pch = 19,     main=c("AAPL 02 Aug 2019 Puts", "Volatility Smile"), xlab="Strike", ylab="Implied Vol")

abline(v=S, col="red", lty=2)

plot(calls$Strike, calls$Open.Interest, type="h",pch = 19, col='green',     main="Open Interest", xlab="Strike", ylab="Open Interest")

abline(v=S, col="red", lty=2)

plot(puts$Strike, puts$Open.Interest, type="h",pch = 19, col='green',     main="Open Interest", xlab="Strike", ylab="Open Interest")

abline(v=S, col="red", lty=2)




