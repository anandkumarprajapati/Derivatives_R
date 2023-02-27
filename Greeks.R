library(derivmkts)
library(optionstrat)

S <- 100
X <- 90
Y  <- 0.5
r <- .06
k <- .02
Sigma <- .35

d1 <- (log(S/X) + (r + Sigma^2/2 * Y))/(Sigma*sqrt(Y)) # As appears anywhere
d2 <- d1 - Sigma * sqrt(Y)

Nd1 <- pnorm(d1)
Nd2 <- pnorm(d2)

CallbyHand <- S * exp(-k*Y) * Nd1 - X * exp(-r*Y) * Nd2
PutbyHand <- -S * exp(-k*Y) * pnorm(-d1) + X * exp(-r*Y) * pnorm(-d2)

callprice1 <- bscall(S, X, Sigma, r, Y, k)
putprice1 <- bsput(S, X, Sigma, r, Y, k)



#Computing the Greeks by Brute Force:

Delta_Call_BF <- exp(-k*Y) * pnorm(d1)
Gamma_Call_BF <- exp(-d1^2/2 - k * Y)/(S * Sigma * sqrt(2*Y*pi))
Vega_Call_BF <- (S * sqrt(Y) * exp(-(d1^2)/(2 - k * Y)))/(sqrt(2 * pi))
Theta_Call_BF <- -S*exp(-(d1^2)/2-k*Y)*Sigma/sqrt(8*Y*pi)+k*S*exp(-k*Y)*Nd1-r*X*exp(-r*Y)*Nd2
Rho_Call_BF <- X*Y*exp(-r*Y) * pnorm(d2)

Delta_Put_BF <- -exp(k*Y) * pnorm(-d1)
Gamma_Put_Bf <- exp(-d1^2/2 - k * Y)/(S * Sigma * sqrt(2*Y*pi))
Vega_Put_BF <- (S * sqrt(Y) * exp(-(d1^2)/(2 - k * Y)))/(sqrt(2 * pi))
Theta_Put_BF <- -S*exp(-(d1^2)/2-k*Y)*Sigma/sqrt(8*Y*pi)-k*S*exp(-k*Y)*(1-Nd1)+r*X*exp(-r*Y)*(1-Nd2)
Rho_Put_BF <- -X*Y*exp(-r*Y) * pnorm(-d2)

Call_Greeks <- bsopt(S, X, Sigma, r, Y, k)[["Call"]][c("Delta", "Gamma", "Vega", "Theta", "Rho"),] 
Put_Greeks <- bsopt(S, X, Sigma, r, Y, k)[["Put"]][c("Delta", "Gamma", "Vega", "Theta", "Rho"), ]

Call_Delta_2 <- callgreek("delta",S, X, Sigma, Y, r, k)
Call_Gamma_2 <- callgreek("gamma",S, X, Sigma, Y, r, k)
Call_Theta_2 <- callgreek("theta",S, X, Sigma, Y, r, k)
Call_Vega_2 <- callgreek("vega",S, X, Sigma, Y, r, k)
Call_Rho_2 <- callgreek("rho",S, X, Sigma, Y, r, k)
Put_Delta_2 <- putgreek("delta",S, X, Sigma, Y, r, k)
Put_Gamma_2 <- putgreek("gamma",S, X, Sigma, Y, r, k)
Put_Theta_2 <- putgreek("theta",S, X, Sigma, Y, r, k)
Put_Vega_2 <- putgreek("vega",S, X, Sigma, Y, r, k)
Put_Rho_2 <- putgreek("rho",S, X, Sigma, Y, r, k)

