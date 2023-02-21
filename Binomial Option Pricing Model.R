library(ggplot2)

build_stock_tree <- function(S, sigma, delta_t, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  U = exp(sigma*sqrt(delta_t))
  D = exp(-sigma*sqrt(delta_t))
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i, j] = S * U^(j-1) * D^((i-1)-(j-1))
    }  }
  return(tree)
}

firstExample <- build_stock_tree(S=80, sigma=0.1, delta_t=1/2, N=2)
secondExample <- build_stock_tree(S=80, sigma=0.1, delta_t=1/4, N=4)

K <- 85
IntrinsecValue <- secondExample[5,]- K
IntrinsecValue[IntrinsecValue<0] <- 0


q_prob <- function(r, delta_t, sigma) {
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  return((exp(r*delta_t) - d)/(u-d))
}


value_binomial_option <- function(tree, sigma, delta_t, r, X, type) {
  q = q_prob(r, delta_t, sigma)
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } 
  else {  option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i,j]=((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  return(option_tree)
}


binomial_option <- function(type, sigma, T, r, X, S, N) {
  q <- q_prob(r=r, delta_t=T/N, sigma=sigma)
  tree <- build_stock_tree(S=S, sigma=sigma, delta_t=T/N, N=N)
  option <- value_binomial_option(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
}


results <- binomial_option(type='call', sigma=0.15, T=1, r=0.1, X=100, S=110, N=5)

# Define an array with strike prices between 80 and 120

strikes <- seq(80, 120)

option_price_vary_function <- function(strike) {
  option = binomial_option(type='call', sigma=0.15, T=1, r=0, X=strike, S=110, N=5)
  return(option$price)
}

# Use the sapply command to pass each value of the strikes array into the option_price_vary_strike function:

values <- sapply(strikes, option_price_vary_function)


# Create a data frame with the Strikes prices and the option prices (values) from above

data <- as.data.frame(list(strikes=strikes, values=values)) 
head(data)

ggplot(data=data) + geom_line(aes(x=strikes, y=values)) + labs(title="Call Value", x="Strikes", y="Value") +   geom_vline(xintercept = 100, linetype="dotted",  color = "red", size=1.5)

