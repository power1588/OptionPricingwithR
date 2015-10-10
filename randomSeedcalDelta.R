r <- 0.01
K <- 100
T <- 100
t <- 0
sigma <- 0.05
S0 <- 70
delta(S0)

source("MCPrice.R")
h <- 0.001
set.seed(123)
f <- function(x) max(0, x - 100)
p1 <- MCPrice(x = S0 + h, t = 0, T = T, r = r, sigma, M = M,f=f)
set.seed(123)
p1
p2 <- MCPrice(x = S0 - h, t = 0, T = T, r = r, sigma, M = M,f=f)
p2
(p1 - p2)/(2*h)
delta(S0) - (p1 - p2)/(2*h)