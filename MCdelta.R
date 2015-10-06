library(foreach)
#x <- underlying t<- start time T<- expiry time r<- interest rate
# f <- payoff fuction
MCdelta <- function(x = 1, t = 0, T = 1, r = 1, sigma = 1,
                    M = 100000,
                    f) {
  # vector in computation of pricing
    h <- function(m) {
    u <- rnorm(M/2)
    tmp <- c(x * exp((r - 0.5 * sigma^2) * (T - t) + sigma *
            sqrt(T - t) * u), x * exp((r - 0.5 * sigma^2) * (T - t) + sigma * sqrt(T - t) * (-u)))
    #payoff function f(z)
    g <- function(z) f(z) * (log(z/x) - (r - 0.5 * sigma^2) *
                                  (T - t))/(x * sigma^2 * (T - t))
  # apply tmp to function(z) g(z) (this function(z) g(z) as a whole)
    mean(sapply(tmp, function(z) g(z)))
    }
  #  
    nodes <- getDoParWorkers()
    p <- foreach(m = rep(M/nodes, nodes), .combine = "c")%dopar%
    h(m)
    p <- mean(p)
    p * exp(-r * (T - t))
}
ptm <- proc.time()
r <- 0.01
K <- 100
T <- 100
t <- 10
M <- 1000000
set.seed(123)
nodes <- 3
sigma <- 0.05
S0 <- 70
#payoff function 
f <- function(x) max(0, x - 100)
delta(S0)
MCdelta(x = S0, t = 0, T = T, r = r, sigma, M = M, f = f)
# count on time of the procedure 
proc.time() - ptm



