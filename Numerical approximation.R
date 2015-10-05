###########
##
#numerical approximation
#
#
r <- 0.01
K <- 100
T <- 100
sigma <- 0.05
t <- 10
St <- 70
h <- 0.01
call.price <- function(x = 1, t = 0, T = 1, r = 1, sigma = 1,
                       K = 1) {
  d2 <- (log(x/K) + (r - 0.5 * sigma^2) * (T - t))/(sigma *
                                                      sqrt(T - t))
  d1 <- d2 + sigma * sqrt(T - t)
  x * pnorm(d1) - K * exp(-r * (T - t)) * pnorm(d2)
}
delta.num <- function(x)(call.price(x = x + h, t = t, T = T,
                                      sigma = sigma, r = r, K = K) - call.price(x = x, t = t,
                                      T = T,sigma = sigma, r = r, K = K))/h
delta(St)
delta.num2 <- function(x) (call.price(x = x + h, t = t, T = T,
      sigma = sigma, r = r, K = K) - call.price(x = x - h, t = t,
      T = T, sigma = sigma, r = r, K = K))/(2 * h)
delta.num2(St)


