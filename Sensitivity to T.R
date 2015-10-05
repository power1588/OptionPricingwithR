##############
#sensitivity to T
#
#
S0 <- 100
r <- 0.01
K <- 100
T <- 10
call.price <- function(x = 1, t = 0, T = 1, r = 1, sigma = 1,
                       K = 1) {
  d2 <- (log(x/K) + (r - 0.5 * sigma^2) * (T - t))/(sigma *
                                                      sqrt(T - t))
  d1 <- d2 + sigma * sqrt(T - t)
  x * pnorm(d1) - K * exp(-r * (T - t)) * pnorm(d2)
}
p <- function(sigma) call.price(x = S0, t = 0, T = T, r = r,K = K, sigma = sigma)                  

curve(p, 0, 1, xlab = expression(sigma),
         ylab = expression(C[t]),
          ylim = c(0, 100))
T <- 50
curve(p, 0, 1, add = TRUE, lty = 2)
T <- 100
curve(p, 0, 1, add = TRUE, lty = 3)
legend(0.5, 40, c("T=10", "T=50", "T=100"), lty = 1:3)