delta <- function(x) {
      d2 <- (log(x/K) + (r - 0.5 * sigma^2) * (T - t))/(sigma *
                                                         sqrt(T - t))
      d1 <- d2 + sigma * sqrt(T - t)
      pnorm(d1)
       }
r <- 0.01
K <- 100
T <- 100
sigma <- 0.05
t <- 1
curve(delta, 0, 200, xlab = expression(S[t]),
         ylab = expression(delta = a^H(t)))
t <- 50
curve(delta, 0, 200, lty = 2, add = TRUE)
t <- 99.5
curve(delta, 0, 200, lty = 3, add = TRUE)
legend(150, 0.6, c("t=1", "t=50", "t=99.5"), lty = 1:3)