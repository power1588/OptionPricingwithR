#
#Using foreach package
#distribute the task
#
#
MCPrice <- function(x = 1, t = 0, T = 1, r = 1, sigma = 1,
                    M = 1000,
                     f) {
  require(foreach)
    h <- function(m) {
       u <- rnorm(m/2)
       tmp <- c(x * exp((r - 0.5 * sigma^2) * (T - t) + sigma *
                         + sqrt(T - t) * u), x * exp((r - 0.5 * sigma^2) * (T -
                                                                              + t) + sigma * sqrt(T - t) * (-u)))
       mean(sapply(tmp, function(xxx) f(xxx)))
     }
    nodes <- getDoParWorkers()
    p <- foreach(m = rep(M/nodes, nodes),.combine = "c") %dopar%
      h(m)
      p <- mean(p)
      p * exp(-r * (T - t))
}
ptm <- proc.time()
f <- function(x) max(0, x - 110)
set.seed(123)
M <- 150000
S0 <- 100
r <- 0.05
sigma <- 0.25
nodes <- 4
T <- 1/4
MCPrice(x = S0, t = 0, T = T, r = r, sigma, M = M, f = f)
proc.time() - ptm





