MCAsian <- function(S0 = 100, K = 100, t = 0, T = 1, mu = 0.1,
                    sigma = 0.1, r = 0.1, N = 100, M = 1000) {
   require(foreach)
   h <- function(x) {
      require(sde)
      z <- colMeans(sde.sim(X0 = S0, model = "BS", theta = c(mu,
                                                               sigma), M = x, N = N))
      f <- function(x) max(x - K, 0)
      p0 <- mean(sapply(z, f))
      }
    nodes <- getDoParWorkers()
    p <- foreach(m = rep(M/nodes, nodes),.combine = "c") %dopar%
      h(m)
    p <- mean(p)
    p * exp(-r * (T - t))
}

#using if to annotate the code block
#which test the MCAsian function
if(1<0){
  M <- 5000
  mu <- 0.1
  s <- 0.5
  K <- 110
  r <- 0.01
  T <- 1
  S0 <- 100
  set.seed(123)
  p0 <- MCAsian(S0 = S0, K = K, t = 0, T = T, mu = mu, sigma = s,
                r = r, N = 250, M = M)
  p0
}
