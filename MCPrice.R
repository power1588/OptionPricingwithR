MCPrice <- function(x = 1, t = 0, T = 1, r = 1, sigma = 0.05,
                    M = 1000, f){
  require(foreach)
  h <- function(m) {
    u <- rnorm(M/2)
    tmp <- c(x * exp((r - 0.5 * sigma^2) * (T - t) + sigma *
                       sqrt(T - t) * u), x * exp((r - 0.5 * sigma^2) * (T - t) + sigma * sqrt(T - t) * (-u)))
    #payoff function f(z) to calculate delta
    #g <- function(z) f(z) * (log(z/x) - (r - 0.5 * sigma^2) *
    #                           (T - t))/(x * sigma^2 * (T - t))
    # apply tmp to function(z) g(z) (this function(z) g(z) as a whole)
    mean(sapply(tmp, function(z) f(z)))
  }
  #  
  nodes <- getDoParWorkers()
  p <- foreach(m = rep(M/nodes, nodes), .combine = "c")%dopar%
  h(m)
  p <- mean(p)
  p * exp(-r * (T - t))
}