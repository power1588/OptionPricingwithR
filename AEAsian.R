AEAsian <- function(S0 = 100, K = 100, t = 0, T = 1, mu = 0.1,
                      e = 0.1, r = 0.1, N = 1000) {
   require(yuima)
   diff.matrix <- matrix(c("x*e"), 1, 1)
    model <- setModel(drift = c("mu*x"), diffusion = diff.matrix)
    xinit <- S0
    f <- list(expression(x/T), expression(0))
    F <- 0
    yuima <- setYuima(model = model, sampling =
                        setSampling(Terminal = T,
                        n = 1000))
    yuima <- setFunctional(yuima, f = f, F = F, xinit = xinit,
                             e = e)
    F0 <- F0(yuima)
    rho <- expression(0)
    get_ge <- function(x, epsilon, K, F0) {
      tmp <- (F0 - K)+(epsilon * x)
      tmp[(epsilon * x)<(K - F0)] <- 0
      return(tmp)
      }
    epsilon <- e
    g <- function(x) {
      tmp <- (F0 - K) + (epsilon * x)
      tmp[(epsilon * x) < (K - F0)] <- 0
      tmp
      }
    asymp <- asymptotic_term(yuima, block = 10, rho, g)
    exp(-r * T) * (asymp$d0+e * asymp$d1)
}


