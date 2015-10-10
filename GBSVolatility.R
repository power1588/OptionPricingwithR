#Implied volatility and volatility smiles
#using package:fImport

require(fImport)
require(quantmod)
S <- yahooSeries("ATL.MI", from = "2004-07-23",
                    to = "2005-05-13")
Close <- S[, "ATL.MI.Close"]
chartSeries(Close, theme = "black")
X <- returns(Close)
Delta <- 1/252
sigma.hat <- sqrt(var(X)/Delta)[1, 1]
sigma.hat
S0 <- Close[1]
K <- 23
T <- 15 * Delta
r <- 0.02074
sigma.hat <- as.numeric(sigma.hat)
require(fOptions)
p0 <- GBSOption("c", S = S0, X = K, Time = T, r = r, b = r,
                   sigma = sigma.hat)@price
p <- 4e-04
sigma.imp <- GBSVolatility(p, "c", S = S0, X = K, Time = T,
                           r = r,
                           b = r)
sigma.imp
