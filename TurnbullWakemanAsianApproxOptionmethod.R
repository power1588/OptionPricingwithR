require(fExoticOptions)
r <- 0.03
b <- 0.02
S0 <- 2.212
K <- 2.3
T <- 13/240
t <- 0
sigma <- 0.25
p2 <- TurnbullWakemanAsianApproxOption("c", S = S0, SA = S0,
                                       X = K, Time = T, time = T, tau = 0, r = r, b = r,
                                       sigma = sigma)@price
p2
