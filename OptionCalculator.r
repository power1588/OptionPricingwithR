r <- 0.03
K <- 2.00
#unit as year
T <- 13/250
t <- 0
sigma <- 0.34
S0 <- 2.212

require(fOptions)
GBSCharacteristics(TypeFlag = "p", S = S0
                  , X = K, Time = T-t
                  , r = r, b=.02, sigma = sigma)$premium