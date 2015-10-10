require(fAsianOptions)
r <- 0.03
b <- 0.02
S0 <- 2.212
K <- 2.3
T <- 13/240
t <- 0
sigma <- 0.25
p3 <- GemanYorAsianOption("c", S = S0, 
                          X = K, Time = T,  r = r,
                          sigma = sigma,
                          doprint = FALSE)$price
p3