######################
#Volatility smiles
#volatility changes for given T but
#different strike K in a nonlinear way
#Apple, Inc. expiry:2009 July 17
#Data collected: 23 April 2009 T=60days
#S0 = 123.9
###########################
S <- yahooSeries("AAPL", from = "2009-01-02", to = "2009-04-23")
Close <- S[, "AAPL.Close"]
X <- returns(Close)
Delta <- 1/252
sigma.hat <- sqrt(var(X)/Delta)
sigma.hat

Pt <- c(22.2, 18.4, 15.02, 11.9, 9.2, 7, 5.2, 3.6, 2.62, 1.76,
          1.28, 0.8, 0.53, 0.34, 0.23, 0.15, 0.09, 0.1)
K <- c(105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155,
            160, 165, 170, 175, 180, 185, 190)
S0 <- 123.9
nP <- length(Pt)
T <- 60 * Delta
r <- 0.056
smile <- sapply(1:nP, function(i) GBSVolatility(Pt[i], "c",
                                                   S = S0,
                                                   X = K[i], Time = T, r = r, b = r))
vals <- c(smile, sigma.hat)
plot(K, smile, type = "l", ylim = c(min(vals, na.rm = TRUE),
                                         max(vals, na.rm = TRUE)), main = "")
abline(v = S0, lty = 3, col = "blue")
#abline(h = sigma.hat, lty = 3, col = "red")
#axis(2, sigma.hat, expression(hat(sigma)), col = "red")
