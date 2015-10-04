#
#Using snowfall package to set up
#a cluster of two cpus
#and assign it to an object c1
#and start up a random number 
#generator for parallelized tasks

require(snowfall)
sfInit(parallel = TRUE, cpus = 2)
cl <- sfGetCluster()
clusterSetupRNG(cl, seed = rep(123, 2))
require(foreach)
require(doSNOW)
require(fOptions)
registerDoSNOW(cl)
getDoParWorkers()
M <- 50000
#MCPrice(x = S0, t = 0, T = T, r = r, sigma, M = M, f = f)
set.seed(123)
m <- c(10, 50, 100, 150, 200, 250, 500, 1000)
p1 <- NULL
err <- NULL
nM <- length(m)
repl <- 100
K <- 110
mat <- matrix(, repl, nM)
for (k in 1:nM) {
  tmp <- numeric(repl)
  for (i in 1:repl) tmp[i] <- MCPrice(x = S0, t = 0, T = T,
                                         r = r, sigma, M = m[k], f = f)
  mat[, k] <- tmp
  p1 <- c(p1, mean(tmp))
  err <- c(err, sd(tmp))
  }
colnames(mat) <- m
# tedious plot
p0 <- GBSOption(TypeFlag = "c", S = S0, X = K, Time = T, r = r,
                 b = r, sigma = sigma)@price
minP <- min(p1 - err)
maxP <- max(p1 + err)
plot(m, p1, type = "n", ylim = c(minP, maxP), axes = F,
     ylab = "MC price",xlab = "MC replications")
lines(m, p1 + err, col = "blue")
lines(m, p1 - err, col = "blue")
axis(2, p0, "B&S price")
axis(1, m)
boxplot(mat, add = TRUE, at = m, boxwex = 15, col = "orange",axes = F)
points(m, p1, col = "blue", lwd = 3, lty = 3)
abline(h = p0, lty = 2, col = "red", lwd = 3)
#close
sfStop()
registerDoSEQ()






