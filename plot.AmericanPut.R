plot.AmericanPut <- function(obj){
  plot(range(obj$t), range(obj$S), type = "n", axes = F, xlab = "t", ylab = "S")
  axis(1, obj$t, obj$t)
  axis(2, obj$S, obj$S)
  abline(v = obj$t, h = obj$S, col = "darkgray", lty = "dotted")
  for ( i in 0:obj$N) {
    for (j in 0:obj$M) {
      J <- obj$M + 1 - j
      I <- i+1
      cl <- "grey"
      if (obj$optTime[J, I])
        cl <- "black"
      text(obj$t[i+1], obj$S[j +1 ], round(obj$P[J, I], 2), cex = 0.75, col = cl)
        
        
    }
  }
  
  DS <- mean(obj$S[1:2])
  y <- as.numeric(apply(obj$optTime, 2, function(x) which(x)[1]))
  lines(obj$t, obj$S[obj$M + 2-y] + DS, lty= 2)
}