AmericanPutImp <- function(Smin = 0, Smax, T = 1, N = 10, M = 10, K , r = 0.05,
                          sigma = 0.01) {
  Dt = T/N
  DS = (Smax - Smin)/M
  t <- seq(0, T, by = Dt)
  S <- seq(Smin, Smax, by = DS)
  A <- function(j) (0.5*r*j *Dt - 0.5 * sigma^2*j^2*Dt)
  B <- function(j) (1 + sigma^2 * j ^2 * Dt + r*Dt)
  C <- function(j) (-0.5* r * j *Dt - 0.5 *sigma^2*j^2*Dt)
  a <- sapply(0:M, A)
  b <- sapply(0:M, B)
  c <- sapply(0:M, C)
  P <- matrix(, M+1, N+1)
  colnames(P) <- round(t,2)
  rownames(P) <- round(rev(S), 2)
  P[M+1, ] <- K
  P[1,] <- 0
  P[, N+1] <- sapply(rev(S), function(x) max(K - x, 0))
  AA <- matrix(0, M-1, M-1)
  for(j in 1:(M-1)) {
    if (j >1 )
      AA[j, j -1] <- A(j)
    if (j < M)
      AA[j, j] <- B(j)
    if (j < M- 1)
      AA[j, j +1] <- C(j)
  }
  optTime <- matrix(FALSE, M + 1, N + 1)
  for( i in (N-1):0){
    I <- i +1 
    bb <- P[M:2, I + 1]
    bb[1] <- bb[1] - A(1) * P[M+1-0, I +1]
    bb[M-1] <- bb[M-1] - C(M-1) * P[M+1-M, I+1]
    P[M:2, I] <- solve(AA,bb)
    idx <- which(P[,I] < P[, N+1])
    P[idx, I ] <- P[idx, N+1]
    optTime[idx, I] <- TRUE
    
  }
  optTime[M+1, ] <- TRUE
  optTime[which(P[, N+1] > 0 ), N+1] <- TRUE
  colnames(optTime) <- colnames(P)
  rownames(optTime) <- rownames(P)
  ans <- list(P = P, t = t, S = S, optTime = optTime, N = N, M = M)
  class(ans) <- "AmericanPut"
  return(invisible(ans))
  
}