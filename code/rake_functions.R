
NewPixelVal <- function(K, x) {
  x <- pmin(pmax(x, 0.001), 0.999)
  logit_x <- log(x / (1 - x))
  adjusted_logit <- logit_x + K
  return(1 / (1 + exp(-adjusted_logit)))
}
NewEst <- function(vals, weights) {
  w <- weights / sum(weights)
  weighted_averages <- crossprod(w, vals)
  mean(weighted_averages)
}
EvalDiff <- function(gbdval, vals, weights) {
  est <- NewEst(vals, weights)
  return(gbdval - est)
}
SimpleFindK <- function(gbdval, vals, weights, MaxJump = 10, NumIter = 40, FunTol = 1e-5) {
  CurrentError <- EvalDiff(gbdval, vals, weights)
  if (CurrentError > 0) {
    a <- 0; b <- MaxJump
  } else {
    a <- -MaxJump; b <- 0
  }
  
  for (i in 1:NumIter) {
    c <- (a + b) / 2
    F_c <- EvalDiff(gbdval, apply(vals, 2, function(x) NewPixelVal(c, x)), weights)
    if (abs(F_c) < FunTol) break
    if (F_c > 0) {
      a <- c
    } else {
      b <- c
    }
  }
  
  return((a + b) / 2)
}
adjust_matrix <- function(K, vals) {
  adjusted_vals <- NewPixelVal(K, vals)
  return(adjusted_vals)
}