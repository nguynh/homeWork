library(matrixcalc)

llr <- function(x,y,z,omega) {
  fits <- sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat <- function(z, x, y, omega) {
  W <- make_weight_matrix(z, x, omega)
  X <- make_predictor_matrix(x)
  y <- matrix(y)
  WX <- sweep(X, 1, W, FUN = "*")
  Wy <- W*y
  f_hat <- c(1,z) %*% solve(t(X) %*% WX) %*% t(X) %*% Wy
  return(f_hat)
}

make_weight_matrix <- function(z, x, omega) {
  r <- abs(x - z) / omega
  w <- ifelse(abs(r) < 1, (1-abs(r)^3)^3, 0)
  return(w)
}

make_predictor_matrix <- function(x) {
  X <- cbind(1, x)
  return(X)
}