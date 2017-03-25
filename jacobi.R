f <- 2 * x + 3 * y + 4 *z 
jacobi <- function (f, x) 
{
  h <- 1e-04
  n <- length(x)
  jac <- matrix(0, n, n)
  f0 <- f(x)
  for (i in 1:n) {
    temp <- x[i]
    x[i] <- temp + h
    f1 <- f(x)
    x[i] <- temp
    jac[, i] <- (f1 - f0)/h
  }
  return(list(jacobianMatrix = jac, f0 = f0))
}
