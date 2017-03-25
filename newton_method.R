newton<-function (f, fp, x, tol = 0.001, m = 100) 
{
  iter <- 0
  oldx <- x
  x <- oldx + 10 * tol
  while (abs(x - oldx) > tol) {
    iter <- iter + 1
    if (iter > m) 
      stop("No solution found")
    oldx <- x
    x <- x - f(x)/fp(x)
  }
  return(x)
}
par(mfrow=c(2,2))
curve(1 - log(x) * exp(-x^2),0.8,2)
f <- function(x) {1 - log(x) * exp(-x^2)}
fp <- function(x) {log(x) * exp(-x^2) * 2 * x - exp(-x^2) * 1/x}
newton(f,fp,0.95)