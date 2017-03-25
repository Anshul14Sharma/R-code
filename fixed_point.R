fixedpoint <- function(fun, x0, tol=1e-07, niter=500){
  ## fixed-point algorithm to find x such that fun(x) == x
  ## assume that fun is a function of a single variable
  ## x0 is the initial guess at the fixed point
  
  xold <- x0
  xnew <- fun(xold)
  for (i in 1:niter) {
    xold <- xnew
    xnew <- fun(xold)
    if ( abs((xnew-xold)) < tol )
      return(xnew)
  }
  stop("exceeded allowed number of iterations")
}
 f <- function(x) x - x^7/5 + 1/5
 
 gfun <- function(x) -1/5 /(x - x ^7/5)
 