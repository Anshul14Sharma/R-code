library("neldermead")
banana <- function(x){
  z <- x[1]
  y <- x[2]
  rez <- exp(0.1 * ((y - z^2))^2 + 0.05*(1 - z)^2)
  rez
}
opt <- optimset(MaxIter=10)
sol <- fminsearch(banana, c(0,-2), opt)
sol