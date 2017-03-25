library("pracma")
dummy <- function(x)
{
  z <- x[1]
  y <- x[2]
  rez <- exp(0.1 * ((y - z^2))^2 + 0.05*(1 - z)^2)
  rez
}
x <- c(-0.3,0.8) 
a <- 0.09
B0 <- hessian(dummy, x)
while (eps>1e-10 && n<100 )
{
  grad1 <- grad(dummy,x)
  eps <- abs(grad1)+abs(grad1)                                                   
  Bk <- hessian(dummy,y)
  p <- solve(Bk, -grad1)
  s <- a * p
  y <-  x + s
  x <- y
  grad2 <- grad(dummy, y)
  yk <- grad2 - grad1
  ykt <- transpose(yk)
  Bk1 <- (Bk + (yk * ykt)/(ykt * s) - (Bk * s)* transpose((Bk * s))/(transpose(s) * Bk* s))
  n <- n+1
}
print(n)
print(y)