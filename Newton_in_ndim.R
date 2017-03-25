library("pracma")
dummy <- function(x)
{
  z <- x[1]
  y <- x[2]
  rez <- exp(0.1 * ((y - z^2))^2 + 0.05*(1 - z)^2)
  #rez <- (z- y)^4+2*z^2+y^2-z+2*y
  rez
}
#grad(dummy , x=c(-0.3,0.8))
#hessian(dummy,x=c(-0.3,0.8))
n <- 0                    #initialize iteration counter 
eps <- 1                  #initialize error 
x <- c(1,1)           #set starting value

#Computation loop 
while (eps>1e-10 && n<100 )
{
  gradf <- grad(dummy , x )                                                      #gradf(x) 
  eps <- abs(gradf)+abs(gradf)                                                   #error 
  Hf <- hessian(dummy,x)
  k <- solve(Hf,-gradf) 
  y <- x + k
  x <- y                                                                         #update x 
  n <- n+1                                                                        #counter+1 
}
print(n)
print(x)
