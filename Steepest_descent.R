library("pracma")
dummy <- function(x)
{
  z <- x[1]
  y <- x[2]
  rez <- exp(0.1 * ((y - z^2))^2 + 0.05*(1 - z)^2)
  rez
}
n <- 0            #initialize iteration counter 
eps <- 1          #initialize error 
a <- 0.09         #set iteration parameter 
x <- c(-0.3,0.8)        #set starting value

#Computation loop 
while (eps > 1e-10  && n<100)
{
gradf <- grad(dummy,x)  #gradf(x) 
eps <- abs(gradf)+abs(gradf)                                                  #error 
y <- x- a * gradf                                                                     #iterate 
x <- y                                                                              #update x 
n <- n+1                                                                            #counter+1 
} 
#display end values
print(n)
print(x)
print(eps)
