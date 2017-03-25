A<-matrix(floor(rnorm(25,1,2)),5,5)
B<-c(floor(rnorm(5)))
print(A)

x <- t(A)%*%A   #Multiply transpose(A) and A 
y <- t(A)%*%B   #Multiply transpose(A) and B 
#Cholesky Factorization transpose(A) * A
cholesky <- function (A, tol = 1e-07) 
{
  nROW <- ncol(A)
  L <- matrix(rep(0, each = nROW * nROW), nrow = nROW, byrow = T)
  for (i in 1:nROW) {
    Aii <- A[i, i] - sum(L[i, 1:i] * L[i, 1:i])
    if (Aii < 0) {
      stop("Matrix no positive definate")
    }
    else {
      L[i, i] <- sqrt(Aii)
    }
    if ((i + 1) <= nROW) {
      for (k in (i + 1):nROW) {
        L[k, i] <- (A[k, i] - sum(L[k, 1:i] * L[i, 1:i]))/L[i, i]
      }
    }
  }
  return(L)
}
c <- cholesky(x)
print(x)
print(y)
k <- print(t(c)) 
z <- solve(k,y)  # kZ = y
print(z)          
l <- solve(c,z)  # t(x)l =z
print(l)         #final Answer Ax=b 