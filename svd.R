#Singular value decomposition 
A<-matrix(floor(rnorm(25,1,2)),5,5)
B<-c(floor(rnorm(5)))
asvd <- svd(A)
print(asvd)
adiag <- diag(1/asvd$d)
print(adiag)
adiag[3,3] = 0
solution = asvd$v %*% adiag %*% t(asvd$u) %*% B
print(solution)
check <- A %*% solution
print(check)