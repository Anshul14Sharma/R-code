A<-matrix(floor(rnorm(25,1,2)),4,4)
B<-c(floor(rnorm(4)))


luA<-optR(A, tol=1e-7, method="LU")

#Suppose that you have an LU - Decomposition of matrix A:
#A=LU,
L <- luA$L
U <- luA$U
print( L )
print( U )
#Where U is upper-triangle and L is lower-triangle. Then the original #system is:
#LUx=b.
#Let us break the task into two parts: first, we find yy such that
#Ly=b
y <- solve(L,B)
print( y )
#Then,find x such that
# Ux=y
x <- solve(U,y)
print( x )