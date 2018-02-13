A<-matrix(c(2,0,1,3), ncol=2)
B<-matrix(c(5,2,4,-1), ncol=2)
A+B
A-B

C<-diag(c(4,1,2,3))
C

D<-matrix(0, ncol=5, nrow=5)
D[1,]=1
D[,1]=2
diag(D)<-3
D
