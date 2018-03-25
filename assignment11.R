tukey_multiple <- function(x) { 
  outliers <- array(TRUE,dim=dim(x)) 
  for (j in 1:ncol(x)) 
  { 
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j]) 
  } 
  outlier.vec <- vector(length=nrow(x)) 
  for (i in 1:nrow(x)) 
  {
    outlier.vec[i] <- all(outliers[i,])
  }
  return(outlier.vec)
  }


y<-c(1,4,7,8,0,11,4,8,4,9,17)
z<-c(8,5,6,9,2,6,19,1,6,7,220)
x<-cbind(y,z)
