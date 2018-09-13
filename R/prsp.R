
# box-cox conversion, and convert to "sp" form

prsp<-function(pr){
  
  names(pr)<-c("x","y","z")
  
  #Box-cox conversion
  tr<-boxcox(z~x,data=pr,lambda=seq(-1,1,length=10),plotit=FALSE)
  lambda<-tr$x[tr$y == max(tr$y)]
  if (lambda == 0)
  {
    pr$z=log(pr$z)
  }else{
    pr$z=(pr$z^lambda-1)/lambda
  }
  
  #convert to SP style
  coordinates(pr) <- ~x+y
  
  return(pr)
  
}