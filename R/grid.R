
# meshing used for kriging 

grid<-function(xgrid,ygrid){
  
  basexy <- expand.grid(xgrid, ygrid)
  
  colnames(basexy) <- c("x", "y")
  
  coordinates(basexy) <- ~x+y
  
  gridded(basexy) <- TRUE
  
  return(basexy)
  
}