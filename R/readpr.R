
# extract the data of the given month

readpr<-function(result,date){
  
  colno<-which(names(result)==date)
  
  pr<-cbind(result[4],result[5],result[colno])
  
  names(pr)<-c("x","y","p")
  
  pr<-data.frame(pr)
  
  pr<-na.omit(pr)
  
  row.names(pr)<-c(1:nrow(pr))

  return(pr)
  
}