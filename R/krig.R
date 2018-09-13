krig<-function(myprsp,pr,basexy,m,nmax){
  
  # kriging
  krige <- krige(z~1, myprsp, basexy, model = m, nmax=26, debug.level=0)
  names(pr)<-c("x","y","z")
  proj4string(krige) <- CRS("+init=epsg:3857")
  
  # Boc-cox inverse-conversion
  tr<-boxcox(z~x,data=pr,lambda=seq(-1,1,length=10),plotit = FALSE)
  lambda<-tr$x[tr$y == max(tr$y)]
  if (lambda == 0)
  {
    krige$var1.pred=exp(krige$var1.pred)
  }else{
    krige$var1.pred=(1+lambda*krige$var1.pred)^(1/lambda)
  }
  
  # convert to dataframe
  # krige<-data.frame(krige)
  # krige<-krige[-4]
  # names(krige)<-c("x","y","p")
  
  return(krige)
  
}
