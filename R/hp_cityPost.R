#' @title House Price calculation of each city
#' @description Calculate every city results in spetial database, based on Odinary Krigging interpolation.
#' @param district City name, character
#' @param host Host of the server, character
#' @param port Port of the server, numeric
#' @param user User of the server, character
#' @param password Password of the server, character
#' @param dbname Database name of city, character
#' @param mon Current month in the form as param startmon, character
#' @param resol Mesh resolution, unit: meter, numeric
#' @param outpath Output path
#' @param sys The system type, Linux or Wins, defines the encoding type of configure file here, character
#' @param newHouse if new house?
#' @return Magnitude, Link and Year-over-year distibution of House Price
#' @details THe outputs mainly contains Altitude, Link and Year-over-year distibution of the price.
#' @export
hp_cityPost <- function(district,host,port,user,password,dbname,mon,resol,outpath,sys,newHouse){
  
  ##################################################
  #the used libraries and functions:               #
  #  library(MASS)                                 #
  #  library(gstat)                                #
  #  library(raster)                               #
  #  library(RMySQL)                               #
  #  source("preprocessPost.R",encoding = 'UTF-8') #
  #  source("boundary.R",encoding = 'UTF-8')       #
  #  source("grid.R",encoding = 'UTF-8')           #
  #  source("readpr.R",encoding = 'UTF-8')         #
  #  source("prsp.R",encoding = 'UTF-8')           #
  #  source("krig.R",encoding = 'UTF-8')           #
  ##################################################
  
  # the encoding type defined by the system
  if (sys == "linux"){
    enctype <- "SET NAMES utf8"
  }else{
    enctype <- "SET NAMES gbk"
  }
  
  ######################################################
  ##################### preprocess #####################
  ######################################################
  if (newHouse){
    result <- preprocessPost(district,host,port,user,password,dbname,mon,enctype,outpath)
  }else{
    result <- preprocessPost2(district,host,port,user,password,dbname,mon,enctype,outpath)
  }
  
  if (class(result) == "numeric") {
    if (result == 0) return(0)
  }
  
  ###########################################
  ########## CRS transformation #############
  ###########################################
  swap <- result[4:5]
  names(swap) <- c("long","lat")
  coordinates(swap) <- ~long+lat
  projection(swap) <- CRS("+init=epsg:4326")
  newproj <- CRS("+init=epsg:3857")
  swap <- spTransform(swap,newproj)
  swap <- as.data.frame(swap)
  result[4:5] <- swap
  
  #####################################
  ############ boundary ###############
  #####################################
  bound<-boundary(district)
  if (class(bound) == "numeric") {
    if (bound == 0) return(0)
  }
  
  ####################################
  #### calculate the locate range ####
  ####################################
  xgridmin<-xmin(bound)-0.
  xgridmax<-xmax(bound)+0.
  ygridmin<-ymin(bound)-0.
  ygridmax<-ymax(bound)+0.
  
  ############################################
  ### set grids, resolution.default = 500m ###
  ############################################
  xgrid <- seq(xgridmin, xgridmax, by = resol)
  ygrid <- seq(ygridmin, ygridmax, by = resol)
  basexy <- grid(xgrid,ygrid)
  
  ###############################################################################################
  ##################### calculate the price distribution of this      month #####################
  ###############################################################################################
  # the month of last year, the laste month
  if (as.numeric(substr(mon,5,6)) > 1)
  {
    lastmon <- as.character(as.numeric(mon)-1)
  }else{
    lastmon <- as.character(as.numeric(mon)-89)
  }
  lastyr <- paste0(as.numeric(substr(mon,1,4))-1,substr(mon,5,6))
  
  # price
  # extract data
  pr <- tryCatch(readpr(result,mon),error=function(e){return("yes")})
  if (class(pr) == "character") {
    return(0)  
  }else{
    if (nrow(pr) < 20) {
      return(0) # if the records of one month is less than 20, ignore!
    }
  }
  
  # boundary
  housebd <- pr[1:2]
  # CRS transformation
  coordinates(housebd)<-~x+y
  projection(housebd) <- CRS("+init=epsg:3857")
  
  # box-cox conversion, and convert to "sp" form
  myprsp <- prsp(pr)
  
  iferror <- tryCatch({ # variogram
                        vgm <- variogram(z~1,myprsp);
                        # fitting
                        m <- fit.variogram(vgm,vgm(model="Sph",psill=mean(vgm$gamma),range=max(vgm$dist)/2,
                          nugget=min(vgm$gamma)),fit.kappa=TRUE);
                        # kriging interplation
                        krige <- krig(myprsp,pr,basexy,m,26)},
                        error=function(e){return("yes")})
  
  if (class(iferror) == "character") {
    if (iferror == "yes") return(0)
  }
  
  output0 <- mask(raster(krige),bound)
  output0 <- mask(output0,housebd)
  names(output0) <- 'p'
  writeRaster(output0,filename=paste0(outpath,"/temp/ras_11_newcalprice","/ras_11_",district,"_newcalprice_",mon,".tif"),
              format='GTiff', NAflag=-9999, overwrite=TRUE)
  
  ###############################################################################################
  #### interpolation of the following months, calculate the link and year-over-year change, #####
  #### always with the price ********************** #############################################
  ###############################################################################################
  # link
  # extract data
  pr <- tryCatch(readpr(result,lastmon),error=function(e){return("yes")})
  if (class(pr) == "character") {
    return(0)  
  }else{
    if (nrow(pr) < 20) {
      return(0) # if the records of one month is less than 20, ignore!
    }else{
      # boundary
      housebd <- pr[1:2]
      # CRS transformation
      coordinates(housebd)<-~x+y
      projection(housebd) <- CRS("+init=epsg:3857")
      
      # box-cox conversion, and convert to "sp" form
      myprsp <- prsp(pr)
      
      iferror <- tryCatch({ # variogram
        vgm <- variogram(z~1,myprsp);
        # fitting
        m <- fit.variogram(vgm,vgm(model="Sph",psill=mean(vgm$gamma),range=max(vgm$dist)/2,
                                   nugget=min(vgm$gamma)),fit.kappa=TRUE);
        # kriging interplation
        krige <- krig(myprsp,pr,basexy,m,26)},
        error=function(e){return("yes")})
      
      if (class(iferror) == "character") {
        if (iferror == "yes") return(0)
      }
      
      output1 <- mask(raster(krige),bound)
      output1 <- mask(output1,housebd)
      names(output1) <- 'p'
      
      output2 <- (output1-output0)/output0
      writeRaster(output2, filename=paste0(outpath,"/temp/ras_11_newlink","/ras_11_",district,"_newlink_",mon,".tif"),
                  format='GTiff', NAflag=-9999, overwrite=TRUE)
    }
  }
  
  # year over year
  # extract data
  pr <- tryCatch(readpr(result,lastyr),error=function(e){return("yes")})
  if (class(pr) == "character") {
    return(0)  
  }else{
    if (nrow(pr) < 20) {
      return(0) # if the records of one month is less than 20, ignore!
    }else{
      # boundary
      housebd <- pr[1:2]
      # CRS transformation
      coordinates(housebd)<-~x+y
      projection(housebd) <- CRS("+init=epsg:3857")
      
      # box-cox conversion, and convert to "sp" form
      myprsp <- prsp(pr)
      
      iferror <- tryCatch({ # variogram
        vgm <- variogram(z~1,myprsp);
        # fitting
        m <- fit.variogram(vgm,vgm(model="Sph",psill=mean(vgm$gamma),range=max(vgm$dist)/2,
                                   nugget=min(vgm$gamma)),fit.kappa=TRUE);
        # kriging interplation
        krige <- krig(myprsp,pr,basexy,m,26)},
        error=function(e){return("yes")})
      
      if (class(iferror) == "character") {
        if (iferror == "yes") return(0)
      }
      
      output1 <- mask(raster(krige),bound)
      output1 <- mask(output1,housebd)
      names(output1) <- 'p'
      
      output2 <- (output1-output0)/output0
      writeRaster(output2, filename=paste0(outpath,"/temp/ras_11_newlike","/ras_11_",district,"_newlike_",mon,".tif"),
                  format='GTiff', NAflag=-9999, overwrite=TRUE)
    }
  }
  
  return(0)
  
}
