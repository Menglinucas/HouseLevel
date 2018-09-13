#' @title House Price calculation of each city
#' @description Calculate every city results in spetial database, based on Odinary Krigging interpolation.
#' @param district City name, character
#' @param host Host of the server, character
#' @param port Port of the server, numeric
#' @param user User of the server, character
#' @param password Password of the server, character
#' @param dbname Database name of city, character
#' @param startmon The firt month in the form such as "200606", character
#' @param endmon The last month in the form as param startmon, character
#' @param resol Mesh resolution, unit: meter, numeric
#' @param outpath Output path
#' @param sys The system type, Linux or Wins, defines the encoding type of configure file here, character
#' @param newHouse if new house?
#' @return Magnitude, Link and Year-over-year distibution of House Price
#' @details THe outputs mainly contains Altitude, Link and Year-over-year distibution of the price.
#' @export
hp_city <- function(district,host,port,user,password,dbname,startmon,endmon,resol,outpath,sys,newHouse){
  
  ##############################################
  #the used libraries and functions:           #
  #  library(MASS)                             #
  #  library(gstat)                            #
  #  library(raster)                           #
  #  library(RMySQL)                           #
  #  source("preprocess.R",encoding = 'UTF-8') #
  #  source("boundary.R",encoding = 'UTF-8')   #
  #  source("grid.R",encoding = 'UTF-8')       #
  #  source("readpr.R",encoding = 'UTF-8')     #
  #  source("prsp.R",encoding = 'UTF-8')       #
  #  source("krig.R",encoding = 'UTF-8')       #
  ##############################################
  
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
    result0 <- preprocess(district,host,port,user,password,dbname,startmon,endmon,enctype)
  }else{
    result0 <- preprocess2(district,host,port,user,password,dbname,startmon,endmon,enctype)
  }
  
  if (class(result0) == "numeric") {
    if (result0 == 0) return(0)
  }
  result <- result0[[1]]
  # save pre-data at the last month
  write.table(result,paste0(outpath,"/pre-data/",endmon,'/',district,".txt"),
              row.names = FALSE,sep='\t', fileEncoding = 'utf-8')
  startmon <- result0[[2]]
  endmon <- result0[[3]]

  ##############################################################################
  ############################ months to calculate #############################
  ##############################################################################
  nmonth<-(as.numeric(substr(endmon,1,4))-as.numeric(substr(startmon,1,4)))*12+
    as.numeric(substr(endmon,5,6))-as.numeric(substr(startmon,5,6))+1
  months<-c()
  months[1]<-as.numeric(startmon)
  if (nmonth>1){
    for (i in 2:nmonth)
    {
      if (as.numeric(substr(months[i-1],5,6))<12)
      {
        months[i]<-months[i-1]+1
      }else{
        months[i]<-months[i-1]+89
      }
    }
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
  ##################### calculate the price distribution of the first month #####################
  ###############################################################################################
  # extract data
  pr <- tryCatch(readpr(result,months[1]),error=function(e){return("yes")})
  if (class(pr) == "character") {
    cat(months[1],"\t")  # ignore
  }else{
    if (nrow(pr) < 20) {
      cat(months[1],"\t") # if the records of one month is less than 20, ignore!
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
        if (iferror == "yes") cat(months[1],"\t")  # ignore
      }else{
        # # blank and collect the data
        # x <- krige$x
        # y <- krige$y
        # krige$mark1 <- inSide(list("x"=bound$long,"y"=bound$lat),x,y)
        # krige$mark2 <- inSide(list("x"=housebd$long,"y"=housebd$lat),x,y)
        # krige <- subset(krige,mark1 & mark2)
        
        # convert to raster, and write to local files
        # output0 <- rasterFromXYZ(krige[1:3], res = c(resol,resol), crs = "+init=epsg:3857")
        output0 <- mask(raster(krige),bound)
        output0 <- mask(output0,housebd)
        names(output0) <- 'p'
        writeRaster(output0,filename=paste0(outpath,"/temp/ras_11_newcalprice","/ras_11_",district,"_newcalprice_",months[1],".tif"),
                    format='GTiff', NAflag=-9999, overwrite=TRUE)
        
        cat(months[1],"\t")
      }
    }
  }

  ###############################################################################################
  #### interpolation of the following months, calculate the link and year-over-year change ######
  ###############################################################################################
  if (nmonth>1){
    for (i in 2:nmonth)
    {
      pr <- tryCatch(readpr(result,months[i]),error=function(e){return("yes")})
      if (class(pr) == "character") {
        cat(months[i],"\t")  #ignore
      }else{
        if (nrow(pr) < 20) {
          cat(months[i],"\t") # if the records of one month is less than 20, ignore!
        }else{
          # boundary
          housebd <- pr[1:2]
          # CRS transformation
          coordinates(housebd)<-~x+y
          projection(housebd) <- CRS("+init=epsg:3857")
          
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
            if (iferror == "yes") cat(months[i],"\t")  # ignore
          }else{
            # x <- krige$x
            # y <- krige$y
            # krige$mark1 <- inSide(list("x"=bound$long,"y"=bound$lat),x,y)
            # krige$mark2 <- inSide(list("x"=housebd$long,"y"=housebd$lat),x,y)
            # krige <- subset(krige,mark1 & mark2)
            # output1 <- rasterFromXYZ(krige[1:3], res = c(resol,resol), crs = "+init=epsg:3857")
            output1 <- mask(raster(krige),bound)
            output1 <- mask(output1,housebd)
            names(output1) <- 'p'
            writeRaster(output1, filename=paste0(outpath,"/temp/ras_11_newcalprice","/ras_11_",district,"_newcalprice_",months[i],".tif"),
                        format='GTiff', NAflag=-9999, overwrite=TRUE)
            
            # calculate the link change, output2
            if (exists('output0')){
              output2 <- (output1-output0)/output0
              writeRaster(output2, filename=paste0(outpath,"/temp/ras_11_newlink","/ras_11_",district,"_newlink_",months[i],".tif"),
                          format='GTiff', NAflag=-9999, overwrite=TRUE)
            }
            
            #calculate the year over year change
            if (i>12) {
              fname <- paste0(outpath,"/temp/ras_11_newcalprice","/ras_11_",district,"_newcalprice_",months[i-12],".tif")
              if (file.exists(fname)){
                yoy1 <- raster(fname)
                output3 <- (output1-yoy1)/yoy1
                writeRaster(output3, filename=paste0(outpath,"/temp/ras_11_newlike","/ras_11_",district,"_newlike_",months[i],".tif"),
                            format='GTiff', NAflag=-9999, overwrite=TRUE)
              }
            }
            
            cat(months[i],"\t")
            
            output0 <- output1
          }
        }
      }
    }
  }
  
  return(0)
  
}
