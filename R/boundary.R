#' @title Boundaries
#' @description Extract the administration bondary and effective position restricted by existed houses, of a city
#' @param district City name, character
#' @return administration boundary, data frame
#' @export
boundary<-function(district){
  
  ###################################################
  # the used libraries and functions:               #
  #  library(maps)                                  #
  #  library(mapdata)                               #
  #  library(sp)                                    #
  #  library(ggplot2)                               #
  #  library(maptools)                              #
  #  library(readxl)                                #
  #  library(mgcv)                                  #
  #  library(rgeos)                                 #
  #  library(raster)                                #
  #  library(RMySQL)                                #
  #  library(rgdal)                                 #
  #  source("segpiece.R", encoding = 'UTF-8')       #
  #  source("fortify-spatial.r",encoding = 'UTF-8') #
  ###################################################
  
  # projection: +init=epsg:3857 +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0
  #  +units=m +nadgrids=@null +no_defs
  newproj <- CRS("+init=epsg:3857")
  
  ########################################################
  ########## fetch boundary from shapfile  ###############
  ########################################################
  suppressWarnings(x<-readShapePoly('data/city/city.shp'))
  city<-subset(x,x@data$city_code==district)
  if (nrow(city@data) == 0){
    cat("There's no boundary shapfile data!!!")
    return(0)
  }
  
  ############################################################################################
  ##################### CRS transformation, and convert to dataframe #########################
  ############################################################################################
  projection(city) <- CRS("+init=epsg:4326")
  # city1 <- plyr::ldply(city@polygons,fortify)  ## fortify maybe deprecated. then, can be instituted by broom::tidy
  city <- spTransform(city,newproj)
  
  return(city)
  
}
