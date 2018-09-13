# new house price level
calLevelPost2 <- function(mon,configfile,outpath,sys){
  source('R/preprocessPost2.R',encoding = 'utf-8')
  if (!file.exists(outpath)) {dir.create(outpath)}
  if (!file.exists(paste0(outpath,"/pre-data"))) {dir.create(paste0(outpath,"/pre-data"))}
  if (!file.exists(paste0(outpath,"/pre-data/",mon))) {dir.create(paste0(outpath,"/pre-data/",mon))}
  if (!file.exists(paste0(outpath,"/level"))) {dir.create(paste0(outpath,"/level"))}
  
  # the encoding type defined by the system
  if (sys == "linux"){
    enctype <- "SET NAMES utf8"
  }else{
    enctype <- "SET NAMES gbk"
  }
  
  # calculating......
  cat("1. calculate the level of each city:\n")
  if (!file.exists("city_info.txt")){
    cat("There's no configure file with database information!!!")
    return(0)
  }else{
    cityinfo <- read.table(configfile,header = TRUE, stringsAsFactors = FALSE, fileEncoding = 'UTF-8')
  }
  
  for (i in 1:nrow(cityinfo))
  {
    ######################################################
    ################ server configuration ################
    ######################################################
    # the city's name, pinyinabb  ########################
    district <- cityinfo$pinyinabb[i] ####################
    # the city's server host  ############################
    host <- cityinfo$host[i]  ############################
    # the city's server port  ############################
    port <- cityinfo$port[i]  ############################
    # the city's server user  ############################
    user <- cityinfo$user[i]  ############################
    # the city's server password  ########################
    password <- cityinfo$password[i] #####################
    # the city's database name  ##########################
    dbname <- cityinfo$dbname[i]  ########################
    ######################################################
    ######################################################
    ######################################################
    
    # calculate each city
    cat(cityinfo$chinese[i],"(",i,")\t")
    # the server exist? if not, stop!
    if (is.na(district) | is.na(host) | is.na(port) | is.na(user) | is.na(password) | is.na(dbname)){
      cat("There's no server for this city!\n")
      next
    }
    preData <- preprocessPost2(district,host,port,user,password,dbname,mon,enctype,outpath)
    # update pre-data
    write.table(preData,paste0(outpath,"/pre-data/",mon,'/',district,".txt"),
                row.names = FALSE,sep='\t', fileEncoding = 'utf-8')
    # read level file
    levelFile <- paste0(outpath,"/level/",district,"level.dat")
    if (file.exists(levelFile))
    {
      level <- read.table(levelFile,header = TRUE,fileEncoding = 'utf-8')
    }else{
      level <- data.frame("time"=NA,"value"=NA)
    }
    # means
    meanData <- preData[ncol(preData)-1][!is.na(preData[ncol(preData)-1])]
    level[nrow(level)+1,] <- c(mon,round(mean(meanData)))
    level<-level[!(duplicated(level['time'])),]
    # write
    write.table(level,paste0(outpath,"/level/",district,"level.dat"),row.names = FALSE)
    cat(as.character(Sys.time()),"\tsucced!\n")
  }
  
  # return(level)
}