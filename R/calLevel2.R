# new house price level
calLevel2 <- function(startmon,endmon,configfile,outpath,sys){
  source('R/preprocess2.R',encoding = 'utf-8')
  if (!file.exists(outpath)) {dir.create(outpath)}
  if (!file.exists(paste0(outpath,"/pre-data"))) {dir.create(paste0(outpath,"/pre-data"))}
  if (!file.exists(paste0(outpath,"/pre-data/",endmon))) {dir.create(paste0(outpath,"/pre-data/",endmon))}
  if (!file.exists(paste0(outpath,"/level"))) {dir.create(paste0(outpath,"/level"))}
  
  # the encoding type defined by the system
  if (sys == "linux"){
    enctype <- "SET NAMES utf8"
  }else{
    enctype <- "SET NAMES gbk"
  }
  
  # months
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
    preData <- preprocess2(district,host,port,user,password,dbname,startmon,endmon,enctype)
    cityData <- preData[[1]]
    startmon <- preData[[2]]
    endmon <- preData[[3]]
    # update pre-data
    write.table(cityData,paste0(outpath,"/pre-data/",endmon,'/',district,".txt"),
                row.names = FALSE,sep='\t', fileEncoding = 'utf-8')
    
    # means
    level <- data.frame("time"=NA,"value"=NA)
    j <- 1
    for (i in 8:(ncol(cityData)-1)){
      meanData <- cityData[i][!is.na(cityData[i])]
      if (length(meanData) > 20){
        level[j,] <- c(colnames(cityData)[i],round(mean(meanData)))
        j <- j+1
      }
    }
    # write
    write.table(level,paste0(outpath,"/level/",district,"level.dat"),row.names = FALSE)
    cat(as.character(Sys.time()),"\tsucced!\n")
  }
  
  # return(level)
}