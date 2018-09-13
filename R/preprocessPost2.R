#' @title Preprocess
#' @description Extract useful data frome database, and organize them to a needed format
#' @param district City name, character
#' @param host Host of the server, character
#' @param port Port of the server, numeric
#' @param user User of the server, character
#' @param password Password of the server, character
#' @param dbname Database name of city, character
#' @param mon Current month in the form as param startmon, character
#' @param enctype The encoding type, gbk or utf8 or else ? character
#' @param outpath root dir for saving
#' @return A list: 
#' @return (1) preproccessed data, data frame
#' @return (2) the first month conform to some regularity, character
#' @return (3) the last month conform to some regularity, character
#' @return Note: here, the total number of sample/month should be greater than 10.
#' @export
preprocessPost2<-function(district,host,port,user,password,dbname,mon,enctype,outpath){
  # district<-'bi';host<-'10.11.10.34';port<-3307;user<-'menglin';password<-'menglin_2017_AR';dbname<-'cityre_beijing'
  # startmon<-'201805';endmon<-'201806';enctype<-'SET NAMES gbk';outpath<-'./result2'
  ##############################################
  #the used libraries and functions:           #
  #  library(maps)                             #
  #  library(mapdata)                          #
  #  library(sp)                               #
  #  library(ggplot2)                          #
  #  library(maptools)                         #
  #  library(readxl)                           #
  #  library(tcltk)                            #
  #  library(RMySQL)                           #
  #  source("groupping.R",encoding = 'UTF-8')  #
  ##############################################
  # 设置房价有效期
  dataLife <- 3*12
  # the last month
  # the month of last year, the laste month
  if (as.numeric(substr(mon,5,6)) > 1)
  {
    lastmon <- as.character(as.numeric(mon)-1)
  }else{
    lastmon <- as.character(as.numeric(mon)-89)
  }
  
  ### read the sample data of the laster month
  prefile <- paste0(outpath,"/pre-data/",lastmon,'/',district,".txt")
  if (file.exists(prefile))
  {
    result <- read.table(prefile,header = TRUE,fileEncoding = 'utf-8')
    names(result)[8:(ncol(result)-1)] <- gsub("X","",names(result)[8:(ncol(result)-1)])
  }else{
    result <- data.frame("code"=c(NA),"name"=c(NA),"dist"=c(NA),"long"=c(NA),
                         "lat"=c(NA),"no1"=c(NA),"no2"=c(NA),"st"=c(NA))
    result <- result[-1,]
  }
  
  #############################################################################
  ################################# fetch data ################################
  #############################################################################
  # entrance the database
  con <- try(dbConnect(MySQL(),host=host,dbname=dbname,user=user,
                       password=password,port=port),silent = TRUE)
  if (class(con) == "try-error"){
    cat("The database cannot be connected !!!")
    return(0)
  }
  dbSendQuery(con,enctype)
  
  # fetch price data
  if (substr(mon,5,6) == "12") {
    mon_yr <- as.character(as.numeric(substr(mon,1,4))+1)
    mon_mon <- "01"
  }else{
    mon_yr <- substr(mon,1,4)
    mon_mon <- as.character(as.numeric(substr(mon,5,6))+1)
  }
  str <- paste0("SELECT qd_prop.ha,ha_kindred.`name`,district.dist_name,avg(ha_position.x)x,avg(ha_position.y)y,avg(unitprice)p,count(*)c,DATE_FORMAT(offertm,'%Y-%m')ym from qd_forsale_history qd_forsale
                INNER JOIN qd_prop on qd_prop.propcode=qd_forsale.propcode
                INNER JOIN district on district.dist_code=qd_prop.distcode AND district.distID<99
                INNER JOIN ha_kindred on ha_kindred.ha_code=qd_prop.ha AND ha_kindred.kindred_cl_code='rn'
                LEFT JOIN ha_position on ha_position.ha_code=qd_prop.ha
                where qd_prop.ha is not null 
                AND showorder=1 
                and IFNULL(Flag,0)>=0 
                and IFNULL(qd_prop.proptype,11)=11 
                AND offertm>='",substr(mon,1,4),"-",substr(mon,5,6),"-01 00:00:00'", 
                "and offertm<'",mon_yr,"-",mon_mon,"-01 00:00:00'",
                "GROUP BY qd_prop.ha,DATE_FORMAT(offertm,'%Y-%m')")
  price <- dbGetQuery(con,str)
  names(price) <- c("code","name","dist","long","lat","p","count","t")
  names(price)[6:8] <- c(mon,"no2","no1")
  # 删除NA
  price <- na.omit(price)
  
  dbDisconnect(con)
  
  #############################################################################
  #################### combine to "result" dataframe ##########################
  #############################################################################
  # combine result0 and mon-price
  result <- merge(result,price,by=intersect(names(result),names(price)),all=T)
  result <- cbind(result[1:(ncol(result)-2)],result[ncol(result)],result[ncol(result)-1])
  
  # 删除重复点
  result<-result[!(duplicated(result[4:5])),]
  if (nrow(result) == 0) {
    cat("There's no record about house price !!!")
    return(0)
  }
  
  # # 整理售罄时间
  # for (j in 1:nrow(result))
  # {
  #   result[j,ncol(result)]<-subset(stop,stop$code==result[j,1])[1,2]
  # }
  
  # 删除给定月份无报价数据的所有楼盘数据
  ntemp<-ncol(result)
  for (j in 1:nrow(result))
  {
    result$ok[j]<-!all(is.na(result[j,8:(ntemp-1)]))
  }
  result<-subset(result,ok)
  result<-result[-ncol(result)]
  if (nrow(result) == 0){
    cat("There's something wrong with the database, ignore this problem !!!")
    return(0)
  }
  row.names(result)<-c(1:nrow(result))
  
  # 本月度在result中的列数
  moncol <- ncol(result)-1
  
  # 填充该月度价格数据（开始报价至今）
  if (moncol > 8 & moncol <= 7+dataLife){
    for (j in 1:nrow(result))
    {
      if (is.na(result[j,moncol])) {result[j,moncol]=result[j,moncol-1]}
    }
  }else{
    for (j in 1:nrow(result))
    {
      if (is.na(result[j,moncol])){
        lifeV <- all(result[j,(moncol-dataLife):(moncol-2)]==result[j,moncol-1])
        if (is.na(lifeV) | lifeV==FALSE){
          result[j,moncol]=result[j,moncol-1]
        }
        else {
          result[j,moncol]=NA
        }
      } 
    }
  }
  
  # 剔除售罄后数据
  if (moncol > 8){
    for (j in 1:nrow(result))
    {
      if (!is.na(result[j,ncol(result)]) & 
          as.numeric(names(result[moncol])) > as.numeric(result[j,ncol(result)]))
      {
        result[j,moncol]=NA
      }
    }
  }
  
  # # 剔除样本点小于10的月度数据
  # if(sum(!is.na(result[moncol])) < 10){result <- result[-moncol]}
  
  if (ncol(result) < 9){
    cat("Records less than 10 !!!")
    return(0)
  }
  
  # update pre-data
  if (!file.exists(paste0(outpath,"/pre-data/",mon))) {dir.create(paste0(outpath,"/pre-data/",mon))}
  write.table(result,paste0(outpath,"/pre-data/",mon,'/',district,".txt"),
              row.names = FALSE,sep='\t', fileEncoding = 'utf-8')
  
  return(result)
}
