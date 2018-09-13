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
#' @return A list: 
#' @return (1) preproccessed data, data frame
#' @return (2) the first month conform to some regularity, character
#' @return (3) the last month conform to some regularity, character
#' @return Note: here, the total number of sample/month should be greater than 10.
#' @export
preprocess2<-function(district,host,port,user,password,dbname,startmon,endmon,enctype){
  # district<-'bj';host<-'10.11.10.34';port<-3307;user<-'menglin';password<-'menglin_2017_AR';dbname<-'cityre_beijing'
  # startmon<-'201805';endmon<-'201806';enctype<-'SET NAMES gbk'
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
  if (substr(endmon,5,6) == "12") {
    endmon_yr <- as.character(as.numeric(substr(endmon,1,4))+1)
    endmon_mon <- "01"
  }else{
    endmon_yr <- substr(endmon,1,4)
    endmon_mon <- as.character(as.numeric(substr(endmon,5,6))+1)
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
                AND offertm>='",substr(startmon,1,4),"-",substr(startmon,5,6),"-01 00:00:00'", 
               "and offertm<'",endmon_yr,"-",endmon_mon,"-01 00:00:00'",
                "GROUP BY qd_prop.ha,DATE_FORMAT(offertm,'%Y-%m')")
  price <- dbGetQuery(con,str)
  names(price) <- c("code","name","dist","long","lat","no1","no2","t")
  # 删除NA
  price <- na.omit(price)
  
  dbDisconnect(con)
  
  #############################################################################
  #################### combine to "result" dataframe ##########################
  #############################################################################
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
  
  # col.names of the dataframe
  result<-data.frame(array(NA,dim=c(nrow(price),nmonth+8)))
  names(result)[1:7]<-names(price)[1:7]
  result[1:7]<-price[1:7]
  names(result)[8:(nmonth+7)]<-months
  names(result)[nmonth+8]<-"st"
  
  # 重复点
  result<-result[!(duplicated(result[4:5])),]
  
  ##### 时间标准化
  price$t<-paste0(substr(price$t,1,4),substr(price$t,6,7))
  # stop$st<-paste0(substr(stop$st,1,4),substr(stop$st,6,7))
  
  # 整理报价和售罄时间
  for (j in 1:nrow(result))
  {
    for (k in 8:(ncol(result)-1))
    {
      result[j,k]<-subset(price,price$code==result[j,1] & 
                            price$t==colnames(result)[k])[1,6]
    }
    # result[j,ncol(result)]<-subset(stop,stop$code==result[j,1])[1,2]
  }
  
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
  
  # 填充各月度价格数据（开始报价至今）
  if (nmonth>1 & nmonth<=dataLife){
    for (j in 1:nrow(result))
    {
      for (i in 9:(ncol(result)-1))  
      {
        if (is.na(result[j,i]))
        {
          result[j,i]=result[j,i-1]
        }
      }
    }
  }else{ 
    # 超过3年有效期，填充为-1
    for (j in 1:nrow(result))
    {
      for (i in (8+dataLife):(ncol(result)-1))
      {
        if (is.na(result[j,i]))
        {
          # 3年有效期, 超过有效期，值为-1
          if(all(is.na(result[j,(i-dataLife):(i-1)]))){
            result[j,i]=-1
          }else{
            result[j,i]=result[j,i-1]
          }
        }
      }
    }
    # 正常填充
    for (j in 1:nrow(result))
    {
      for (i in 9:(ncol(result)-1))
      {
        if (is.na(result[j,i]))
        {
          result[j,i]=result[j,i-1]
        }
      }
    }
  }
  # 将超过有效期的-1改为NA
  result[result==-1] <- NA
  
  # # 剔除售罄后数据
  # if (nmonth>1){
  #   for (j in 1:nrow(result))
  #   {
  #     for (i in 9:(ncol(result)-1))
  #     {
  #       if (!is.na(result[j,ncol(result)]) & 
  #           as.numeric(names(result[i])) > as.numeric(result[j,ncol(result)]))
  #       {
  #         result[j,i]=NA
  #       }
  #     }
  #   }
  # }
  
  # # 剔除样本点小于10的月度数据
  # delcol <- c()
  # j <- 1
  # for (i in 8:(ncol(result)-1))
  # {
  #   if(sum(!is.na(result[i])) < 10){
  #     delcol[j] <- i
  #     j <- j+1
  #   }
  # }
  # if (length(delcol) > 0) {result <- result[-delcol]}
  
  if (ncol(result) < 9){
    cat("Records less than 10 !!!")
    return(0)
  }
  startmon <- colnames(result[8])
  endmon <- colnames(result[ncol(result)-1])
  
  ####################################################
  ################### groupping ######################
  ####################################################
  # year, group.R: 
    # for (i in substr(startmon,1,4):substr(endmon,1,4))
    # {
    #   result<-group(result,i)
    # }
  
  return(list(result,startmon,endmon))
}
