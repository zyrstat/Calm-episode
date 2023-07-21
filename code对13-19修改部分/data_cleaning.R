install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("openair")
install.packages("plm")
install.packages("Amelia")
install.packages("VIM")
install.packages("mice")
install.packages("tseries")
library(VIM)
library(mice)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(openair)
library("plm")
library(Amelia)
library('tseries')
library(grid)
#missing data
require(magrittr)
require(Amelia)
library("geosphere")
library(ncdf4)
library(pinyin)
library(stringr)

nc <- nc_open("/Users/zyr/Documents/PM2.5/datas/boundary_layer_height.nc", write=FALSE, 
              readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
print(nc)
latitudes=ncvar_get( nc = nc, varid = 'latitude')
longitudes=ncvar_get( nc = nc, varid = 'longitude')
time=ncvar_get( nc = nc, varid = 'time')
blhs=(ncvar_get( nc = nc, varid = 'blh'))[,,1,]
#2013-01-01:0h --> 990552
blhs=blhs[,,time>=990552]
time=time[time>=990552]
  
station_pol=read.csv("/Users/zyr/Documents/PM2.5/datas/location_weather_pollution/Total_station_pollution.csv",stringsAsFactors = F,fileEncoding = 'GB2312')
station_wea=read.csv("/Users/zyr/Documents/PM2.5/datas/location_weather_pollution/Total_station_weather_nomark.csv",stringsAsFactors = F,fileEncoding = 'UTF8')

object="北京" #"河北"
object.py = str_to_title(py(object, sep = "",dic = pydic(method = "toneless",dic = "pinyin2")))
object.ab='BJ' #'HB'

station_pol_object=station_pol[station_pol$province.c==object,]
station_wea_object=station_wea[station_wea$province==object,]
station_pol_object$station=str_to_title(py(station_pol_object$station, sep = "",dic = pydic(method = "toneless",dic = "pinyin2")))
#For Beijing
station_pol_object$station[station_pol_object$station=="Yongyuedian"]="Yongledian"

#之前的数据
#onames=unlist(strsplit(dir(paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/",object.py,"/")),'.csv'))
#updated data
onames=unlist(strsplit(dir(paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/",object.py,"_2020update/")),'.csv'))

sites_pol_object=str_split_fixed(onames,"_",2)[,2]
station_pol_object$station.py=station_pol_object$station
station_pol_object$station=paste0(station_pol_object$city,'_',station_pol_object$station)
setdiff(station_pol_object$station,onames)
setdiff(onames,station_pol_object$station)

BJ_18=read.csv(paste0("/Users/zyr/Documents/PM2.5/datas/location_weather_pollution/",object.ab,"_18.csv"),stringsAsFactors = F)
station_wea_object=arrange(station_wea_object,station_code)

#match pollution stations and weather stations
spswmatch=distinct(BJ_18[,c('weather_station_code','station_code')])
#length(unique(spswmatch$weather_station_code))
spsws=inner_join(spswmatch,station_pol_object[,c('code','station','lat','lon')],by=c('station_code'='code'))
spsws=arrange(spsws,weather_station_code,station_code)

reps=table(spsws$weather_station_code)
reps=reps[order(as.numeric(names(reps)))]
pws=data.frame(weather_station_code=as.numeric(names(reps)),
               station=spsws$station[c(1,cumsum(reps[1:(length(reps)-1)])+1)])

#之前的数据
#setwd(paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/",object.py,'/'))
#updated data
setwd(paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/",object.py,'_2020update/'))

#setwd("F:/data")
path1="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/NSdata/"
path2="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/data_cleaned/"  
path3="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/" 

#oname="Beijing_Aotizhongxin"

year1=2013
year2=2019
#欧洲时间差8个小时，2013-01-01，2020-02-29
timerange_blh=1:(as.numeric(24*(as.Date(paste0(year2+1,'-03-01'))-as.Date(paste0(year1,'-01-01'))))-8)

cleandata <- function(oname,year1,year2,path1,path2)
{
  data_a=read.csv(paste0(oname,".csv"),header=T)
  data_a=data_a[data_a$year>=year1,]
  data_a=data_a[1:which(data_a$year==year2+1&data_a$month==2&data_a$day==29&data_a$hour==23),]
  
  #blh 
  lon_oname=station_pol_object$lon[station_pol_object$station==oname]
  lat_oname=station_pol_object$lat[station_pol_object$station==oname]
  if(length(lon_oname)!=1) print(oname)
  data_a$blh=c(rep(NA,8),blhs[which.min(abs(longitudes-lon_oname)),which.min(abs(latitudes-lat_oname)),timerange_blh])
  data_a=data_a[data_a$X>=data_a$X[data_a$year==year1&data_a$month==2&data_a$day==1&data_a$hour==0],]
  
  #判断删还是补，只补前后都不缺失的
  dsn <- function(x,rm)
  {
    return(all(x%in%rm))#true是都在
  }
  
  qs=data_a$X[is.na(data_a$RAIN)]
  rm=data_a$X[which(!is.na(data_a$RAIN))]
  need=cbind(qs-1,qs+1)
  rdsn=apply(need,1,dsn,rm)
  #补RAIN
  for(x in qs[rdsn])
  {
    if(data_a$RAIN[which(data_a$X==x-1)]==data_a$RAIN[which(data_a$X==x+1)])
      data_a$RAIN[which(data_a$X==x)]=data_a$RAIN[which(data_a$X==x-1)]
  }
  
  continufIRAIN0 <- function(da_s)#包括自己和前面共多少个连续点RAIN为0
  {
    ns=nrow(da_s)
    x=rep(0,ns)
    x[!is.na(da_s$RAIN)&da_s$RAIN==0]=1
    
    a=rep(0,ns)#前
    
    a[1]=x[1]
    
    for (i in 2:ns) 
    {
      if(x[i]==1)
        a[i]=a[i-1]+x[i]
    }
    
    da_s$ncfir0=a
    da_s
  }
  
  #wind types or wind speed miss
  wdmiss=which(is.na(data_a$wd)|is.na(data_a$cbwd)|is.na(data_a$WSPM))
  
  #构造新的风向
  angle <- function(x)
  {
    # calculate the degree
    if(is.na(x))#缺失
      return(0)
    else if(x =='E' )
      return(360)
    else if(x=='ENE')
      return(22.5)
    else if(x == 'NE')
      return(45)
    else if(x =='NNE') 
      return(67.5)
    else if(x == 'N')
      return(90)
    else if(x =='NNW')
      return(112.5)
    else if(x=='NW')
      return(135)
    else if(x=='WNW')
      return(157.5)
    else if(x=='W')
      return(180)  
    else if(x=='WSW')
      return(202.5)
    else if(x=='SW')
      return(225)
    else if(x=='SSW')
      return(247.5)
    else if(x=='S')
      return(270)
    else if(x=='SSE')
      return(292.5)
    else if(x=='SE')
      return(315)
    else if(x=='ESE')
      return(337.5)
    else #CV
      return(180)
  }
 
  jN <- function(x)
  {
    if(!is.na(x) & (grepl(pattern = "N",x) | x=='W'))#风向中含北或正西风
      return(1)
    else 
      return(0)
  }
#------------------------change 2020-05-14-----------------   
  
  #smoothing angles of wind, in order to calculate INws and ISws
  judgeN <- function(da)
  {
    miss=which(is.na(da$wd)&is.na(da$cbwd))# wd cbwd
    x=rep(1,nrow(da))#wd cbwd 是否是NA的行
    x[miss]=0
    y=rev(x)
    
    iN=rep(0,nrow(da))
    iN[1]=jN(da$wd[1])
    a=rep(0,nrow(da))#前
    b=rep(0,nrow(da))#后
    a[1]=x[1]
    b[1]=y[1]
    
    for(i in 2:nrow(da)) 
    {
      iN[i]=jN(da$wd[i]) 
      if(x[i]==1)
        a[i]=a[i-1]+x[i]
      if(y[i]==1)
        b[i]=b[i-1]+y[i]
    }
    
    #-------angles-----------    
    if(all(names(da)!='winddir')) {da$winddir=NA 
    da$angl=apply(as.matrix(da$wd),1,angle)
    winddir_exist=F}
    else {da$angl=90-da$winddir 
    angl_alter=apply(as.matrix(da$wd),1,angle)
    da$angl[!is.na(da$angl)&da$angl<=0]=360+da$angl[!is.na(da$angl)&da$angl<=0]
    da$angl[is.na(da$angl)]=angl_alter[is.na(da$angl)]
    da$angl[is.na(da$angl)]=0
    winddir_exist=T}
    
    ang=da$angl[1]
    
    #东风，只是为了光滑风向角
    if(ang==360)
    {
      pt=1:min(3,nrow(da))
      #ppn=length(which(iN[pt]==1))/length(which(!is.na(da$wd[pt])))#是“北”风的那些在非缺失中的占比
      ppn=length(which(iN[pt]==1))/length(da$wd[pt])
      if(ppn>0.5)
        da$angl[1]=0#N的那一侧
      else
        da$angl[1]=360#S的那一侧
    }
    
    for(i in 2:nrow(da)) 
    {
      ang=da$angl[i]
      if(ang==360)
      {
        pt=max(i-2,1):min(i+2,nrow(da))#5个
        #ppn=length(which(iN[pt]==1))/length(which(!is.na(da$wd[pt])))
        ppn=length(which(iN[pt]==1))/length(da$wd[pt])
        if(ppn>0.5)
          da$angl[i]=0
        else
          da$angl[i]=360
      }
    }
    
    #这里只是为了smoothing，后面会改
    da$ncof=c(0,a[-nrow(da)])#前面共多少个连续点wd cbwd非na
    da$ncob=rev(c(0,b[-nrow(da)]))#后面共多少个连续点wd cbwd非na
    
    da$angl[is.na(da$wd)&da$cbwd=='CV']=180
    if(!winddir_exist) {da$avan=rep(0,nrow(da))}#光滑风向角度
    else {da$avan=da$angl}
    
    #according to the smoothing angles
    da$N=rep(0,nrow(da))
    da$S=rep(0,nrow(da))
    #smoothing angles and at the same time complete 某些 WSPM
    da$avan[c(1,nrow(da))]= da$angl[c(1,nrow(da))]
    if(180<da$avan[1]&da$avan[1]<=360)
      da$S[1]=1
    else if(!is.na(da$wd[1]))
      da$N[1]=1
    if(180<da$avan[nrow(da)]&da$avan[nrow(da)]<=360)
      da$S[nrow(da)]=1
    else if(!is.na(da$wd[nrow(da)]))
      da$N[nrow(da)]=1
    for(i in 2:(nrow(da)-1))
    {
      h=min(da$ncof[i],da$ncob[i])
      if(h==1)
      {
        if(is.na(da$wd[i]))
        {
          da$avan[i]=weighted.mean(da$angl[(i-1):(i+1)], c(3,4,3)/6)
          if(180<da$avan[i]&da$avan[i]<=360)
            da$S[i]=1
          else
            da$N[i]=1
          if(is.na(da$WSPM[i]))
            da$WSPM[i]=weighted.mean(da$WSPM[c(i-1,i+1)], c(3,3)/6)
        }
        else
        {
          if(!winddir_exist) {da$avan[i]=weighted.mean(da$angl[(i-1):(i+1)], c(3,4,3)/10)}
          if(180<da$avan[i]&da$avan[i]<=360)
            da$S[i]=1
          else
            da$N[i]=1
        }
      }
      else if(h==2)
      {
        if(is.na(da$wd[i]))
        {
          da$avan[i]=weighted.mean(da$angl[(i-2):(i+2)], c(2,3,4,3,2)/10)
          if(180<da$avan[i]&da$avan[i]<=360)
            da$S[i]=1
          else
            da$N[i]=1
          if(is.na(da$WSPM[i]))
            da$WSPM[i]=weighted.mean(da$WSPM[c(i-2,i-1,i+1,i+2)], c(2,3,3,2)/10)
        }
        else 
        {
          if(!winddir_exist) {da$avan[i]=weighted.mean(da$angl[(i-2):(i+2)], c(2,3,4,3,2)/14)}
          if(180<da$avan[i]&da$avan[i]<=360)
            da$S[i]=1
          else
            da$N[i]=1
        }
      }
      else if(h>=3)
      {
        if(is.na(da$wd[i]))
        {
          da$avan[i]=weighted.mean(da$angl[(i-3):(i+3)], c(1,2,3,4,3,2,1)/12)
          if(180<da$avan[i]&da$avan[i]<=360)
            da$S[i]=1
          else
            da$N[i]=1
          if(is.na(da$WSPM[i]))
            da$WSPM[i]=weighted.mean(da$WSPM[c(i-3,i-2,i-1,i+1,i+2,i+3)], c(1,2,3,3,2,1)/12)
        }
        else 
        {
          if(!winddir_exist) {da$avan[i]=weighted.mean(da$angl[(i-3):(i+3)], c(1,2,3,4,3,2,1)/16)}
          if(180<da$avan[i]&da$avan[i]<=360)
            da$S[i]=1
          else
            da$N[i]=1
        }
      }
    }
    
    da$IN=rep(0,nrow(da))#北风连续累积时长
    da$IS=rep(0,nrow(da))#南风连续累积时长
    da$INws=rep(0,nrow(da))#北风连续累积风速
    da$ISws=rep(0,nrow(da))#南风连续累积风速
    da$epfmINws=rep(0,nrow(da))
    da$epfpn=rep(0,nrow(da))
    da$epfNen=rep(0,nrow(da))
    da$epfpen=rep(0,nrow(da))
    da$epfmINws48=rep(0,nrow(da))
    da$epfpn48=rep(0,nrow(da))
    da$epfNen48=rep(0,nrow(da))
    da$epfpen48=rep(0,nrow(da))
    da$IN[1]=da$N[1]
    da$IS[1]=da$S[1]
    da$epfmINws[1]=0
    da$epfpn[1]=0
    da$epfNen[1]=0
    da$epfpen[1]=0
    da$epfmINws48[1]=0
    da$epfpn48[1]=0
    da$epfNen48[1]=0
    da$epfpen48[1]=0
    
    if(da$IN[1]==1)
      da$INws[1]=da$WSPM[1]
    if(da$IS[1]==1)
      da$ISws[1]=da$WSPM[1]
    WS=da$WSPM
    WS[is.na(WS)]=0
    da$uwind=WS*cos((da$angl+180)/180*pi)
    da$uwind[wdmiss]=0
    da$uwind[abs(da$uwind)<10^(-14)]=0
    da$vwind=WS*sin((da$angl+180)/180*pi)
    da$vwind[wdmiss]=0
    da$vwind[abs(da$vwind)<10^(-14)]=0
    da$uIws=rep(0,nrow(da))
    da$uIws[1]=da$uwind[1]
    da$vIws=rep(0,nrow(da))
    da$vIws[1]=da$vwind[1]
    
    for(i in 2:nrow(da)) 
    {
      if(da$N[i]==1)
      {
        da$IN[i]=da$IN[i-1]+1
        da$INws[i]=da$INws[i-1]+WS[i]
        if(is.na(da$WSPM[i]))
          print(i)
      }
      else if(da$S[i]==1)
      {
        da$IS[i]=da$IS[i-1]+1
        da$ISws[i]=da$ISws[i-1]+WS[i]
        if(is.na(da$WSPM[i]))
          print(i)
      }
      if(da$uwind[i]*da$uwind[i-1]>0)
        da$uIws[i]=da$uIws[i-1]+da$uwind[i]
      else
        da$uIws[i]=da$uwind[i]
      if(da$vwind[i]*da$vwind[i-1]>0)
        da$vIws[i]=da$vIws[i-1]+da$vwind[i]
      else
        da$vIws[i]=da$vwind[i]
      
      interv=max(i-min(da$ncof[i],24),1):(i-1)
      interv48=max(i-min(da$ncof[i],48),1):(i-1)
      da$epfmINws[i]=max(da$INws[interv])
      da$epfpn[i]=length(which(da$IN[interv]>0))/length(interv)
      da$epfNen[i]=sum(WS[interv[1]+which(da$IN[interv]>0)-1])
      if(sum(WS[interv])==0)
        da$epfpen[i]=0
      else
        da$epfpen[i]=da$epfNen[i]/sum(WS[interv])
      da$epfmINws48[i]=max(da$INws[interv48])
      da$epfpn48[i]=length(which(da$IN[interv48]>0))/length(interv48)
      da$epfNen48[i]=sum(WS[interv48[1]+which(da$IN[interv48]>0)-1])
      if(sum(WS[interv48])==0)
        da$epfpen48[i]=0
      else
        da$epfpen48[i]=da$epfNen48[i]/sum(WS[interv48])      
    }
    da
  }
  
  data_a=continufIRAIN0(data_a)
  
  data_a=judgeN(data_a)
  
  write.csv(data_a,paste0(path1,oname,"_NS.csv"))

  #----------------------------------------------------
  data_a$oX=1:nrow(data_a)
  # 2013.3.1.0--2020.2.29.23 Aotizhongxin
  bbb=data_a$X[which(data_a$year==year1&data_a$month==3&data_a$day==1&data_a$hour==0)]
  eee=data_a$X[which(data_a$year==year2+1&data_a$month==2&data_a$day==29&data_a$hour==23)]
  
  #前后都不是null,complete气象变量
  comple_me <- function(data_a,meter)
  {
    qs=data_a$X[which(is.na(data_a[,meter]))]
    rm=data_a$X[which(!is.na(data_a[,meter]))]
    need=cbind(qs-1,qs+1)
    rdsn=apply(need,1,dsn,rm)
    for(x in qs[rdsn])
    {
      data_a[,meter][which(data_a$X==x)]=mean(c(data_a[,meter][which(data_a$X==x-1)],
                                                data_a[,meter][which(data_a$X==x+1)]))
    }
    data_a
  }
  
  meters=c('TEMP','DEWP','HUMI','PRES','blh')
  for(i in 1:length(meters))
  {
    data_a=comple_me(data_a,meters[i])
  }
  gass=c('PM2.5','PM10','SO2','NO2','CO','O3')
  for(i in 1:length(gass))
  {
    data_a=comple_me(data_a,gass[i])
  }
  
  apply(is.na(data_a),2,sum)
  
  continufpm <- function(da_s)#前面共多少个连续点PM2.5<=35
  {
    ns=nrow(da_s)
    x=rep(0,ns)
    x[!is.na(da_s$PM2.5)&da_s$PM2.5<=35]=1
    
    a=rep(0,ns)#前
    
    a[1]=x[1]
    
    for (i in 2:ns) 
    {
      if(x[i]==1)
        a[i]=a[i-1]+x[i]
    }
    
    #da_s$pm35=apply(cbind(c(0,a[-ns]),da_s$ncof),1,min)
    da_s$pm35=c(0,a[-ns])
    da_s
  }
  
  data_a=continufpm(data_a)  

  #delete
  data_a=data_a[setdiff(which(!is.na(data_a$cbwd)&!is.na(data_a$WSPM)&!is.na(data_a$RAIN)),wdmiss),]
  
  apply(is.na(data_a),2,sum)
  
  #补完之后删
  data_a=data_a[!is.na(data_a$TEMP)&!is.na(data_a$DEWP)&!is.na(data_a$HUMI)&!is.na(data_a$PRES)&!is.na(data_a$blh),]
  #注意IRAIN有问题
  
  apply(is.na(data_a),2,sum)
  
  aqs=intersect(which(is.na(data_a$PM2.5)),which(is.na(data_a$PM10)))
  aqs=intersect(aqs,which(is.na(data_a$SO2)))
  aqs=intersect(aqs,which(is.na(data_a$NO2)))
  aqs=intersect(aqs,which(is.na(data_a$CO)))
  aqs=intersect(aqs,which(is.na(data_a$O3)))
  data_a=data_a[-aqs,]
  
  #------------------------------------------
  
  #5年的数据
  da_a=data_a[data_a$X%in%(bbb:eee),]
  print(missmap(da_a[,gass],col=c("black","grey"),main =paste0('Missing Map for ',oname)))
  da_a=da_a[!is.na(da_a$PM2.5),]
  
  da_a$loca=rep(oname,nrow(da_a))#"a"
  
  
  avpm <- function(data)#dataframe,smoothing with weights 1,2,4,2,1
  {
    data$avpm=data$PM2.5
    leng=nrow(data)#行数
    for (i in 1:leng) {
      if(i==1|i==leng)
        data$avpm[i]=data$avpm[i]
      else if(data$X[i]-data$X[i-1]>1|data$X[i+1]-data$X[i]>1)#断裂的开头或尾部
        data$avpm[i]=data$avpm[i]
      else if(i==2|i==leng-1)
        data$avpm[i]=weighted.mean(data$PM2.5[(i-1):(i+1)], c(2,4,2)/8)
      else if((data$X[i]-data$X[i-1]==1&data$X[i]-data$X[i-2]>2)|(data$X[i+1]-data$X[i]==1&data$X[i+2]-data$X[i]>2))
        data$avpm[i]=weighted.mean(data$PM2.5[(i-1):(i+1)], c(2,4,2)/8)
      else
        data$avpm[i]=weighted.mean(data$PM2.5[(i-2):(i+2)], c(1,2,4,2,1)/10)
    }
    data
  }
  
  da_a=avpm(da_a)
  
  #X从1开始了
  da_a$X=da_a$X-bbb+1
  
  continufb <- function(da_s)#前后有多少连续点
  {
    should=da_s$X[1]:da_s$X[nrow(da_s)]
    miss=which(!(should%in%da_s$X))# 断裂处在should中的行号
    x=rep(1,length(should))#是否是delete的行
    x[miss]=0
    y=rev(x)
    
    a=rep(0,length(should))#前
    b=rep(0,length(should))#后
    a[1]=x[1]
    b[1]=y[1]
    for (i in 2:length(should)) 
    {
      if(x[i]==1)
        a[i]=a[i-1]+x[i]
      if(y[i]==1)
        b[i]=b[i-1]+y[i]
    }
    
    da_s$ncof=a[-miss]-1
    da_s$ncob=rev(b)[-miss]-1
    da_s
  }
  
  da_a=continufb(da_a)
  #写清理后的数据
  write.csv(da_a,file=paste0(path2,oname,"_cleaned.csv"))
  
}
#只有avpm,ncof,ncob是针对13-17的

# da_agw=rbind(da_a,da_g,da_w)
# write.csv(da_agw,file="F:/agw_dnt_data_cleaned/agw_cleaned.csv")
pdf(file= paste0(path3,object.py,'_missing.pdf'),family = 'GB1')

for(i in 1:length(onames))
{cleandata(onames[i],year1,year2,path1,path2)
  print(onames[i])}
dev.off()

#---------------------------------------------
path4="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/NSdata_update/"
path5="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/data_cleaned_update/"
wsuwinds=c()
wsvwinds=c()
for(i in 1:nrow(pws))
{
  stationwinds=read.csv(paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/NSdata/",
                               pws$station[i],"_NS.csv"),stringsAsFactors = F)
  
  wsuwinds=cbind(wsuwinds,stationwinds$uwind)
  wsvwinds=cbind(wsvwinds,stationwinds$vwind)
}
colnames(wsuwinds) <- colnames(wsvwinds) <- as.character(pws$weather_station_code)
#前面为水平风速，后为竖直方向
write.csv(cbind(wsuwinds,wsvwinds),file=paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/regionwindfield/",object.py,"wind_weather_station.csv"))
#distance between pollution station and weather station
lonlat_wea=station_wea_object[station_wea_object$station_code %in% pws$weather_station_code,c('lon','lat')]
dispw.u=c(); dispw.v=c()
for(h in 1:nrow(station_pol_object))
{
  lonlat_weau=lonlat_wea
  lonlat_weav=lonlat_wea
  lonlat_weau[,2]=station_pol_object[h,c('lat')]
  lonlat_weav[,1]=station_pol_object[h,c('lon')]
  dispw.u=rbind(dispw.u,distm(station_pol_object[h,c('lon','lat')],lonlat_weau))
  dispw.v=rbind(dispw.v,distm(station_pol_object[h,c('lon','lat')],lonlat_weav))
  
}

rownames(dispw.u)=rownames(dispw.v)=station_pol_object$station
colnames(dispw.u)=colnames(dispw.v)=pws$weather_station_code
write.csv(dispw.u,file=paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/regionwindfield/",object.py,"distance_pollution_weather_u.csv"))
write.csv(dispw.v,file=paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/regionwindfield/",object.py,"distance_pollution_weather_v.csv"))

dispw.u[dispw.u==0]=0.01; dispw.v[dispw.v==0]=0.01
Iwus=wsuwinds%*%t(10000*3.6/dispw.u); 
Iwvs=wsvwinds%*%t(10000*3.6/dispw.v); 

countpositive <- function(z)
{
  return(length(which(z>0)))
}

lon_weather=station_wea_object[station_wea_object$station_code %in% pws$weather_station_code,c('lon')]
lat_weather=station_wea_object[station_wea_object$station_code %in% pws$weather_station_code,c('lat')]

for(i in 1:length(onames))
{
  stationdata=read.csv(paste0(path1,onames[i],"_NS.csv"),stringsAsFactors = F)
  idN=which(lat_weather>station_pol_object$lat[station_pol_object$station==onames[i]])
  idS=which(lat_weather<station_pol_object$lat[station_pol_object$station==onames[i]])
  idE=which(lon_weather>station_pol_object$lon[station_pol_object$station==onames[i]])
  idW=which(lon_weather<station_pol_object$lon[station_pol_object$station==onames[i]])
  
  #南北方向
  if(length(idN)==0)
    massN=0
  else
    massN=apply(as.matrix(wsvwinds[,idN]),1,countpositive)/length(idN)
  if(length(idS)==0)
    massS=0
  else
    massS=apply(as.matrix(wsvwinds[,idS]),1,countpositive)/length(idS)
  if(length(idE)==0)
    massE=0
  else
    massE=apply(as.matrix(wsuwinds[,idE]),1,countpositive)/length(idE)
  if(length(idW)==0)
    massW=0
  else
    massW=apply(as.matrix(wsuwinds[,idW]),1,countpositive)/length(idW)
  
  stationdata$Iwu=Iwus[,which(colnames(Iwus)==onames[i])]
  stationdata$Iwv=Iwvs[,which(colnames(Iwvs)==onames[i])]
  stationdata$massN=massN
  stationdata$massS=massS
  stationdata$massE=massE
  stationdata$massW=massW
  stationdata$IIwu=rep(0,nrow(stationdata))
  stationdata$IIwv=rep(0,nrow(stationdata))
  stationdata$IIwu[1]=stationdata$Iwu[1]
  stationdata$IIwv[1]=stationdata$Iwv[1]
  for(j in 2:nrow(stationdata))
  {
    if(stationdata$Iwu[j]*stationdata$Iwu[j-1]>0)
      stationdata$IIwu[j]=stationdata$IIwu[j-1]+stationdata$Iwu[j]
    else
      stationdata$IIwu[j]=stationdata$Iwu[j]
    if(stationdata$Iwv[j]*stationdata$Iwv[j-1]>0)
      stationdata$IIwv[j]=stationdata$IIwv[j-1]+stationdata$Iwv[j]
    else
      stationdata$IIwv[j]=stationdata$Iwv[j]
  }
  stationdata$X=1:nrow(stationdata)
  write.csv(stationdata,file=paste0(path4,onames[i],"_NS.csv"))
  stationd=read.csv(paste0(path2,onames[i],"_cleaned.csv"),stringsAsFactors = F)
  stationd=inner_join(stationd,stationdata[,c('X','Iwu','Iwv','IIwu','IIwv','massN','massS','massE','massW')],by=c('oX'='X'))
  
  write.csv(stationd,file=paste0(path5,onames[i],"_cleaned.csv"))
  print(onames[i])
}

lon_lats=station_pol_object[station_pol_object$station%in% c(paste0('Beijing_',c("Dongsi","Tiantan","Nongzhanguan",   
                                                                                 "Guanyuan","Wanliu","Aotizhongxin"))),
                            c('station','province','city','lon','lat','station.py')]
lon_lats=rbind(lon_lats,station_pol_object[station_pol_object$station%in% c(paste0('Baoding_',c("Huadianerqu","Youyongguan","Jiancezhan")),
                                                                            paste0('Tangshan_',c("Shierzhong","Wuziju","Leidazhan"))),
                                           c('station','province','city','lon','lat','station.py')])
write.csv(lon_lats,file = "/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/lon_lats.csv")
