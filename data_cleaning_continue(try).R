path7="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/NSdata_update_new/"
path8="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/data_cleaned_update_new/"

pollutant_smooth <- function(data,pollutant_name)#dataframe,smoothing with weights 1,2,4,2,1
{
  avpm=data[,pollutant_name]
  leng=nrow(data)#行数
  for (i in 1:leng) {
    if(i==1|i==leng)
    {}
    else if(data$X[i]-data$X[i-1]>1|data$X[i+1]-data$X[i]>1)#断裂的开头或尾部
    {}
    else if(i==2|i==leng-1)
      avpm[i]=weighted.mean(data[(i-1):(i+1),pollutant_name], w=c(2,4,2)/8,na.rm=T)
    else if((data$X[i]-data$X[i-1]==1&data$X[i]-data$X[i-2]>2)|(data$X[i+1]-data$X[i]==1&data$X[i+2]-data$X[i]>2))
      avpm[i]=weighted.mean(data[(i-1):(i+1),pollutant_name], w=c(2,4,2)/8,na.rm=T)
    else
      avpm[i]=weighted.mean(data[(i-2):(i+2),pollutant_name], w=c(1,2,4,2,1)/10,na.rm=T)
  }
  avpm[is.nan(avpm)]=NA
  return(avpm)
}

judgeN_new <- function(stationdata,sitename)# x--two columes, wd, avan
{
  stationdata.ori=stationdata
  specific=str_split_fixed(sitename,"_",2)[1]
  stationdata$N=rep(0,nrow(stationdata))
  stationdata$S=rep(0,nrow(stationdata))
  
  # if(specific=='Beijing')
  # {
  #   for(uu in 1:nrow(stationdata))
  #   {
  #     if(!is.na(stationdata$wd[uu])&stationdata$wd[uu]=='CV')
  #     {stationdata$N[uu]=0;stationdata$S[uu]=1}
  #     else if(22.5<=stationdata$avan[uu]&stationdata$avan[uu]<202.5)
  #     {stationdata$N[uu]=1;stationdata$S[uu]=0}
  #     else if(stationdata$avan[uu]==0&is.na(stationdata$wd[uu]))
  #     {stationdata$N[uu]=0;stationdata$S[uu]=0}
  #     else 
  #     {stationdata$N[uu]=0;stationdata$S[uu]=1}
  #   }
  #   
  #   stationdata$IN=rep(0,nrow(stationdata))#北风连续累积时长
  #   stationdata$IS=rep(0,nrow(stationdata))#南风连续累积时长
  #   stationdata$INws=rep(0,nrow(stationdata))#北风连续累积风速
  #   stationdata$ISws=rep(0,nrow(stationdata))#南风连续累积风速
  #   stationdata$epfmINws=rep(0,nrow(stationdata))
  #   stationdata$epfpn=rep(0,nrow(stationdata))
  #   stationdata$epfNen=rep(0,nrow(stationdata))
  #   stationdata$epfpen=rep(0,nrow(stationdata))
  #   stationdata$epfmINws48=rep(0,nrow(stationdata))
  #   stationdata$epfpn48=rep(0,nrow(stationdata))
  #   stationdata$epfNen48=rep(0,nrow(stationdata))
  #   stationdata$epfpen48=rep(0,nrow(stationdata))
  #   stationdata$IN[1]=stationdata$N[1]
  #   stationdata$IS[1]=stationdata$S[1]
  #   stationdata$epfmINws[1]=0
  #   stationdata$epfpn[1]=0
  #   stationdata$epfNen[1]=0
  #   stationdata$epfpen[1]=0
  #   stationdata$epfmINws48[1]=0
  #   stationdata$epfpn48[1]=0
  #   stationdata$epfNen48[1]=0
  #   stationdata$epfpen48[1]=0
  #   
  #   if(stationdata$IN[1]==1)
  #     stationdata$INws[1]=stationdata$WSPM[1]
  #   if(stationdata$IS[1]==1)
  #     stationdata$ISws[1]=stationdata$WSPM[1]
  #   WS=stationdata$WSPM
  #   WS[is.na(WS)]=0
  #   
  #   for(i in 2:nrow(stationdata)) 
  #   {
  #     if(stationdata$N[i]==1)
  #     {
  #       stationdata$IN[i]=stationdata$IN[i-1]+1
  #       stationdata$INws[i]=stationdata$INws[i-1]+WS[i]
  #       if(is.na(stationdata$WSPM[i]))
  #         print(i)
  #     }
  #     else if(stationdata$S[i]==1)
  #     {
  #       stationdata$IS[i]=stationdata$IS[i-1]+1
  #       stationdata$ISws[i]=stationdata$ISws[i-1]+WS[i]
  #       if(is.na(stationdata$WSPM[i]))
  #         print(i)
  #     }
  #     
  #     interv=max(i-min(stationdata$ncof[i],24),1):(i-1)
  #     interv48=max(i-min(stationdata$ncof[i],48),1):(i-1)
  #     stationdata$epfmINws[i]=max(stationdata$INws[interv])
  #     stationdata$epfpn[i]=length(which(stationdata$IN[interv]>0))/length(interv)
  #     stationdata$epfNen[i]=sum(WS[interv[1]+which(stationdata$IN[interv]>0)-1])
  #     if(sum(WS[interv])==0)
  #       stationdata$epfpen[i]=0
  #     else
  #       stationdata$epfpen[i]=stationdata$epfNen[i]/sum(WS[interv])
  #     stationdata$epfmINws48[i]=max(stationdata$INws[interv48])
  #     stationdata$epfpn48[i]=length(which(stationdata$IN[interv48]>0))/length(interv48)
  #     stationdata$epfNen48[i]=sum(WS[interv48[1]+which(stationdata$IN[interv48]>0)-1])
  #     if(sum(WS[interv48])==0)
  #       stationdata$epfpen48[i]=0
  #     else
  #       stationdata$epfpen48[i]=stationdata$epfNen48[i]/sum(WS[interv48])      
  #   }
  #   return(stationdata)
  # }
  if(specific=='Beijing')
  {
    return(stationdata.ori)
  }
  else if(specific%in%c('Tangshan','Baoding'))
  # else if(specific%in%c('Tangshan'))
  {
    for(uu in 1:nrow(stationdata))
    {
      if(!is.na(stationdata$wd[uu])&stationdata$wd[uu]=='CV')
      {stationdata$N[uu]=0;stationdata$S[uu]=1}
      # # else if(22.5<=stationdata$avan[uu]&stationdata$avan[uu]<202.5)
      # else if((11.25<stationdata$avan[uu]&stationdata$avan[uu]<=191.25))
      # {stationdata$N[uu]=1;stationdata$S[uu]=0} 
      # else if(stationdata$avan[uu]==0&is.na(stationdata$wd[uu]))
      # {stationdata$N[uu]=0;stationdata$S[uu]=0}
      # else 
      # {stationdata$N[uu]=0;stationdata$S[uu]=1}
      else if((191.25<=stationdata$avan[uu]&stationdata$avan[uu]<348.75))
      {stationdata$N[uu]=0;stationdata$S[uu]=1}
      else if(stationdata$avan[uu]==0&is.na(stationdata$wd[uu]))
      {stationdata$N[uu]=0;stationdata$S[uu]=0}
      else
      {stationdata$N[uu]=1;stationdata$S[uu]=0}
      
    }
    
    stationdata$IN=rep(0,nrow(stationdata))#北风连续累积时长
    stationdata$IS=rep(0,nrow(stationdata))#南风连续累积时长
    stationdata$INws=rep(0,nrow(stationdata))#北风连续累积风速
    stationdata$ISws=rep(0,nrow(stationdata))#南风连续累积风速
    stationdata$epfmINws=rep(0,nrow(stationdata))
    stationdata$epfpn=rep(0,nrow(stationdata))
    stationdata$epfNen=rep(0,nrow(stationdata))
    stationdata$epfpen=rep(0,nrow(stationdata))
    stationdata$epfmINws48=rep(0,nrow(stationdata))
    stationdata$epfpn48=rep(0,nrow(stationdata))
    stationdata$epfNen48=rep(0,nrow(stationdata))
    stationdata$epfpen48=rep(0,nrow(stationdata))
    stationdata$IN[1]=stationdata$N[1]
    stationdata$IS[1]=stationdata$S[1]
    stationdata$epfmINws[1]=0
    stationdata$epfpn[1]=0
    stationdata$epfNen[1]=0
    stationdata$epfpen[1]=0
    stationdata$epfmINws48[1]=0
    stationdata$epfpn48[1]=0
    stationdata$epfNen48[1]=0
    stationdata$epfpen48[1]=0
    
    if(stationdata$IN[1]==1)
      stationdata$INws[1]=stationdata$WSPM[1]
    if(stationdata$IS[1]==1)
      stationdata$ISws[1]=stationdata$WSPM[1]
    WS=stationdata$WSPM
    WS[is.na(WS)]=0
    
    for(i in 2:nrow(stationdata)) 
    {
      if(stationdata$N[i]==1)
      {
        stationdata$IN[i]=stationdata$IN[i-1]+1
        stationdata$INws[i]=stationdata$INws[i-1]+WS[i]
        if(is.na(stationdata$WSPM[i]))
          print(i)
      }
      else if(stationdata$S[i]==1)
      {
        stationdata$IS[i]=stationdata$IS[i-1]+1
        stationdata$ISws[i]=stationdata$ISws[i-1]+WS[i]
        if(is.na(stationdata$WSPM[i]))
          print(i)
      }
      
      interv=max(i-min(stationdata$ncof[i],24),1):(i-1)
      interv48=max(i-min(stationdata$ncof[i],48),1):(i-1)
      stationdata$epfmINws[i]=max(stationdata$INws[interv])
      stationdata$epfpn[i]=length(which(stationdata$IN[interv]>0))/length(interv)
      stationdata$epfNen[i]=sum(WS[interv[1]+which(stationdata$IN[interv]>0)-1])
      if(sum(WS[interv])==0)
        stationdata$epfpen[i]=0
      else
        stationdata$epfpen[i]=stationdata$epfNen[i]/sum(WS[interv])
      stationdata$epfmINws48[i]=max(stationdata$INws[interv48])
      stationdata$epfpn48[i]=length(which(stationdata$IN[interv48]>0))/length(interv48)
      stationdata$epfNen48[i]=sum(WS[interv48[1]+which(stationdata$IN[interv48]>0)-1])
      if(sum(WS[interv48])==0)
        stationdata$epfpen48[i]=0
      else
        stationdata$epfpen48[i]=stationdata$epfNen48[i]/sum(WS[interv48])      
    }
    return(stationdata)
  }
  # else if(specific=='Baoding')
  # {
  #   for(uu in 1:nrow(stationdata))
  #   {
  #     if(!is.na(stationdata$wd[uu])&stationdata$wd[uu]=='CV')
  #     {stationdata$N[uu]=0;stationdata$S[uu]=1}
  #     # else if((22.5<=stationdata$avan[uu]&stationdata$avan[uu]<112.5)|(247.5<=stationdata$avan[uu]&stationdata$avan[uu]<337.5))
  #     # else if(22.5<=stationdata$avan[uu]&stationdata$avan[uu]<202.5)
  #     # else if((33.75<=stationdata$avan[uu]&stationdata$avan[uu]<78.75)|(191.25<=stationdata$avan[uu]&stationdata$avan[uu]<326.25))
  #     else if((33.75<=stationdata$avan[uu]&stationdata$avan[uu]<56.25)|(191.25<=stationdata$avan[uu]&stationdata$avan[uu]<348.25))
  #     {stationdata$N[uu]=0;stationdata$S[uu]=1}
  #     else if(stationdata$avan[uu]==0&is.na(stationdata$wd[uu]))
  #     {stationdata$N[uu]=0;stationdata$S[uu]=0}
  #     else
  #     {stationdata$N[uu]=1;stationdata$S[uu]=0}
  #   }
  # 
  #   stationdata$IN=rep(0,nrow(stationdata))#北风连续累积时长
  #   stationdata$IS=rep(0,nrow(stationdata))#南风连续累积时长
  #   stationdata$INws=rep(0,nrow(stationdata))#北风连续累积风速
  #   stationdata$ISws=rep(0,nrow(stationdata))#南风连续累积风速
  #   stationdata$epfmINws=rep(0,nrow(stationdata))
  #   stationdata$epfpn=rep(0,nrow(stationdata))
  #   stationdata$epfNen=rep(0,nrow(stationdata))
  #   stationdata$epfpen=rep(0,nrow(stationdata))
  #   stationdata$epfmINws48=rep(0,nrow(stationdata))
  #   stationdata$epfpn48=rep(0,nrow(stationdata))
  #   stationdata$epfNen48=rep(0,nrow(stationdata))
  #   stationdata$epfpen48=rep(0,nrow(stationdata))
  #   stationdata$IN[1]=stationdata$N[1]
  #   stationdata$IS[1]=stationdata$S[1]
  #   stationdata$epfmINws[1]=0
  #   stationdata$epfpn[1]=0
  #   stationdata$epfNen[1]=0
  #   stationdata$epfpen[1]=0
  #   stationdata$epfmINws48[1]=0
  #   stationdata$epfpn48[1]=0
  #   stationdata$epfNen48[1]=0
  #   stationdata$epfpen48[1]=0
  # 
  #   if(stationdata$IN[1]==1)
  #     stationdata$INws[1]=stationdata$WSPM[1]
  #   if(stationdata$IS[1]==1)
  #     stationdata$ISws[1]=stationdata$WSPM[1]
  #   WS=stationdata$WSPM
  #   WS[is.na(WS)]=0
  # 
  #   for(i in 2:nrow(stationdata))
  #   {
  #     if(stationdata$N[i]==1)
  #     {
  #       stationdata$IN[i]=stationdata$IN[i-1]+1
  #       stationdata$INws[i]=stationdata$INws[i-1]+WS[i]
  #       if(is.na(stationdata$WSPM[i]))
  #         print(i)
  #     }
  #     else if(stationdata$S[i]==1)
  #     {
  #       stationdata$IS[i]=stationdata$IS[i-1]+1
  #       stationdata$ISws[i]=stationdata$ISws[i-1]+WS[i]
  #       if(is.na(stationdata$WSPM[i]))
  #         print(i)
  #     }
  # 
  #     interv=max(i-min(stationdata$ncof[i],24),1):(i-1)
  #     interv48=max(i-min(stationdata$ncof[i],48),1):(i-1)
  #     stationdata$epfmINws[i]=max(stationdata$INws[interv])
  #     stationdata$epfpn[i]=length(which(stationdata$IN[interv]>0))/length(interv)
  #     stationdata$epfNen[i]=sum(WS[interv[1]+which(stationdata$IN[interv]>0)-1])
  #     if(sum(WS[interv])==0)
  #       stationdata$epfpen[i]=0
  #     else
  #       stationdata$epfpen[i]=stationdata$epfNen[i]/sum(WS[interv])
  #     stationdata$epfmINws48[i]=max(stationdata$INws[interv48])
  #     stationdata$epfpn48[i]=length(which(stationdata$IN[interv48]>0))/length(interv48)
  #     stationdata$epfNen48[i]=sum(WS[interv48[1]+which(stationdata$IN[interv48]>0)-1])
  #     if(sum(WS[interv48])==0)
  #       stationdata$epfpen48[i]=0
  #     else
  #       stationdata$epfpen48[i]=stationdata$epfNen48[i]/sum(WS[interv48])
  #   }
  #   return(stationdata)
  # }
  else
    print('I have not consider this case!')
}

onames_update=c('Beijing_Dongsi','Beijing_Tiantan','Beijing_Nongzhanguan',
               'Beijing_Guanyuan','Beijing_Wanliu','Beijing_Aotizhongxin',
               'Tangshan_Shierzhong','Tangshan_Wuziju','Tangshan_Leidazhan',
               'Baoding_Huadianerqu','Baoding_Youyongguan','Baoding_Jiancezhan')
for(i in 1:length(onames_update))
{
  stationdata=read.csv(paste0(path4,onames_update[i],"_NS.csv"),stringsAsFactors = F)
  stationdata=judgeN_new(stationdata,onames_update[i])
  
  write.csv(stationdata,file=paste0(path7,onames_update[i],"_NS.csv"))
  stationd=read.csv(paste0(path5,onames_update[i],"_cleaned.csv"),stringsAsFactors = F)
  stationd$av_PM2.5=stationd$avpm
  stationd$av_PM10=pollutant_smooth(stationd,'PM10')
  stationd$av_SO2=pollutant_smooth(stationd,'SO2')
  stationd$av_NO2=pollutant_smooth(stationd,'NO2')
  stationd$av_O3=pollutant_smooth(stationd,'O3')
  stationd$av_CO=pollutant_smooth(stationd,'CO')
  col_update=c("N","S","IN","IS","INws","ISws","epfmINws","epfpn","epfNen","epfpen","epfmINws48","epfpn48","epfNen48","epfpen48")
  id_col_update=which(colnames(stationd)%in%col_update)
  stationd=inner_join(stationd[,-id_col_update],stationdata[,c('X',col_update)],by=c('oX'='X'))
  
  write.csv(stationd,file=paste0(path8,onames_update[i],"_cleaned.csv"))
  print(onames_update[i])
}
#-------add BLD--------2021-02-18----------
onames_update=c('Beijing_Dongsi','Beijing_Tiantan','Beijing_Nongzhanguan',
                'Beijing_Guanyuan','Beijing_Wanliu','Beijing_Aotizhongxin',
                'Tangshan_Shierzhong','Tangshan_Wuziju','Tangshan_Leidazhan',
                'Baoding_Huadianerqu','Baoding_Youyongguan','Baoding_Jiancezhan')

year1=2013
year2=2019
#欧洲时间差8个小时，2013-01-01，2020-02-29
timerange_blh=1:(as.numeric(24*(as.Date(paste0(year2+1,'-03-01'))-as.Date(paste0(year1,'-01-01'))))-8)
station_pol_object=read.csv('/Users/zyr/Documents/PM2.5/datas/stations.csv',stringsAsFactors = F)
for(i in 1:length(onames_update))
{
  #bld
  lon_oname=station_pol_object$lon[station_pol_object$station==onames_update[i]]
  lat_oname=station_pol_object$lat[station_pol_object$station==onames_update[i]]
  if(length(lon_oname)!=1) print(onames_update[i])
  #2013-03-01-0h与2013-01-01-0h相差1416, 2013-03-01，2020-02-29
  bld_sp=c(rep(NA,8),blds[which.min(abs(longitudes_BLD-lon_oname)),which.min(abs(latitudes_BLD-lat_oname)),timerange_blh])[-(1:1416)]
  
  stationd=read.csv(paste0(path8,onames_update[i],"_cleaned.csv"),stringsAsFactors = F)
  print(stationd$oX[1]-1)
  print(length(which(is.na(bld_ad$bld))))
  
  bld_ad=data.frame(oX=(1:length(bld_sp))+stationd$oX[1]-1,bld=bld_sp)
  stationd=inner_join(stationd,bld_ad)
  
  write.csv(stationd,file=paste0(path8,onames_update[i],"_cleaned.csv"))
  print(onames_update[i])
}

