prefix=c('Beijing_','Beijing_','Tangshan_','Baoding_')
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin",
          "Shierzhong","Wuziju","Leidazhan",
          "Huadianerqu","Youyongguan","Jiancezhan")

csnames=matrix(csnames,ncol=3,byrow = T)
rnames=c("DongsiTiantanNongzhanguan","GuanyuanWanliuAotizhongxin","ShierzhongWuzijuLeidazhan","HuadianerquYouyongguanJiancezhan")

for(k in 1:nrow(csnames))
{
  sites=csnames[k,]
  csysn <-paste0(sites[1],sites[2],sites[3],rep(years,each=length(season)),rep(seasons,length(years)),'_cleaned.csv')
  sysepn<-paste0(rep(sites,length(years)*length(seasons)),rep(years,each=length(seasons)*length(sites)),
                 rep(rep(seasons,each=length(sites)),length(years)),'_ep')
  
  sitn <-paste0(prefix[k],sites)
  
  for(j in 1:length(sitn))
  {
    sit=sitn[j]
    data_site=read.csv(paste0(path8,sit,'_cleaned.csv'),stringsAsFactors = F)
    
    for(i in 1:length(csysn))
    {
      sysepname=sysepn[(i-1)*3+j]
      sys_ep=read.csv(paste0(pathsavee,sysepname,'.csv'),stringsAsFactors = F)
      sys_ep=inner_join(sys_ep,data_site[,c('oX','bld')])
      write.csv(sys_ep,file = paste0(pathsavee,sysepname,'.csv'))
      dataep_before=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_before/',sysepname,'_before.csv'),stringsAsFactors = F)
      if(length(nrow(dataep_before))>0){
        dataep_before=inner_join(dataep_before,data_site[,c('oX','bld')])
        write.csv(dataep_before,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_before/',sysepname,'_before.csv'))
      }
      dataep_middle=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_middle/',sysepname,'_middle.csv'),stringsAsFactors = F)
      if(length(nrow(dataep_middle))>0){
        dataep_middle=inner_join(dataep_middle,data_site[,c('oX','bld')])
        write.csv(dataep_middle,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_middle/',sysepname,'_middle.csv'))
      }
      dataep_after=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_after/',sysepname,'_after.csv'),stringsAsFactors = F)
      if(length(nrow(dataep_after))>0){
        dataep_after=inner_join(dataep_after,data_site[,c('oX','bld')])
        write.csv(dataep_after,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_after/',sysepname,'_after.csv'))
      }
      dat=read.csv(paste0(pathsaveem,sysepname,'m.csv'),stringsAsFactors = F)
      dat=inner_join(dat,data_site[,c('oX','bld')])
      write.csv(dat,file=paste0(pathsaveem,sysepname,'m.csv'))
    }
  }
  
  csysepn <-paste0(sites[1],sites[2],sites[3],rep(years,each=length(season)),rep(seasons,length(years)))
  for(i in 1:(length(season)*length(years)))
  {
    s1=read.csv(paste0(pathsavee,sysepn[(i-1)*3+1],'.csv'), header = TRUE)
    s2=read.csv(paste0(pathsavee,sysepn[(i-1)*3+2],'.csv'), header = TRUE)
    s2$ep_id=s2$ep_id+max(s1$ep_id)
    s3=read.csv(paste0(pathsavee,sysepn[(i-1)*3+3],'.csv'), header = TRUE)
    s3$ep_id=s3$ep_id+max(s2$ep_id)
    s123=rbind(s1,s2,s3)
    write.csv(s123,file=paste0(pathsavee,csysepn[i],'_ep.csv'))
  }
}
ysepn <-paste0(rep(years,length(seasons)),rep(seasons,each=length(years)))
pathmdatasave <- '/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/modeldata/' #存model data的地址

for (k in 1:length(rnames)) 
{
  for (i in 1:length(seasons)) 
  {
    for (j in 1:length(years)) 
    {
      a=read.csv(paste0(pathsavee,rnames[k],ysepn[(i-1)*length(years)+j],'_ep.csv'), stringsAsFactors = FALSE)
      sta=read.csv(paste0(pathsavees,rnames[k],ysepn[(i-1)*length(years)+j],'_epstat.csv'), stringsAsFactors = FALSE)
      a$epdpen=rep(sta$epdpen,times=sta$epl)
      a$epfmINws=rep(sta$epfmINws,times=sta$epl)
      a$epfpn=rep(sta$epfpn,times=sta$epl)
      a$epfNen=rep(sta$epfNen,times=sta$epl)
      sta$epfpen[which(is.na(sta$epfpen))]=0
      a$epfpen=rep(sta$epfpen,times=sta$epl)
      a$epfmINws48=rep(sta$epfmINws48,times=sta$epl)
      a$epfpn48=rep(sta$epfpn48,times=sta$epl)
      a$epfNen48=rep(sta$epfNen48,times=sta$epl)
      sta$epfpen48[which(is.na(sta$epfpen48))]=0
      a$epfpen48=rep(sta$epfpen48,times=sta$epl)
      a$pmf35=rep(sta$pmf35,times=sta$epl)
      a$type=rep(sta$type,times=sta$epl)
      
      write.csv(a,paste0(pathmdatasave,rnames[k],ysepn[(i-1)*length(years)+j],'_eptreat.csv'))
    }
  }
}

