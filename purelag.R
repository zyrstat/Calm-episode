install.packages("installr")
library(installr)
updateR()
install.packages("devtools")
library(devtools)
install_github("easyGgplot2","kassambara")
library(easyGgplot2)
library("cowplot")
library("gridExtra")
library("scales")
Sys.setlocale(category = "LC_ALL", locale = "italian")
#path7-->path4; path8-->path5; path9-->path6

tt=3;#长度超过3

# be_WSPM=5.5;be_Iws=7.9;be_INws=10.8;be_ISws=13.8;be_time=2;be_tn=7;be_ar=7#起点限制条件
# lagg=7#起点和上一条episode的间隔
# r_pm=0
# du_Iws=15.6;du_INws=3.4;du_ISws=15.6#;du_IS=20#中间点限制条件
# du_ld_prop=0.5;du_li_prop=0.5;du_pn=1;du_ps=0.1
# years=2013:2019

be_PM2.5=35;be_WSPM=5.5;be_INws=10.8;be_ISws=13.8;be_time=2;windowsize=8#起点限制条件
lagg=6#起点和上一条episode的间隔
r_pm=0
du_Iws=Inf;du_INws=3.4;du_ISws=13.9#;du_IS=20#中间点限制条件
du_ld_prop=0.5;du_li_prop=0.5;du_pn=1;du_ps=0.1
years=2013:2018

prefix=c('Beijing_','Beijing_','Baoding_','Tangshan_')
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin",
          "Huadianerqu","Youyongguan","Jiancezhan",
          "Shierzhong","Wuziju","Leidazhan")
csnames=matrix(csnames,ncol=3,byrow = T)

cleanstart_lag <- function(csysname,sit,be_PM2.5,bbb1)
{
  #  csysname <- 'agw2017xia.csv' #combinedsitesyearseason.csv输入
  #  sit <- "a"#输入站点
  csys <- read.csv(paste0(path9,csysname), header = TRUE)##agwyearseason,agw2017xia
  site=csys$loca
  sys=csys[which(site==sit),]#a2017xia
  sys$id=1:nrow(sys)
  cleanings=c()
  for(cc in 2:nrow(sys))
  {
    if(sys$ncof[cc]>0)
    {
      if(sys$INws[cc-1]>=be_INws&sys$INws[cc]==0)
        cleanings=c(cleanings,cc)
    }
  }
  #可能的起点
  #降水为0，avpm<=35，连续时长>=3,WSPM<2.4
  start_candidate=c()
  for(ss in 2:(nrow(sys)-tt))
  {
    if(sys$ncof[ss]>0&sys$ncob[ss]>=tt)
    {
      if(max(sys$RAIN[(ss-1):ss])==0&sys$WSPM[ss]<be_WSPM&max(sys$avpm[(ss-1):ss])<=be_PM2.5&
         sys$avpm[ss-1]<sys$avpm[ss]&sys$avpm[ss]<sys$avpm[ss+1])
        start_candidate=c(start_candidate,ss)
    }
  }
  
  begin_X=c()
  clean_X=c()
  
  for(j in 1:length(cleanings))
  {
    i=cleanings[j]
    clean_X=c(clean_X,sys$X[i])
    begin_X=c(begin_X,sys$X[start_candidate[which.min(abs(start_candidate-i))]])
  }
  remove=which(abs(begin_X-clean_X)>12)
  if(length(remove)>0)
  {
    clean_X=clean_X[-remove]
    begin_X=begin_X[-remove]
  }
  return(begin_X-clean_X)
}

datas_lag=c()
for(k in 1:nrow(csnames))
{
  sites=csnames[k,]
  if(prefix[k]=='Beijing_') 
    be_PM2.5=35
  if(prefix[k]%in% c('Tangshan_','Baoding_')) 
    be_PM2.5=50
  da_cs <-read.csv(paste0(path8,prefix[k],sites[1],sites[2],sites[3],'_cleaned.csv'),header = TRUE) #da_agw
  
  csysn <-paste0(sites[1],sites[2],sites[3],rep(years,each=length(season)),rep(seasons,length(years)),'_cleaned.csv')
  
  sites_mark=rep(sites,length(years)*length(seasons))
  years_mark=rep(years,each=length(seasons)*length(sites))
  seasons_mark=rep(rep(seasons,each=length(sites)),length(years))
  sysepn<-paste0(sites_mark,years_mark,seasons_mark)
  sNSn <-paste0(prefix[k],sites,'_NS.csv') 
  
  sitn <-paste0(prefix[k],sites)
  
  for(j in 1:length(sitn))
  {
    sit=sitn[j]
    site=da_cs$loca
    da_s=da_cs[which(site==sit),]#da_a
    daNS_s=read.csv(paste0(path7,sNSn[j]), header = TRUE)#daNS_a
    bbb=daNS_s$X[which(daNS_s$year==year1&daNS_s$month==3&daNS_s$day==1&daNS_s$hour==0)]
    bbb1=bbb-1
    for(i in 1:length(csysn))
    {
      csysname=csysn[i]
      lags=cleanstart_lag(csysname,sit,be_PM2.5,bbb1)
      data_lag=data.frame(site=sites_mark[(i-1)*3+j],year=years_mark[(i-1)*3+j],season=seasons_mark[(i-1)*3+j],lag=lags)
      datas_lag=rbind(datas_lag,data_lag)
      print(paste0(k,': ',j,' ',i,' ',sysepn[(i-1)*3+j]),is.na(data_lag$site)) 
    }
  }
}

datas_lag$sites=rep('',nrow(datas_lag))
datas_lag$sites[datas_lag$site%in% csnames[1,]]='DNT, Bejing'
datas_lag$sites[datas_lag$site%in% csnames[2,]]='AGW, Bejing'
datas_lag$sites[datas_lag$site%in% csnames[3,]]='LSW, Tangshan'
datas_lag$sites[datas_lag$site%in% csnames[4,]]='HJY, Baoding'
datas_lag$sites=factor(datas_lag$sites,levels=c('DNT, Bejing','AGW, Bejing','LSW, Tangshan','HJY, Baoding'),
                       labels=c('Beijing 1','Beijing 2','Tangshan','Baoding'))
datas_lag$season=factor(datas_lag$season,levels=c('spring','summer','autumn','winter'))
ggplot(datas_lag, aes(x=lag, color=season)) + 
  #geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.2)+ facet_wrap(.~ sites,ncol=4)+
  labs(x='Time lag', y='Density')+mytheme 

ggplot(datas_lag, aes(x=lag, color=season)) +
  geom_histogram(fill="white", position="dodge",bins=length(unique(datas_lag$lag)))+
  facet_wrap(.~ sites,ncol=4)+
  labs(x='Time lag', y='Count')+mytheme 

# ggplot(data=datas_lag, aes(x=lag, fill=season)) +
#   geom_bar(aes(x =lag, y = ..prop.., group = season), position=position_dodge())+
#   # geom_bar(stat="count", position=position_dodge())+
#   # scale_fill_manual(values=c('spring'='#CC0033','summer'='#FFCC33','autumn'='#009900','winter'='#006699'))+
#   scale_fill_manual(values=c('spring'='#9966CC','summer'='#CC0033','autumn'='#FFCC33','winter'='#006699'))+
#   facet_wrap(.~ sites,ncol=2)+
#   labs(x='Difference', y='Frequency')+scale_x_continuous(breaks=(c(-12,-8,-4,0,4,8,12)))+
#   mytheme+theme(legend.position = 'top') 
# 
# ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/puregap.png'),units="in",width=8, height=6, dpi=300)

quantile(datas_lag$lag,0.975)
quantile(datas_lag$lag,0.025)
quantile(datas_lag$lag,0.95)
quantile(datas_lag$lag,0.05)

for(ss in seasons){
  ggplot(data=datas_lag[datas_lag$season==ss,], aes(x=lag)) +
    geom_bar(aes(x =lag, y = ..prop.., group = 1), position=position_dodge())+
    # geom_bar(stat="count", position=position_dodge())+
    # scale_fill_manual(values=c('spring'='#CC0033','summer'='#FFCC33','autumn'='#009900','winter'='#006699'))+
    # scale_fill_manual(values=c('spring'='#9966CC','summer'='#CC0033','autumn'='#FFCC33','winter'='#006699'))+
    facet_wrap(.~ sites,ncol=4)+ylim(0,0.6)+
    labs(x='Difference', y='Frequency')+scale_x_continuous(breaks=(c(-12,-8,-4,0,4,8,12)))+
    mytheme+theme(legend.position = 'top') 
  
  ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/puregap',ss,'.png'),units="in",width=12, height=3, dpi=300)
  
}
