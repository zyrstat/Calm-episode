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

tt=3;#长度超过3

# be_PM2.5=35;be_WSPM=5.5;be_Iws=7.9;be_INws=10.8;be_ISws=13.8;be_time=2;be_tn=7;be_ar=7#起点限制条件
# lagg=7#起点和上一条episode的间隔
# r_pm=0
# du_Iws=15.6;du_INws=3.4;du_ISws=15.6#;du_IS=20#中间点限制条件
# du_ld_prop=0.5;du_li_prop=0.5;du_pn=1;du_ps=0.1
# years=2013:2019

be_PM2.5=35;be_WSPM=5.5;be_Iws=7.9;be_INws=10.8;be_ISws=13.8;be_time=2;be_tn=6;be_ar=6#起点限制条件
lagg=6#起点和上一条episode的间隔
r_pm=0
du_Iws=Inf;du_INws=3.4;du_ISws=13.9#;du_IS=20#中间点限制条件
du_ld_prop=0.5;du_li_prop=0.5;du_pn=1;du_ps=0.1
years=2013:2019

prefix='Beijing_'

if(prefix=='Beijing_') 
{be_PM2.5=35
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin")}
if(prefix=='Tangshan_') 
{be_PM2.5=50
csnames=c("Shierzhong","Wuziju","Leidazhan")}
if(prefix=='Baoding_')
{be_PM2.5=50
csnames=c("Huadianerqu","Youyongguan","Jiancezhan")}

csnames=matrix(csnames,ncol=3,byrow = T)
######选择episode

pathsavep <- '/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episode_plots_new/' #存图的地址
pathsavee <- '/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_new/'  #存episode的地址
pathsaveem <- '/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesmark_new/'  #存episodemark的地址
pathsavees <- '/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat_new/'  #存episodemark的地址

plotting_result=function(dat,sysepname){
  dats=dat
  dats$date=as.POSIXct(paste0(dats$year,'-',dats$month,'-',dats$day,' ',dats$hour,':00'))
  dats$rownum=1:nrow(dats)
  
  dat1=dats[,c("year","month","day","hour","X","season","PM2.5","INws","ISws","seperation","rownum")]
  datse=dats[,c("year","month","day","hour","X","season","PM10","INws","ISws","seperation","rownum")]
  datse$PM10=datse$PM10+50
  colnames(datse)[7]='PM2.5'
  dat1=rbind(dat1,datse)
  dat1$pollution=rep(c('PM2.5','PM10'),each=nrow(dats))
  
  dat2=dats[,c("year","month","day","hour","X","season","PM2.5","INws","ISws","seperation","rownum")]
  datse=dats[,c("year","month","day","hour","X","season","SO2","INws","ISws","seperation","rownum")]
  datse$SO2=datse$SO2*2+50
  colnames(datse)[7]='PM2.5'
  dat2=rbind(dat2,datse)
  dat2$pollution=rep(c('PM2.5','SO2'),each=nrow(dats))
  
  dat3=dats[,c("year","month","day","hour","X","season","PM2.5","INws","ISws","seperation","rownum")]
  datse=dats[,c("year","month","day","hour","X","season","NO2","INws","ISws","seperation","rownum")]
  datse$NO2=datse$NO2*3+50
  colnames(datse)[7]='PM2.5'
  dat3=rbind(dat3,datse)
  dat3$pollution=rep(c('PM2.5','NO2'),each=nrow(dats))
  
  dat4=dats[,c("year","month","day","hour","X","season","PM2.5","INws","ISws","seperation","rownum")]
  datse=dats[,c("year","month","day","hour","X","season","CO","INws","ISws","seperation","rownum")]
  datse$CO=datse$CO/11+50
  colnames(datse)[7]='PM2.5'
  dat4=rbind(dat4,datse)
  dat4$pollution=rep(c('PM2.5','CO'),each=nrow(dats))
  
  dat5=dats[,c("year","month","day","hour","X","season","PM2.5","INws","ISws","seperation","rownum")]
  datse=dats[,c("year","month","day","hour","X","season","O3","INws","ISws","seperation","rownum")]
  datse$O3=datse$O3*2+50
  colnames(datse)[7]='PM2.5'
  dat5=rbind(dat5,datse)
  dat5$pollution=rep(c('PM2.5','O3'),each=nrow(dats))
  
  j=ceiling(nrow(dats)/672)
  
  datses=list(dat1,dat2,dat3,dat4,dat5)
  for(w in 1:5)
  {
    datses[[w]]=arrange(datses[[w]],rownum,pollution)
    datses[[w]]$w_direction='Missing'
    datses[[w]]$w_direction[datses[[w]]$ISws>0]="S"
    datses[[w]]$w_direction[datses[[w]]$INws>0]="N"
    datses[[w]]$INSws=datses[[w]]$INws+datses[[w]]$ISws
    datses[[w]]$date=as.POSIXct(paste0(datses[[w]]$year,'-',datses[[w]]$month,'-',datses[[w]]$day,' ',datses[[w]]$hour,':00'))
    datses[[w]]$seperation_pollution=paste0(datses[[w]]$seperation,'-',datses[[w]]$pollution)
    
    plots=c()
    for(i in 1:(j-1))
    {
      plots[[i]]<-ggplot(data=datses[[w]][(672*2*(i-1)+1):min(672*2*i,nrow(datses[[w]])),],aes(x=date,y=PM2.5,color=seperation_pollution,group=pollution))+
        geom_line(size=0.8,show.legend = FALSE)+
        #scale_x_datetime(date_breaks ="1 day",date_labels ="%Y-%m-%d %H:%M")+
        scale_x_datetime(date_breaks ="1 day",date_labels ="%y-%m-%d")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 30,hjust = 1),axis.text = element_text(face='bold'))+
        geom_hline(aes(yintercept=35),linetype = 'dashed')+
        geom_line(data=datses[[w]][(672*2*(i-1)+1):min(672*2*i,nrow(datses[[w]])),],aes(x=date,y=INSws-100,color=w_direction),size=0.9,show.legend = T)+
        geom_hline(aes(yintercept=be_INws-100),linetype = 'dashed')+
        ggtitle(sysepname)
    }
    plots[[j]]<-ggplot(data=datses[[w]][(672*2*(j-1)+1):min(672*2*j,nrow(datses[[w]])),],aes(x=date,y=PM2.5,color=seperation_pollution,group=pollution))+
      geom_line(size=0.8,show.legend = FALSE)+
      #scale_x_datetime(date_breaks ="1 day",date_labels ="%Y-%m-%d %H:%M")+
      scale_x_datetime(date_breaks ="1 day",date_labels ="%y-%m-%d",limits =c(dats$date[672*(j-1)+1],dats$date[672*(j-1)+1]+671*3600))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 30,hjust = 1),axis.text = element_text(face='bold'))+
      geom_hline(aes(yintercept=35),linetype = 'dashed')+
      geom_line(data=datses[[w]][(672*2*(j-1)+1):min(672*2*j,nrow(datses[[w]])),],aes(x=date,y=INSws-100,color=w_direction),size=0.9,show.legend = T)+
      geom_hline(aes(yintercept=be_INws-100),linetype = 'dashed')
    ggtitle(sysepname)
    
    print(plot_grid(plotlist = plots,ncol=1))
  }
}

choosep <- function(csysname,sysepname,sit,n12,bbb1)
{
  n1=n12[1]
  n2=n12[2]
  n3=n12[3]
  #  csysname <- 'agw2017xia.csv' #combinedsitesyearseason.csv输入
  #  sysepname <- 'a2017xia_ep'#输入
  #  sit <- "a"#输入站点
  csys <- read.csv(paste0(path9,csysname), header = TRUE)##agwyearseason,agw2017xia
  site=csys$loca
  sys=csys[which(site==sit),]#a2017xia
  sys$id=1:nrow(sys)
  #可能的起点
  #降水为0，avpm<=35，连续时长>=3,WSPM<2.4
  pb <- filter(sys, RAIN==0,avpm<=be_PM2.5,ncob>=tt, WSPM<be_WSPM,id<nrow(sys)-tt+1)
  nr=nrow(pb)
  #pb的行号
  hang=c()#可能的起点在sys中的行号
  for(i in 1:nr)
  {
    j=pb$id[i]#sys的行号
    x=sys$X[j]
    ncof=sys$ncof[j]
    if(all(da_s$avpm[(which(da_s$X==x)-min(1,ncof)):which(da_s$X==x)]<=be_PM2.5)&
       daNS_s$ncfir0[which(daNS_s$X==x+bbb1)]>=be_time)
      hang=c(hang,j)
  }
  
  begin=c()#sys的行号
  en=c()#sys的行号
  epfmINws=c()#episode前24小时内最大INws
  epfpn=c()#episode前24小时内北风占比
  epfNen=c()#episode前24小时内北风的累积风速
  epfpen=c()#episode前24小时内北风的累积风速占比
  epfmINws48=c()#episode前48小时内最大INws
  epfpn48=c()#episode前48小时内北风占比
  epfNen48=c()#episode前48小时内北风的累积风速
  epfpen48=c()#episode前48小时内北风的累积风速占比
  
  type=c()#episode类型
  pmf35=c()#episode前多少个连续点PM2.5在35以下
  epl=c()#episode长度
  epr=c()#episodePM2.5极差
  eprPM10=c()#episodePM10极差
  eprSO2=c()#episodeSO2极差
  eprNO2=c()#episodeNO2极差
  eprCO=c()#episodeCO极差
  eprO3=c()#episodeO3极差
  epsp=c()#episode平均增长速度
  epmW=c()#episode平均WSPM
  epmp=c()#episode平均PM2.5
  epbp=c()#episode起点PM2.5
  epmTE=c()#episode平均TEMP
  epmDE=c()#episode平均DEWP
  epmHU=c()#episode平均HUMI
  epmPR=c()#episode平均PRES
  epmbl=c()#episode平均blh
  epdpen=c()#episode中北风的能量
  
  for(j in 1:length(hang))
  {
    i=hang[j]
    X=sys$X[i]
    if(i>max(en))
    {
      if(sys$INws[i]>=be_INws)
      {
        if(min(sys$INws[(i+1):min(i+be_tn,i+sys$ncob[i],nrow(sys))])==0)
        {
          y=which.min(sys$INws[(i+1):min(i+be_tn,i+sys$ncob[i],nrow(sys))])#i+y处为0
          inter=intersect(i:(i+y+be_ar),hang)
          mm=inter[max(which(sys$PM2.5[inter]==min(sys$PM2.5[inter])))]
          #mm=inter[max(which(sys$avpm[inter]==min(sys$avpm[inter])))]
          if(mm<i+y)
          {
            if(all(sys$RAIN[mm:(i+y-1)]==0))#&##all(sys$WSPM[(i+1):(i+y)]<=du_WSPM)&
            {
              k=i+y-1
              be=mm
              # k=i+y
              # be=i
              while(k<nrow(sys)&k+1-be<=sys$ncob[be]&sys$RAIN[k+1]==0 & sys$Iws[k+1]<du_Iws&sys$INws[k+1]<du_INws&sys$ISws[k+1]<du_ISws)#&sys$IS[k+1]<=du_IS)
                #不下雨，du_WSPM=3.3;du_Iws=7;du_INws=3;du_ISws=7.5;du_IS=8  ;&sys$WSPM[k+1]<=du_WSPM
                k=k+1
              len_po=k-be+1 
              if(len_po>tt)#时长超过3h
              {
                #              be=be-1+which.min(sys$PM2.5[be:k])
                k=be+max(which(sys$PM2.5[(be+1):k]==max(sys$PM2.5[(be+1):k])))
                #k=be+max(which(sys$avpm[(be+1):k]==max(sys$avpm[(be+1):k])))
                len_po=k-be+1
                if(len_po>tt&be>max(en)+lagg&sys$ncof[be]>0&sys$ncob[k]>0)
                {
                  
                  sn <- sum(sys$IN[be:k]>0)
                  pn <- sn/len_po#北风时长占比
                  #if(sys$PM2.5[be]<sys$PM2.5[k]&ld_prop<du_ld_prop & li_prop>du_li_prop& pn<=du_pn)
                  if(sys$PM2.5[be]+r_pm<sys$PM2.5[k]& pn<du_pn)
                  {
                    
                    judg=sys$pm35[be]>0
                    if(!judg&be>1)
                      judg=sys$avpm[be-1]<=be_PM2.5
                    #if(judg&(max(sys$PM2.5[be:k])-min(sys$PM2.5[be:k]))/(len_po-1)>=1)
                    if(judg)
                    {
                      
                      epfmINws=c(epfmINws,sys$epfmINws[be])
                      epfpn=c(epfpn,sys$epfpn[be])
                      epfNen=c(epfNen,sys$epfNen[be])
                      epfpen=c(epfpen,sys$epfpen[be])
                      pmf35=c(pmf35,sys$pm35[be])
                      epfmINws48=c(epfmINws48,sys$epfmINws48[be])
                      epfpn48=c(epfpn48,sys$epfpn48[be])
                      epfNen48=c(epfNen48,sys$epfNen48[be])
                      epfpen48=c(epfpen48,sys$epfpen48[be])
                      
                      type=c(type,1)
                      
                      epl=c(epl,len_po)
                      epr=c(epr,max(sys$PM2.5[be:k])-min(sys$PM2.5[be:k]))
                      eprP=sys$PM10[be:k]
                      eprP=eprP[!is.na(eprP)]
                      if(length(eprP)==0)
                        eprP=0
                      else
                        eprP=max(eprP)-min(eprP)
                      eprPM10=c(eprPM10,eprP)
                      eprS=sys$SO2[be:k]
                      eprS=eprS[!is.na(eprS)]
                      if(length(eprS)==0)
                        eprS=0
                      else
                        eprS=max(eprS)-min(eprS)
                      eprSO2=c(eprSO2,eprS)
                      eprN=sys$NO2[be:k]
                      eprN=eprN[!is.na(eprN)]
                      if(length(eprN)==0)
                        eprN=0
                      else
                        eprN=max(eprN)-min(eprN)
                      eprNO2=c(eprNO2,eprN)
                      eprC=sys$CO[be:k]
                      eprC=eprC[!is.na(eprC)]
                      if(length(eprC)==0)
                        eprC=0
                      else
                        eprC=max(eprC)-min(eprC)
                      eprCO=c(eprCO,eprC)
                      eprO=sys$O3[be:k]
                      eprO=eprO[!is.na(eprO)]
                      if(length(eprO)==0)
                        eprO=0
                      else
                        eprO=max(eprO)-min(eprO)
                      eprO3=c(eprO3,eprO)
                      
                      epsp=c(epsp,epr[length(epr)]/(len_po-1))
                      epmW=c(epmW,mean(sys$WSPM[be:k]))
                      epmp=c(epmp,mean(sys$PM2.5[be:k]))
                      epbp=c(epbp,sys$PM2.5[be])
                      epmTE=c(epmTE,mean(sys$TEMP[be:k]))
                      epmDE=c(epmDE,mean(sys$DEWP[be:k]))
                      epmHU=c(epmHU,mean(sys$HUMI[be:k]))
                      epmPR=c(epmPR,mean(sys$PRES[be:k]))
                      epmbl=c(epmbl,mean(sys$blh[be:k]))
                      interval=sys[be:k,]
                      if(sum(interval$WSPM)==0)
                        epdpen=c(epdpen,0)
                      else
                        epdpen=c(epdpen,sum(interval$WSPM[which(interval$IN>0)])/sum(interval$WSPM))
                      
                      begin=c(begin,be)
                      en=c(en,k)
                      n1=n1+1}
                  }
                }
                
              }
              
            }
          }
          #else if(mm>=i+y&sys$INws[mm]<be_INws)
          else if(mm>=i+y)
          {
            if(sys$Iws[mm]<=be_Iws)#&max(sys$ISws[(i+y):mm])<3.4)
            {
              be=mm#可能的起点
              k=mm
              while(k<nrow(sys)&k+1-be<=sys$ncob[be] &sys$RAIN[k+1]==0 & sys$Iws[k+1]<du_Iws&sys$INws[k+1]<du_INws&sys$ISws[k+1]<du_ISws)#&sys$IS[k+1]<=du_IS)
                #不下雨，du_WSPM=3.3;du_Iws=7;du_INws=3;du_ISws=7.5;du_IS=8  ;&sys$WSPM[k+1]<=du_WSPM
                k=k+1
              len_po=k-be+1 
              if(len_po>tt)#时长超过3h
              {
                #              be=be-1+which.min(sys$PM2.5[be:k])
                k=be+max(which(sys$PM2.5[(be+1):k]==max(sys$PM2.5[(be+1):k])))
                len_po=k-be+1
                if(len_po>tt&be>max(en)+lagg&sys$ncof[be]>0&sys$ncob[k]>0)
                {
                  
                  sn <- sum(sys$IN[be:k]>0)
                  pn <- sn/len_po#北风时长占比
                  #if(sys$PM2.5[be]<sys$PM2.5[k]&ld_prop<du_ld_prop & li_prop>du_li_prop& pn<=du_pn)
                  if(sys$PM2.5[be]+r_pm<sys$PM2.5[k]& pn<du_pn)
                  {
                    
                    judg=sys$pm35[be]>0
                    if(!judg&be>1)
                      judg=sys$avpm[be-1]<=be_PM2.5
                    #if(judg&(max(sys$PM2.5[be:k])-min(sys$PM2.5[be:k]))/(len_po-1)>=1)
                    if(judg)
                    {
                      
                      epfmINws=c(epfmINws,sys$epfmINws[be])
                      epfpn=c(epfpn,sys$epfpn[be])
                      epfNen=c(epfNen,sys$epfNen[be])
                      epfpen=c(epfpen,sys$epfpen[be])
                      pmf35=c(pmf35,sys$pm35[be])
                      epfmINws48=c(epfmINws48,sys$epfmINws48[be])
                      epfpn48=c(epfpn48,sys$epfpn48[be])
                      epfNen48=c(epfNen48,sys$epfNen48[be])
                      epfpen48=c(epfpen48,sys$epfpen48[be])
                      
                      type=c(type,2)
                      
                      epl=c(epl,len_po)
                      epr=c(epr,max(sys$PM2.5[be:k])-min(sys$PM2.5[be:k]))
                      eprP=sys$PM10[be:k]
                      eprP=eprP[!is.na(eprP)]
                      if(length(eprP)==0)
                        eprP=0
                      else
                        eprP=max(eprP)-min(eprP)
                      eprPM10=c(eprPM10,eprP)
                      eprS=sys$SO2[be:k]
                      eprS=eprS[!is.na(eprS)]
                      if(length(eprS)==0)
                        eprS=0
                      else
                        eprS=max(eprS)-min(eprS)
                      eprSO2=c(eprSO2,eprS)
                      eprN=sys$NO2[be:k]
                      eprN=eprN[!is.na(eprN)]
                      if(length(eprN)==0)
                        eprN=0
                      else
                        eprN=max(eprN)-min(eprN)
                      eprNO2=c(eprNO2,eprN)
                      eprC=sys$CO[be:k]
                      eprC=eprC[!is.na(eprC)]
                      if(length(eprC)==0)
                        eprC=0
                      else
                        eprC=max(eprC)-min(eprC)
                      eprCO=c(eprCO,eprC)
                      eprO=sys$O3[be:k]
                      eprO=eprO[!is.na(eprO)]
                      if(length(eprO)==0)
                        eprO=0
                      else
                        eprO=max(eprO)-min(eprO)
                      eprO3=c(eprO3,eprO)
                      
                      epsp=c(epsp,epr[length(epr)]/(len_po-1))
                      epmW=c(epmW,mean(sys$WSPM[be:k]))
                      epmp=c(epmp,mean(sys$PM2.5[be:k]))
                      epbp=c(epbp,sys$PM2.5[be])
                      epmTE=c(epmTE,mean(sys$TEMP[be:k]))
                      epmDE=c(epmDE,mean(sys$DEWP[be:k]))
                      epmHU=c(epmHU,mean(sys$HUMI[be:k]))
                      epmPR=c(epmPR,mean(sys$PRES[be:k]))
                      epmbl=c(epmbl,mean(sys$blh[be:k]))
                      interval=sys[be:k,]
                      if(sum(interval$WSPM)==0)
                        epdpen=c(epdpen,0)
                      else
                        epdpen=c(epdpen,sum(interval$WSPM[which(interval$IN>0)])/sum(interval$WSPM))
                      
                      begin=c(begin,be)
                      en=c(en,k)
                      n2=n2+1}
                  }
                }
                
              }
              
            }
            
          }  
          
        }
      }
      #else if(sys$INws[i]<be_INws&sys$Iws[i]<=be_Iws)
      else if(sys$Iws[i]<=be_Iws)
      {
        a=daNS_s[(which(daNS_s$X==X+bbb1)-24):(which(daNS_s$X==X+bbb1)-1),]
        if(max(a$INws)>=be_INws&max(a$ISws[which.max(a$INws):nrow(a)])<=be_ISws)
        {
          be=i#可能的起点
          k=i
          while(k<nrow(sys)&k+1-i<=sys$ncob[i] &sys$RAIN[k+1]==0 & sys$Iws[k+1]<du_Iws&sys$INws[k+1]<du_INws&sys$ISws[k+1]<du_ISws)#&sys$IS[k+1]<=du_IS)
            #不下雨，du_WSPM=3.3;du_Iws=7;du_INws=3;du_ISws=7.5;du_IS=8  ;&sys$WSPM[k+1]<=du_WSPM
            k=k+1
          len_po=k-be+1
          if(len_po>tt)#时长超过3h
          {
            be=be-1+which.min(sys$PM2.5[be:k])
            k=be+max(which(sys$PM2.5[(be+1):k]==max(sys$PM2.5[(be+1):k])))
            len_po=k-be+1
            if(len_po>tt&be>max(en)+lagg&sys$ncof[be]>0&sys$ncob[k]>0)
            {
              
              sn <- sum(sys$IN[be:k]>0)
              pn <- sn/len_po#北风时长占比
              #if(sys$PM2.5[be]<sys$PM2.5[k]&ld_prop<du_ld_prop & li_prop>du_li_prop& pn<=du_pn)
              if(sys$PM2.5[be]+r_pm<sys$PM2.5[k]& pn<du_pn)
              {
                
                judg=sys$pm35[be]>0
                if(!judg&be>1)
                  judg=sys$avpm[be-1]<=be_PM2.5
                #if(judg&(max(sys$PM2.5[be:k])-min(sys$PM2.5[be:k]))/(len_po-1)>=1)
                if(judg)
                {
                  
                  epfmINws=c(epfmINws,sys$epfmINws[be])
                  epfpn=c(epfpn,sys$epfpn[be])
                  epfNen=c(epfNen,sys$epfNen[be])
                  epfpen=c(epfpen,sys$epfpen[be])
                  pmf35=c(pmf35,sys$pm35[be])
                  epfmINws48=c(epfmINws48,sys$epfmINws48[be])
                  epfpn48=c(epfpn48,sys$epfpn48[be])
                  epfNen48=c(epfNen48,sys$epfNen48[be])
                  epfpen48=c(epfpen48,sys$epfpen48[be])
                  
                  type=c(type,3)
                  
                  epl=c(epl,len_po)
                  epr=c(epr,max(sys$PM2.5[be:k])-min(sys$PM2.5[be:k]))
                  eprP=sys$PM10[be:k]
                  eprP=eprP[!is.na(eprP)]
                  if(length(eprP)==0)
                    eprP=0
                  else
                    eprP=max(eprP)-min(eprP)
                  eprPM10=c(eprPM10,eprP)
                  eprS=sys$SO2[be:k]
                  eprS=eprS[!is.na(eprS)]
                  if(length(eprS)==0)
                    eprS=0
                  else
                    eprS=max(eprS)-min(eprS)
                  eprSO2=c(eprSO2,eprS)
                  eprN=sys$NO2[be:k]
                  eprN=eprN[!is.na(eprN)]
                  if(length(eprN)==0)
                    eprN=0
                  else
                    eprN=max(eprN)-min(eprN)
                  eprNO2=c(eprNO2,eprN)
                  eprC=sys$CO[be:k]
                  eprC=eprC[!is.na(eprC)]
                  if(length(eprC)==0)
                    eprC=0
                  else
                    eprC=max(eprC)-min(eprC)
                  eprCO=c(eprCO,eprC)
                  eprO=sys$O3[be:k]
                  eprO=eprO[!is.na(eprO)]
                  if(length(eprO)==0)
                    eprO=0
                  else
                    eprO=max(eprO)-min(eprO)
                  eprO3=c(eprO3,eprO)
                  
                  epsp=c(epsp,epr[length(epr)]/(len_po-1))
                  epmW=c(epmW,mean(sys$WSPM[be:k]))
                  epmp=c(epmp,mean(sys$PM2.5[be:k]))
                  epbp=c(epbp,sys$PM2.5[be])
                  epmTE=c(epmTE,mean(sys$TEMP[be:k]))
                  epmDE=c(epmDE,mean(sys$DEWP[be:k]))
                  epmHU=c(epmHU,mean(sys$HUMI[be:k]))
                  epmPR=c(epmPR,mean(sys$PRES[be:k]))
                  epmbl=c(epmbl,mean(sys$blh[be:k]))
                  interval=sys[be:k,]
                  if(sum(interval$WSPM)==0)
                    epdpen=c(epdpen,0)
                  else
                    epdpen=c(epdpen,sum(interval$WSPM[which(interval$IN>0)])/sum(interval$WSPM))
                  
                  begin=c(begin,be)
                  en=c(en,k)
                  n3=n3+1}
              }
            }
            
          }
          
        }
        
      }
    }
  }
  
  ep <- cbind(begin,en,type,epl,epr,eprPM10,eprSO2,eprNO2,eprCO,eprO3,epsp,epmp,epbp,epmW,epmTE,epmDE,
              epmHU,epmPR,epmbl,epdpen,epfmINws,epfpn,epfNen,epfpen,epfmINws48,epfpn48,epfNen48,epfpen48,pmf35)
  write.csv(ep,file=paste0(pathsavees,sysepname,'stat.csv'))
  
  len_ep=ep[,4]
  
  sec=c()#行号
  orde=c()#在一个episode中的序号
  ep_id=c()#第几个episode
  sec_before=c()
  orde_before=c()
  ep_id_before=c()
  sec_after=c()
  orde_after=c()
  ep_id_after=c()
  
  for(j in 1:nrow(ep))
  {
    sect=ep[j,1]:ep[j,2]
    sec=c(sec,sect)
    orde=c(orde,1:len_ep[j])
    ep_id=c(ep_id,rep(j,len_ep[j]))
    
    lag_ear=min(10,sys$ncof[ep[j,1]])
    beg=which(da_s$X==sys$X[ep[j,1]]-lag_ear)
    lag_lat=min(10,sys$ncob[ep[j,2]])
    enn=which(da_s$X==sys$X[ep[j,2]]+lag_lat)
    sec_before=c(sec_before,beg:(beg+lag_ear-1))
    orde_before=c(orde_before,1:lag_ear)
    ep_id_before=c(ep_id_before,rep(j,lag_ear))
    sec_after=c(sec_after,(enn-lag_lat+1):enn)
    orde_after=c(orde_after,1:lag_lat)
    ep_id_after=c(ep_id_after,rep(j,lag_lat))
  }
  
  sys_ep_before=cbind(da_s[sec_before,],ep_id_before)
  sys_ep_before=cbind(sys_ep_before,orde_before)
  write.csv(sys_ep_before,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_before_new/',sysepname,'_before.csv'))
  
  sys_ep_after=cbind(da_s[sec_after,],ep_id_after)
  sys_ep_after=cbind(sys_ep_after,orde_after)
  write.csv(sys_ep_after,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_after_new/',sysepname,'_after.csv'))
  
  sys_ep=cbind(sys[sec,-dim(sys)[2]],ep_id)
  sys_ep=cbind(sys_ep,orde)
  write.csv(sys_ep,file=paste0(pathsavee,sysepname,'.csv'))
  
  pdf(file= paste0(pathsavep,sysepname,'isode.pdf'),height = 20, width = 20,family = 'GB1')
  par(mfrow=c(1,1))
  boxplot(len_ep,range = T);title(paste0('Length of ',nrow(ep),' episodes of ',sysepname))
  for(j in 1:nrow(ep))
  {
    lag_ear=min(10,sys$ncof[ep[j,1]])
    beg=which(da_s$X==sys$X[ep[j,1]]-lag_ear)
    lag_lat=min(10,sys$ncob[ep[j,2]])
    enn=which(da_s$X==sys$X[ep[j,2]]+lag_lat)
    sect=beg:enn
    
    par(mfrow=c(5,2))
    plot(da_s$X[sect],da_s$PM2.5[sect],type = 'o',main="PM2.5",xlab='X',ylab ='PM2.5');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=be_PM2.5,col="red")
    plot(da_s$X[sect],da_s$Iws[sect],main="Iws",xlab='X',ylab ='Iws');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=be_Iws,col="red");abline(h=du_Iws,col="green")
    plot(da_s$X[sect],da_s$INws[sect],main="INws",xlab='X',ylab ='INws');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=be_Iws,col="red");abline(h=du_INws,col="green")
    plot(da_s$X[sect],da_s$ISws[sect],main="ISws",xlab='X',ylab ='ISws');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=du_ISws,col="green")
    plot(da_s$X[sect],da_s$massN[sect],main="massN",xlab='X',ylab ='massN');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=0.5,col="green")
    plot(da_s$X[sect],da_s$massS[sect],main="massS",xlab='X',ylab ='massS');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=0.5,col="green")
    plot(da_s$X[sect],da_s$massE[sect],main="massE",xlab='X',ylab ='massE');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=0.5,col="green")
    plot(da_s$X[sect],da_s$massW[sect],main="massW",xlab='X',ylab ='massW');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=0.5,col="green")
    plot(da_s$X[sect],da_s$IIwu[sect],main="IIwu",xlab='X',ylab ='IIwu');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=0,col="green")
    plot(da_s$X[sect],da_s$IIwv[sect],main="IIwv",xlab='X',ylab ='IIwv');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=0,col="green")
  }
  
  dat=sys
  dat$seperation=rep('nonepisode',nrow(dat))
  dat$seperation[sec]='episode'
  
  dat$rpm2=rep(-0.1,nrow(dat))
  nn=intersect(which(!is.na(dat$PM2.5)),which(dat$CO!=0))
  dat$rpm2[nn]=dat$PM2.5[nn]/dat$CO[nn]
  dat$rpm1=rep(-0.1,nrow(dat))
  nn=intersect(which(!is.na(dat$PM10)),which(dat$CO!=0))
  dat$rpm1[nn]=dat$PM10[nn]/dat$CO[nn]
  dat$rS=rep(-0.1,nrow(dat))
  nn=intersect(which(!is.na(dat$SO2)),which(dat$CO!=0))
  dat$rS[nn]=dat$SO2[nn]/dat$CO[nn]
  dat$rN=rep(-0.1,nrow(dat))
  nn=intersect(which(!is.na(dat$NO2)),which(dat$CO!=0))
  dat$rN[nn]=dat$NO2[nn]/dat$CO[nn]
  dat$rO=rep(-0.1,nrow(dat))
  nn=intersect(which(!is.na(dat$O3)),which(dat$CO!=0))
  dat$rO[nn]=dat$O3[nn]/dat$CO[nn]
  
  write.csv(dat,file=paste0(pathsaveem,sysepname,'m.csv'))
  print(plotting_result(dat,sysepname))
  
  dev.off()
  
  return(c(n1,n2,n3))
}
#select episodes
n123=c()
rnames=c()
pollulength=c()
#episodes <- function(x,path7,path8,pathsavep,pathsavee,pathsaveem,pathsavees,year,season,seasons,year1)
for(k in 1:nrow(csnames))
{
  sites=csnames[k,]
  rnames=c(rnames,paste0(sites[1],sites[2],sites[3]))
  n12=c(0,0,0)
  da_cs <-read.csv(paste0(path8,prefix,sites[1],sites[2],sites[3],'_cleaned.csv'),header = TRUE) #da_agw
  
  csysn <-paste0(sites[1],sites[2],sites[3],rep(years,each=length(season)),rep(seasons,length(years)),'_cleaned.csv')
  sysepn<-paste0(rep(sites,length(years)*length(season)),rep(years,each=length(season)*length(sites)),
                 rep(rep(seasons,each=length(sites)),length(years)),'_ep')
  
  sNSn <-paste0(prefix,sites,'_NS.csv') 
  
  sitn <-paste0(prefix,sites)
  
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
      sysepname=sysepn[(i-1)*3+j]
      n12=choosep(csysname,sysepname,sit,n12,bbb1)
      print(paste0(k,': ',j,' ',i,' ',sysepname)) 
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
    
    s11=read.csv(paste0(pathsavees,sysepn[(i-1)*3+1],'stat.csv'), header = TRUE)
    s11$loca=rep(sites[1],nrow(s11))
    s12=read.csv(paste0(pathsavees,sysepn[(i-1)*3+2],'stat.csv'), header = TRUE)
    s12$loca=rep(sites[2],nrow(s12))
    s13=read.csv(paste0(pathsavees,sysepn[(i-1)*3+3],'stat.csv'), header = TRUE)
    s13$loca=rep(sites[3],nrow(s13))
    s1123=rbind(s11,s12,s13)
    pollulength=rbind(pollulength,
                      apply(s1123[,c('epr','eprPM10','eprSO2','eprNO2','eprCO','eprO3')],2,mean))
    write.csv(s1123,file=paste0(pathsavees,csysepn[i],'_epstat.csv'))
  }
  n123=rbind(n123,n12)
}
rownames(n123)=rnames
colnames(n123)=c('type1','type2','type3')
n123=as.data.frame(n123)
n123$total=apply(n123,1,sum)
#--------------------------------------------------------------
ysepn <-paste0(rep(years,length(seasons)),rep(seasons,each=length(years)))
pathmdatasave <- '/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/modeldata_new/' #存model data的地址

Miss=c()
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
      #污染物缺失率
      pomiss=apply(is.na(a[,c('PM2.5','PM10','SO2','NO2','CO','O3')]),2,sum)
      Miss=rbind(Miss,c(pomiss,nrow(a),pomiss/nrow(a)))
      
      write.csv(a,paste0(pathmdatasave,rnames[k],ysepn[(i-1)*length(years)+j],'_eptreat.csv'))
    }
  }
}
rownames(Miss)=paste0(rep(rnames,each=length(ysepn)),rep(ysepn,length(rnames)))
#----------------------------------------
allnames=rownames(Miss)
berange=7:18#range of the beginning of episodes 
nrs=0
nep=0
for(m in 1:length(allnames))
{
  ep=read.csv(paste0(pathsavee,allnames[m],'_ep.csv'), stringsAsFactors = FALSE)
  ep1=ep[ep$orde==1,c('ep_id','orde','hour')]
  id_period=ep1$ep_id[which(ep1$hour %in% (berange))]
  ep=ep[which(ep$ep_id%in%id_period),]
  nrs=nrs+nrow(ep)
  nep=nep+length(unique(ep$ep_id))
}
mepl=round(nrs/nep,0)#平均长度
write.csv(as.data.frame(Miss),paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/missingrate/',prefix,'episodemissing.csv'))
