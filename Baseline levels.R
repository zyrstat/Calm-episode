library(easyGgplot2)
library("cowplot")
library("gridExtra")
library("scales")
Sys.setlocale(category = "LC_ALL", locale = "italian")

tt=3
be_PM2.5=35;be_WSPM=5.5;be_INws=10.8;be_ISws=13.8;be_time=2;windowsize=6#起点限制条件
lagg=0#起点和上一条episode的间隔
r_pm=0
du_Iws=Inf;du_INws=3.4;du_ISws=13.9#;du_IS=20#中间点限制条件
du_ld_prop=0.5;du_li_prop=0.5;du_pn=1;du_ps=0.1
years=2013:2018
season=c(1,2,3,4)
seasons=c('spring','summer','autumn','winter')

prefix='Baoding_'

if(prefix=='Tangshan_') 
{be_PM2.5=40
csnames=c("Shierzhong","Wuziju","Leidazhan")}
if(prefix=='Baoding_')
{be_PM2.5=40
csnames=c("Huadianerqu","Youyongguan","Jiancezhan")}

csnames=matrix(csnames,ncol=3,byrow = T)
######选择episode

pathsavep <- paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episode_plots_',be_PM2.5,'/') #存图的地址
pathsavee <- paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_',be_PM2.5,'/')  #存episode的地址
pathsaveem <- paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesmark_',be_PM2.5,'/')   #存episodemark的地址
pathsavees <- paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat_',be_PM2.5,'/')   #存episodemark的地址

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

episode_selection <- function(csysname,sysepname,sit,num,bbb1,windowsize,data_site)
{
  #  csysname <- 'agw2017xia.csv' #combinedsitesyearseason.csv输入
  #  sysepname <- 'a2017xia_ep'#输入
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
  for(ss in 1:(nrow(sys)-tt))
  {
    count_f=min(1,sys$ncof[ss])
    if(max(sys$RAIN[(ss-count_f):ss])==0&sys$WSPM[ss]<be_WSPM&max(sys$avpm[(ss-count_f):ss])<=be_PM2.5&sys$ncob[ss]>=tt)
      start_candidate=c(start_candidate,ss)
  }
  
  begin=c()#sys的行号
  en=c()#sys的行号
  begin_hour=c()
  en_hour=c()
  clean_hour=c()
  begin_X=c()
  en_X=c()
  clean_X=c()
  epfmINws=c()#episode前24小时内最大INws
  epfpn=c()#episode前24小时内北风占比
  epfNen=c()#episode前24小时内北风的累积风速
  epfpen=c()#episode前24小时内北风的累积风速占比
  epfmINws48=c()#episode前48小时内最大INws
  epfpn48=c()#episode前48小时内北风占比
  epfNen48=c()#episode前48小时内北风的累积风速
  epfpen48=c()#episode前48小时内北风的累积风速占比
  
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
  
  clean=c()
  for(j in 1:length(cleanings))
  {
    i=cleanings[j]
    neighborhood=(i-min(sys$ncof[i],windowsize)):(i+min(sys$ncob[i],windowsize))
    neighbor_candidate=intersect(start_candidate,neighborhood)
    if(length(en)>0)
      neighbor_candidate=neighbor_candidate[which(sys$X[neighbor_candidate]>sys$X[max(en)]+lagg)]
    
    if(length(neighbor_candidate)==0)
    {
      windowsize_try=8
      neighborhood=(i-min(sys$ncof[i],windowsize_try)):(i+min(sys$ncob[i],windowsize_try))
      neighbor_candidate=intersect(start_candidate,neighborhood)
      if(length(en)>0)
        neighbor_candidate=neighbor_candidate[which(sys$X[neighbor_candidate]>sys$X[max(en)]+lagg)]
    }
    if(length(neighbor_candidate)>0)
    {
      data_candidate=sys[neighbor_candidate,]
      be=data_candidate$id[max(c(max(which(data_candidate$avpm==min(data_candidate$avpm))),
                                 # max(which(data_candidate$PM2.5==min(data_candidate$PM2.5))),
                                 # max(which(data_candidate$NO2==min(data_candidate$NO2,na.rm = T))),
                                 # max(which(data_candidate$SO2==min(data_candidate$SO2,na.rm = T))),
                                 max(which(data_candidate$CO==min(data_candidate$CO,na.rm = T)))))]
      if(be<i)
      {
        if(all(sys$RAIN[be:(i-1)]==0))
          k=i-1
      }
      else if(be>=i)
        k=be-1
      while(k<nrow(sys)&k+1-be<=sys$ncob[be]&sys$RAIN[k+1]==0 &sys$INws[k+1]<du_INws&sys$ISws[k+1]<du_ISws)
        k=k+1
      len_po=k-be+1 
      if(len_po>tt)#时长超过3h
      {
        # k=be-1+max(which(sys$avpm[be:k]==max(sys$avpm[be:k])))
        # len_po=k-be+1
        # if(len_po>tt)
        # {
        #   sn <- sum(sys$IN[be:k]>0)
        #   pn <- sn/len_po#北风时长占比
        #   if(sys$PM2.5[be]+r_pm<sys$PM2.5[k]& pn<du_pn)
        {
          clean=c(clean,i)
          begin=c(begin,be)
          en=c(en,k)
          num=num+1
        }
        {
          clean_hour=c(clean_hour,sys$hour[i])
          begin_hour=c(begin_hour,sys$hour[be])
          en_hour=c(en_hour,sys$X[k])
          clean_X=c(clean_X,sys$X[i])
          begin_X=c(begin_X,sys$X[be])
          en_X=c(en_X,sys$X[k])
          
          epfmINws=c(epfmINws,sys$epfmINws[be])
          epfpn=c(epfpn,sys$epfpn[be])
          epfNen=c(epfNen,sys$epfNen[be])
          epfpen=c(epfpen,sys$epfpen[be])
          pmf35=c(pmf35,sys$pm35[be])
          epfmINws48=c(epfmINws48,sys$epfmINws48[be])
          epfpn48=c(epfpn48,sys$epfpn48[be])
          epfNen48=c(epfNen48,sys$epfNen48[be])
          epfpen48=c(epfpen48,sys$epfpen48[be])
          
          epl=c(epl,len_po)
          epr=c(epr,max(sys$av_PM2.5[be:k])-min(sys$av_PM2.5[be:k]))
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
          epmp=c(epmp,mean(sys$av_PM2.5[be:k]))
          epbp=c(epbp,sys$av_PM2.5[be])
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
        }
      }
    }
  }
  
  ep <- as.data.frame(cbind(begin,en,clean,epl,epr,eprPM10,eprSO2,eprNO2,eprCO,eprO3,epsp,epmp,epbp,epmW,epmTE,epmDE,
                            epmHU,epmPR,epmbl,epdpen,epfmINws,epfpn,epfNen,epfpen,epfmINws48,epfpn48,epfNen48,epfpen48,pmf35,
                            begin_hour,en_hour,clean_hour,begin_X,en_X,clean_X))
  write.csv(ep,file=paste0(pathsavees,sysepname,'stat.csv'))
  
  len_ep=ep$epl
  
  sec=c()#行号
  orde=c()#在一个episode中的序号
  ep_id=c()#第几个episode
  
  dataep_before=c()
  dataep_middle=c()
  dataep_after=c()
  
  lag_ear=4; lag_lat=3
  for(j in 1:nrow(ep))
  {
    sect=ep$begin[j]:ep$en[j]
    sec=c(sec,sect)
    orde=c(orde,1:len_ep[j])
    ep_id=c(ep_id,rep(j,len_ep[j]))
    
    if(ep$begin_X[j]==ep$clean_X[j])
    {
      beg=ep$begin_X[j]-lag_ear
      star=ep$begin_X[j]
      enn=ep$begin_X[j]+lag_lat
      
      sec_before_id=which(data_site$X %in% beg:(star-1))
      sec_after_id=which(data_site$X %in% star:enn)
      
      if(length(sec_before_id)>0)
      {
        sec_before=data_site[sec_before_id,]
        sec_before$kind=0
        sec_before$ep_id=j
        sec_before$orde=sec_before$X-star+1
        dataep_before=rbind(dataep_before,sec_before)
      }
      
      if(length(sec_after_id)>0)
      {
        sec_after=data_site[sec_after_id,]
        sec_after$kind=0
        sec_after$ep_id=j
        sec_after$orde=sec_after$X-star+1
        dataep_after=rbind(dataep_after,sec_after)
      }
    }
    else if (ep$begin_X[j]<ep$clean_X[j])
    {
      beg=ep$begin_X[j]-lag_ear
      star=ep$begin_X[j]
      clea=ep$clean_X[j]
      enn=ep$clean_X[j]+lag_lat
      
      sec_before_id=which(data_site$X %in% beg:(star-1))
      sec_middle_id=which(data_site$X %in% star:(clea-1)) 
      sec_after_id=which(data_site$X %in% clea:enn) 
      
      if(length(sec_before_id)>0)
      {
        sec_before=data_site[sec_before_id,]
        sec_before$kind=-1
        sec_before$ep_id=j
        sec_before$orde=sec_before$X-star+1
        dataep_before=rbind(dataep_before,sec_before)
      }
      
      if(length(sec_middle_id)>0)
      {
        sec_middle=data_site[sec_middle_id,]
        sec_middle$kind=-1
        sec_middle$ep_id=j
        sec_middle$orde=sec_middle$X-star+1
        dataep_middle=rbind(dataep_middle,sec_middle)
      }
      
      if(length(sec_after_id)>0)
      {
        sec_after=data_site[sec_after_id,]
        sec_after$kind=-1
        sec_after$ep_id=j
        sec_after$orde=sec_after$X-star+1
        dataep_after=rbind(dataep_after,sec_after)
      }  
    }
    else
    {
      beg=ep$clean_X[j]-lag_ear
      clea=ep$clean_X[j]
      star=ep$begin_X[j]
      enn=ep$begin_X[j]+lag_lat
      
      sec_before_id=which(data_site$X %in% beg:(clea-1))
      sec_middle_id=which(data_site$X %in% clea:(star-1))
      sec_after_id=which(data_site$X %in% star:enn) 
      
      if(length(sec_before_id)>0)
      {
        sec_before=data_site[sec_before_id,]
        sec_before$kind=1
        sec_before$ep_id=j
        sec_before$orde=sec_before$X-star+1
        dataep_before=rbind(dataep_before,sec_before)
      }
      if(length(sec_middle_id)>0)
      {
        sec_middle=data_site[sec_middle_id,]
        sec_middle$kind=1
        sec_middle$ep_id=j
        sec_middle$orde=sec_middle$X-star+1
        dataep_middle=rbind(dataep_middle,sec_middle)
      }
      if(length(sec_after_id)>0)
      {
        sec_after=data_site[sec_after_id,]
        sec_after$kind=1
        sec_after$ep_id=j
        sec_after$orde=sec_after$X-star+1
        dataep_after=rbind(dataep_after,sec_after)
      }
    }
    
  }
  
  sys_ep=cbind(sys[sec,-dim(sys)[2]],ep_id)
  sys_ep=cbind(sys_ep,orde)
  write.csv(sys_ep,file=paste0(pathsavee,sysepname,'.csv'))
  
  pdf(file= paste0(pathsavep,sysepname,'isode.pdf'),height = 20, width = 20,family = 'GB1')
  par(mfrow=c(1,1))
  boxplot(len_ep,range = T);title(paste0('Length of ',nrow(ep),' episodes of ',sysepname))
  for(j in 1:nrow(ep))
  {
    lag_ear=min(10,sys$ncof[ep$begin[j]])
    beg=which(da_s$X==sys$X[ep$begin[j]]-lag_ear)
    lag_lat=min(10,sys$ncob[ep$en[j]])
    enn=which(da_s$X==sys$X[ep$en[j]]+lag_lat)
    sect=beg:enn
    
    par(mfrow=c(5,2))
    plot(da_s$X[sect],da_s$avpm[sect],type = 'o',main="PM2.5",xlab='X',ylab ='PM2.5');abline(v=da_s$X[beg+lag_ear],lty = 3,col="blue");abline(v=da_s$X[enn-lag_lat],lty = 3,col="blue");abline(h=be_PM2.5,col="red")
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
  
  return(num)
}
#select episodes
n123=c()
rnames=c()
pollulength=c()
#episodes <- function(x,path4,path5,pathsavep,pathsavee,pathsaveem,pathsavees,year,season,seasons,year1)
for(k in 1:nrow(csnames))
{
  sites=csnames[k,]
  rnames=c(rnames,paste0(sites[1],sites[2],sites[3]))
  n12=0
  da_cs <-read.csv(paste0(path8,prefix,sites[1],sites[2],sites[3],'_cleaned.csv'),header = TRUE) #da_agw
  
  csysn <-paste0(sites[1],sites[2],sites[3],rep(years,each=length(season)),rep(seasons,length(years)),'_cleaned.csv')
  sysepn<-paste0(rep(sites,length(years)*length(seasons)),rep(years,each=length(seasons)*length(sites)),
                 rep(rep(seasons,each=length(sites)),length(years)),'_ep')
  
  sNSn <-paste0(prefix,sites,'_NS.csv') 
  
  sitn <-paste0(prefix,sites)
  
  for(j in 1:length(sitn))
  {
    sit=sitn[j]
    site=da_cs$loca
    da_s=da_cs[which(site==sit),]#da_a
    daNS_s=read.csv(paste0(path7,sNSn[j]), header = TRUE)#daNS_a
    data_site=read.csv(paste0(path8,sit,'_cleaned.csv'),stringsAsFactors = F)
    bbb=daNS_s$X[which(daNS_s$year==year1&daNS_s$month==3&daNS_s$day==1&daNS_s$hour==0)]
    bbb1=bbb-1
    for(i in 1:length(csysn))
    {
      csysname=csysn[i]
      sysepname=sysepn[(i-1)*3+j]
      n12=episode_selection(csysname,sysepname,sit,n12,bbb1,windowsize=6,data_site)
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
  n123=c(n123,n12)
}
n123=as.data.frame(n123)
rownames(n123)=rnames
#--------------------------------------------------------------
ysepn <-paste0(rep(years,length(seasons)),rep(seasons,each=length(years)))
pathmdatasave <- paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/modeldata_',be_PM2.5,'/') #存model data的地址
rnames=c("DongsiTiantanNongzhanguan","GuanyuanWanliuAotizhongxin","ShierzhongWuzijuLeidazhan","HuadianerquYouyongguanJiancezhan")

Miss=c()
for (k in 3:length(rnames)) 
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

#-----------
pm_start=c()
for(u in 3:length(rnames))
{
  for(i in 1:length(seasons))
  {
    for(j in 1:length(years))
    {
      data=read.csv(paste0(pathsavee,rnames[u],ysepn[(i-1)*length(years)+j],'_ep.csv'),
                    stringsAsFactors = FALSE)
      ep_begin=data$avpm[data$orde==1&data$hour%in% berange]
      pm_start=rbind(pm_start,c(round(max(ep_begin),1),length(ep_begin),length(which(ep_begin<=35)),length(which(ep_begin>35&ep_begin<=40)),length(which(ep_begin>40&ep_begin<=45)),
                                length(which(ep_begin>45&ep_begin<=50))))
    }
  }
}
pm_start=as.data.frame(pm_start)
colnames(pm_start)=c('Max','Total','0-35','35-40','40-45','45-50')
pm_start$cluster=rep(rnames[3:4],each=length(seasons)*length(years))
pm_start$season=rep(rep(seasons,each=length(years)),length(rnames[3:4]))
pm_start$year=rep(years,length(rnames[3:4])*length(seasons))
pm_start$cluster=factor(pm_start$cluster,levels=c('ShierzhongWuzijuLeidazhan','HuadianerquYouyongguanJiancezhan'),
                        labels=c('Tangshan','Baoding'))
out=apply(pm_start[,c('cluster','season','year','Total','0-35','35-40','40-45','45-50','Max')],2,as.character)
print(xtable(out),include.rownames =F)

#--------------------Table 1------------------
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin",
          "Shierzhong","Wuziju","Leidazhan",
          "Huadianerqu","Youyongguan","Jiancezhan")
csnames=matrix(csnames,ncol=3,byrow = T)

ep_len=c()
clean_begin=c()
statistic=c()
count_years=c()
for(ss in 1:length(seasons))
{
  for(ii in 3:nrow(csnames))
  {
    sites=csnames[ii,]
    statses=c()
    gaps=c()
    for(yy in 1:length(years))
    {
      for(jj in 1:length(sites)){
        stats=read.csv(paste0(pathsavees,sites[jj],years[yy],seasons[ss],'_epstat.csv'),stringsAsFactors = F)
        stats$site=sites[jj]
        stats$year=years[yy]
        statses=rbind(statses,stats)
        gaps=c(gaps,stats$begin_X[2:nrow(stats)]-stats$en_X[1:(nrow(stats)-1)])
      }
    }
    count_year=as.data.frame(statses%>%group_by(year)%>%summarise(counts=n()))
    count_year$day=as.data.frame(statses[statses$begin_hour%in% berange,]%>%group_by(year)%>%summarise(counts=n()))$counts
    count_year$night=as.data.frame(statses[!statses$begin_hour%in% berange,]%>%group_by(year)%>%summarise(counts=n()))$counts
    count_year$sites=str_c(sites, collapse = "")
    count_year$season=seasons[ss]
    count_years=rbind(count_years,count_year)
    
    statistic=rbind(statistic,data.frame(count=nrow(statses),day=sum(statses$begin_hour%in% berange),night=sum(!statses$begin_hour%in% berange),
                                         length.m=mean(statses$epl),length.sd=sd(statses$epl),
                                         length.25=quantile(statses$epl,0.25),length.50=median(statses$epl),length.75=quantile(statses$epl,0.75),
                                         range.m=mean(statses$epr),range.sd=sd(statses$epr),
                                         lag.m=mean(statses$begin-statses$clean),lag.sd=sd(statses$begin-statses$clean),
                                         gap.m=mean(gaps),gap.sd=sd(gaps),
                                         season=seasons[ss],sites=str_c(sites, collapse = "")))
    clean_begin=rbind(clean_begin,cbind(rep(ss,nrow(statses)),rep(ii,nrow(statses)),statses$begin-statses$clean))
    ep_len=rbind(ep_len,cbind(rep(ss,nrow(statses)),rep(ii,nrow(statses)),statses$epl))
  }
}
count_years$season=factor(count_years$season,levels=seasons)
statistic$season=factor(statistic$season,levels=seasons)
statistic=arrange(statistic,sites,season)
statistic$length=paste0(round(statistic$length.m-1,1),'(',round(statistic$length.sd/sqrt(statistic$count),1),')')
statistic$range=paste0(round(statistic$range.m,1),'(',round(statistic$range.sd/sqrt(statistic$count),1),')')
statistic$lag=paste0(round(statistic$lag.m,1),'(',round(statistic$lag.sd/sqrt(statistic$count),1),')')
statistic$gap=paste0(round(statistic$gap.m,1),'(',round(statistic$gap.sd/sqrt(statistic$count),1),')')
statistic[,c('count','day','night')]=apply(round(statistic[,c('count','day','night')]/3/length(years),0),2,as.character)
statistic[,c('length.25','length.50','length.75')]=apply(statistic[,c('length.25','length.50','length.75')]-1,2,as.character)
print(xtable(statistic[,c('sites','season','count','day','night','length','length.25','length.50','length.75','range','lag','gap')],
             caption=paste0('statistics')), include.rownames =F)

#-------------------
eplen=7
significance <- function(x)
{
  if(x<0.001) 
    return('***')
  else if(x<0.01) 
    return('**')
  else if(x<0.05) 
    return('*')
  else return(' ')
}
convert_variables <- function(x)
{
  if(x=='epfmINws')
    return('MCNWS')
  else if(x=='epfNen')
    return('SNWS')
  # else if(x=='pmf35')
  #   return('DPM')
  else if(x=='blh')
    return('D(LogBLH)')
  else if(x=='HUMI')
    return('D(LogHUMI)')
  else if(x=='uwind')
    return('D(WSU)')
  else if(x=='vwind')
    return('D(WSV)')
  else if(x=='Iwu')
    return('D(IWU)')
  else if(x=='Iwv')
    return('D(IWV)')
  else if(x=='WSPM')
    return('D(WS)')
  else if(x=='INws')
    return('D(CNWS)')
  else if(x=='ISws')
    return('D(CSWS)')
  else if(x%in%c('TEMP','PRES','DEWP'))
    return(paste0('D(',x,')'))
  else 
    return('Time dummies')
}

Vcx <- function(x) vcovHC(x, cluster = "group", method = "arellano")
mytheme=theme_bw()+theme(axis.title = element_text(size = 10), 
                         text = element_text(face = "bold"),
                         strip.text = element_text(size = 10,face = 'bold'),
                         strip.background = element_rect(color="black", fill="white", linetype="solid"),
                         #axis.title.x = element_blank(),
                         #axis.title.y = element_blank(),
                         axis.text.x = element_text(size=10,hjust = 0.5, face = 'bold'),
                         axis.text.y = element_text(size=10, face = 'bold'),
                         plot.title = element_text(size=15, face = 'bold', hjust = 0.5, vjust = 0.5),
                         legend.title = element_blank(),
                         legend.text = element_text(size=10, face = 'bold'),
                         legend.key.width  = unit(.3,"inches"),
                         legend.key.height = unit(.3,"inches"),
                         panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

mytheme1=theme_bw()+theme(axis.title = element_text(size = 10), 
                          text = element_text(face = "bold"),
                          strip.text = element_text(size = 10,face = 'bold'),
                          strip.background = element_rect(color="black", fill="white", linetype="solid"),
                          #axis.title.x = element_blank(),
                          #axis.title.y = element_blank(),
                          axis.text.x = element_text(size=10,hjust = 0.5, face = 'bold'),
                          axis.text.y = element_text(size=10, face = 'bold'),
                          plot.title = element_blank(),
                          #legend.title = element_blank(),
                          legend.text = element_text(size=10, face = 'bold'),
                          legend.key.width  = unit(.3,"inches"),
                          legend.key.height = unit(.3,"inches"),
                          panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 
mytheme_year=theme_bw()+theme(axis.title = element_text(size = 10), 
                              text = element_text(face = "bold"),
                              strip.text = element_text(size = 10,face = 'bold'),
                              strip.background = element_rect(color="black", fill="white", linetype="solid"),
                              #axis.title.x = element_blank(),
                              #axis.title.y = element_blank(),
                              axis.text.x = element_text(size=10, angle=45,hjust = 0.5, vjust=0.5,face = 'bold'),
                              axis.text.y = element_text(size=10, face = 'bold'),
                              plot.title = element_blank(),
                              legend.title = element_blank(),
                              legend.text = element_text(size=10, face = 'bold'),
                              legend.key.width  = unit(.3,"inches"),
                              legend.key.height = unit(.3,"inches"),
                              panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 
mytheme_null=theme_bw()+theme(axis.title = element_text(size = 10), 
                              text = element_text(face = "bold"),
                              strip.text = element_text(size = 10,face = 'bold'),
                              strip.background = element_rect(color="black", fill="white", linetype="solid"),
                              axis.text.x = element_text(size=10,hjust = 0.5, face = 'bold'),
                              axis.text.y = element_text(size=10, face = 'bold'),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_blank(),
                              #legend.title = element_blank(),
                              legend.text = element_text(size=10, face = 'bold'),
                              legend.key.width  = unit(.3,"inches"),
                              legend.key.height = unit(.3,"inches"),
                              panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 
mytheme_null_year=theme_bw()+theme(axis.title = element_text(size = 10), 
                                   text = element_text(face = "bold"),
                                   strip.text = element_text(size = 10,face = 'bold'),
                                   strip.background = element_rect(color="black", fill="white", linetype="solid"),
                                   axis.text.x = element_text(size=10, angle=45,hjust = 0.5, vjust=0.5,face = 'bold'),
                                   axis.text.y = element_text(size=10, face = 'bold'),
                                   axis.title.x = element_blank(),
                                   axis.title.y = element_blank(),
                                   plot.title = element_blank(),
                                   legend.title = element_blank(),
                                   legend.text = element_text(size=10, face = 'bold'),
                                   legend.key.width  = unit(.3,"inches"),
                                   legend.key.height = unit(.3,"inches"),
                                   panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

eplen=7
pathsavegp=paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/olsplot_new_",be_PM2.5,"/")
pathsaveform=paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_",be_PM2.5,"/")
berange=6:18#range of the beginning of episodes 
years=2013:2018
seasons=c('spring','summer','autumn','winter')
pollutant_sp='NO2' #'SO2'
ysepn <-paste0(rep(years,length(seasons)),rep(seasons,each=length(years)))

#--------------------------------------------
candidate=c('TEMP','PRES','DEWP','blh','HUMI',
            'INws','ISws',
            'epfmINws','epfNen')
variables_all=c(candidate,paste0('factor(orde)',2:eplen))

allls=c()
origin_adjust=c()
compared=c()
coeffises=c()
u=i=j=1
for(u in 3:length(rnames))
{
  adjust_bas=c()
  coeffis=c()
  for(i in 1:length(seasons))
  {
    modef1=vector("list",length(years))
    modeplm=vector("list",length(years))
    out_print=vector("list",length(years))
    
    #-----------------
    new=matrix(rep(0,(eplen-1)*(length(candidate)+1)),nrow=eplen-1)#value for ajustment
    
    mean_or_ad=vector("list",length(years))
    adjust_ba=c()
    test_pv=c()
    p_residuals=c()
    
    datas1=vector("list",length(years))
    datas_coef=vector("list",length(years))
    results=vector("list",length(years))
    v_rank=data.frame(variable=c('factor(orde)',candidate))
    V_conditions=vector("list",(eplen-1))
    for(j in 1:length(years))
    {
      d<- read.csv(paste0(pathmdatasave,rnames[u],ysepn[(i-1)*length(years)+j],'_eptreat.csv'),
                   stringsAsFactors = FALSE)
      d$pollutant=d[,paste0('av_',pollutant_sp)]
      idna=unique(d$ep_id[is.na(d$pollutant)])#delete the episode whose pollutant is missing
      d=d[!d$ep_id%in%idna,]
      sta=read.csv(paste0(pathsavees,rnames[u],ysepn[(i-1)*length(years)+j],'_epstat.csv'),
                   stringsAsFactors = FALSE)
      sta$epfpen[which(is.na(sta$epfpen))]=0
      sta$epfpen48[which(is.na(sta$epfpen48))]=0
      sta$X=1:nrow(sta)
      sta=sta[setdiff(sta$X,idna),]
      
      d1=d[d$orde==1,c('ep_id','orde','hour')]
      poll_s=d$pollutant[d$orde==1]
      poll_6=rep(-1,length(d$pollutant[d$orde==1]))
      poll_6[which(sta$epl>=6)]=d$pollutant[d$orde==6]
      poll_6[which(sta$epl<6)]=d$pollutant[cumsum(sta$epl)[which(sta$epl<6)]]
      if(pollutant_sp!="SO2")
        id_period=d1$ep_id[which((d1$hour %in% berange)& poll_s<poll_6)]#starting time in the range
      else
        {id_period=d1$ep_id[which((d1$hour %in% berange))]#starting time in the range
        idps=c()
        for(idp in id_period)
        {
          if(coef(lm(pollutant~orde,data=d[d$ep_id==idp,]))[2]>0)
            idps=c(idps,idp)
        }
        id_period=idps
        }
      print(length(id_period))
      if(length(id_period)>0)
      {
        d=d[which(d$ep_id%in%id_period),]
        sta=sta[which(sta$X%in%id_period),]
      }
      
      da=d[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv','INws','ISws',
              'epdpen','epfmINws','epfpn','epfNen','epfpen','pmf35','epfmINws48','epfpn48','epfpen48','epfNen48')]
      da$blh=log(d$blh)
      da$HUMI=log(d$HUMI)
      da$pollutant=d$pollutant
      da$ep_id=d$ep_id
      da$orde=d$orde
      da$type=d$type
      
      # ggplot(da,aes(x=orde, y=pollutant,group=factor(ep_id)))+
      #   geom_point(size=0.5)+geom_line()+stat_smooth(method="lm",se = F)+
      #   facet_wrap(.~ ep_id,ncol=8, scales = "free") + theme_bw()+
      #   labs(x = "Time", y = pollutant_sp)
      # 
      # boxplot(pollutant~orde, data=da)
      
      origin=da[which(da$orde==1),c('TEMP','PRES','DEWP','blh','HUMI','WSPM','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv','INws','ISws','pollutant')]
      repe=c()
      for(k in 1:length(id_period))
      {
        repe=rbind(repe,matrix(unlist(rep(as.vector(origin[k,]),each=sta$epl[k])),nrow=sta$epl[k]))
      }
      colnames(repe)=c('TEMP','PRES','DEWP','blh','HUMI','WSPM','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv','INws','ISws','pollutant')
      
      dataa=as.matrix(da)  
      col_sub=which(colnames(dataa)%in%c('TEMP','PRES','DEWP','blh','HUMI','WSPM','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv','INws','ISws','pollutant'))
      #after difference
      dataa[,col_sub]=dataa[,col_sub]-repe
      dataa=as.data.frame(dataa)
      
      # ggplot(dataa,aes(x=orde, y=pollutant,group=factor(ep_id)))+
      #   geom_point(size=0.5)+geom_line()+stat_smooth(method="lm",se = F)+
      #   facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + theme_bw()+
      #   labs(x = "Time", y = pollutant_sp)
      #delete the first point in each episode
      dataaa=dataa[1<dataa$orde,]
      # ggplot(dataaa,aes(x=orde, y=pollutant,group=factor(ep_id)))+
      #   geom_point(size=0.5)+geom_line()+stat_smooth(method="lm",se = F)+
      #   facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + theme_bw()+
      #   labs(x = "Time", y = pollutant_sp)
      
      dataaa1=dataaa
      datas1[[j]]=dataaa1
      
      dat_epl=left_join(dataaa1,sta[,c('X','epl')],by=c('ep_id'='X'))
      #----------------------------------
      mm=as.data.frame(dat_epl %>%
                         group_by(orde) %>%
                         summarise(num=n(),TEMP = mean(TEMP),PRES=mean(PRES),DEWP=mean(DEWP),blh=mean(blh),HUMI = mean(HUMI),
                                   INws=mean(INws),ISws=mean(ISws),epfmINws=mean(epfmINws),epfNen= mean(epfNen),
                                   pollutant=mean(pollutant)))
      dif_length=table(sta$epl)
      dif_length=cbind(as.numeric(names(dif_length)),as.numeric(dif_length))
      V_condition=vector("list",(eplen-1))
      
      for(tt in 2:eplen)
      {
        VC=matrix(rep(0,length(candidate)*length(candidate)),ncol=length(candidate))
        substi=matrix(rep(0,length(candidate)*length(candidate)),ncol=length(candidate))
        for(dd in which(dif_length[,1]>=tt))
        {
          len=dif_length[dd,1]
          coun=dif_length[dd,2]
          scattermatrix=(coun-1)*var(dat_epl[dat_epl$epl==len&dat_epl$orde==tt,candidate])
          if(coun>1) substi=scattermatrix
          VC=VC+substi
        }
        #\frac{1}{A^2}\frac{1}{(\sum_{l\geq t}n_{al})^2}\sum_{l\geq t}(n_{al}-1)\hat{var}(X_{asl}|T_{as}=l)
        V_condition[[tt-1]]=VC/((length(years)*mm$num[mm$orde==tt])^2)
        #add up terms about var about E(X) in different years
        if(j==1)  V_conditions[[tt-1]]=V_condition[[tt-1]]
        else V_conditions[[tt-1]]=V_conditions[[tt-1]]+V_condition[[tt-1]]
        #\frac{1}{A^2}\sum_{a=1}^{A}\frac{1}{(\sum_{l\geq t}n_{al})^2}\sum_{l\geq t}(n_{al}-1)\hat{var}(X_{asl}|T_{as}=l)
      }
      mm$year=years[j]
      mean_or_ad[[j]]=mm[mm$orde<=eplen,]#yearly mean
      
      new=new+as.matrix(mm[mm$orde<=eplen,c('orde','TEMP','PRES','DEWP','blh','HUMI',
                                            'INws','ISws','epfmINws','epfNen')])
      
      data_coef=dataaa1[,c(candidate,'ep_id','orde',"pollutant")]
      data_coef[,candidate]=as.data.frame(scale(data_coef[,candidate],center = TRUE, scale = TRUE))
      model_null=lm(pollutant~-1,data_coef)
      com1 <- update(model_null, paste("~ .+ factor(orde)+", paste(candidate,collapse='+')), evaluate = FALSE)
      com1=eval.parent(com1)
      rank_forw=step(model_null,scope=formula(com1),k=log(nobs(com1)),trace=0,direction="forward")
      v_select=str_sub(rank_forw$anova$Step[-1],3)
      v_rank=left_join(v_rank,data.frame(variable=v_select,rank=1:length(v_select)))
      colnames(v_rank)[j+1]=years[j]
      datas_coef[[j]]=data_coef
    }
    new=new/length(years)#X_t^*
    v_rank_im=v_rank
    v_rank_im_sub=v_rank_im
    v_rank_im_sub[is.na(v_rank_im_sub)]=nrow(v_rank)
    v_rank_im_sub$average=apply(v_rank_im_sub[,-1],1,mean)
    v_rank_im$average=v_rank_im_sub$average
    v_rank_season=arrange(v_rank_im,average)
    element_order=as.character(v_rank_im$variable[order(v_rank_im$average)])
    
    select_try=c()
    try_re=c()
    
    for(vv in 1:length(element_order)){
      signi=c()
      for(k in 1:length(years))
      {
        M1=lm(pollutant ~ -1,data=datas1[[k]])
        com1 <- update(M1, paste("~ .+", paste(element_order[1:vv],collapse='+')), evaluate = FALSE)
        com1=eval.parent(com1)
        signi=rbind(signi,c(summary(com1)$r.squared,summary(com1)$adj.r.squared,AIC(com1),BIC(com1)))
      }
      colnames(signi)=c('R2','adj.R2','AIC','BIC')
      signi=as.data.frame(signi)
      
      try_re=rbind(try_re,apply(signi,2,mean))
    }
    try_re=as.data.frame(try_re)
    partial=which(apply(!is.na(v_rank_season[,as.character(years)]),1,sum)>0&max(try_re$R2)-try_re$R2<=0.05)
    decide=partial[which.min(round(try_re$BIC[partial]))]
    print(paste(element_order[1:decide],collapse='+'))
    
    #---------------------------------------------------------------    
    elemen_final=element_order[1:decide]
    elemen_need=setdiff(elemen_final,'factor(orde)')
    for(k in 1:length(years))
    {
      M1_stand=lm(pollutant ~ -1,data=datas_coef[[k]])
      com1_stand <- update(M1_stand, paste("~ .+ ", paste(elemen_final,collapse='+')), evaluate = FALSE)
      com1_stand=eval.parent(com1_stand)
      plmmod_stand=plm(formula(com1_stand), data=datas_coef[[k]],index=c("ep_id","orde"),model = "pooling")
      
      M1=lm(pollutant ~ -1,data=datas1[[k]])
      com1 <- update(M1, paste("~ .+ ", paste(elemen_final,collapse='+')), evaluate = FALSE)
      com1=eval.parent(com1)
      plmmod=plm(formula(com1), data=datas1[[k]],index=c("ep_id","orde"),model = "pooling")
      
      #final linear model
      modef1[[k]]=com1
      
      # Testing individual-fixed effects. The null is that no individual-fixed effects needed
      plmtest(plmmod_stand, c("individual"), type=("bp"))
      #Semi-parametric test for the presence of individual unobserved effects in panel models.
      test1=pwtest(plmmod_stand)
      print(test1)
      #test the presence of heteroskedasticity
      test2=bptest(plmmod_stand,studentize=F)
      print(test2)
      #Modified BNF–Durbin–Watson Test for AR(1) disturbances in panel models.
      test3=pbnftest(plmmod_stand)
      print(test3)
      
      test_pv=cbind(test_pv,c(test1$p.value,test2$p.value))
      modeplm[[k]]=plmmod
      residual=plmmod$residuals
      datas1[[k]]$residuals=residual[order(as.numeric(names(residual)))]
      datas1[[k]]$fitted=datas1[[k]]$pollutant-datas1[[k]]$residuals
      
      boxplot(residuals~orde, data=datas1[[k]])
      p.residual=datas1[[k]][,c('ep_id','orde','residuals')]
      p.residual$years=years[k]
      p_residuals=rbind(p_residuals,p.residual)
      
      out_result=as.data.frame(coeftest(plmmod, vcov = Vcx)[,1:4])
      out_result=out_result[rownames(out_result)%in%variables_all,]
      out_result_stand=as.data.frame(coeftest(plmmod_stand, vcov = Vcx)[,1:4])
      out_result_stand=out_result_stand[rownames(out_result_stand)%in%variables_all,]
      
      vector_order=as.vector(matrix(1:(2*nrow(out_result_stand)),byrow=T,nrow=2))
      out_print[[k]]=data.frame(Variables=c(c(rownames(out_result_stand),paste0('SE',rownames(out_result_stand)))[vector_order],
                                            'R2','Adj.R2','Number of panels','RMSE'),
                                Estimate=c(c(paste0(round(out_result_stand$Estimate,2),apply(as.matrix(out_result_stand$`Pr(>|t|)`),1,significance)),
                                             paste0('(',round(out_result_stand$`Std. Error`,2),')'))[vector_order],
                                           #round(summary(plmmod_stand)$r.squared[1],2),round(summary(plmmod_stand)$r.squared[2],2),
                                           round(summary(com1_stand)$r.squared,2),round(summary(com1_stand)$adj.r.squared,2),
                                           length(unique(datas1[[k]]$ep_id)),
                                           round(sqrt(sum((summary(plmmod_stand)$residuals)^2)/summary(plmmod_stand)$df[2]),2)))
      names(out_print[[k]])[2]=as.character(years[k])
      
      
      coeffi=data.frame(Variables=variables_all,Estimates=NA,Signifance=NA,Years=years[k],Seasons=seasons[i])
      coeffi$Estimates[match(rownames(out_result_stand),variables_all)]=out_result_stand$Estimate
      coeffi$Signifance[match(rownames(out_result_stand),variables_all)]=apply(as.matrix(out_result_stand$`Pr(>|t|)`),1,significance)
      coeffis=rbind(coeffis,coeffi)
      
      allls=rbind(allls,c(length(unique(datas1[[k]]$ep_id)),summary(com1_stand)$r.squared,summary(com1_stand)$adj.r.squared))
      
      new_du=dummy_cols(as.data.frame(new),select_columns = c("orde"),remove_first_dummy = F)
      names(new_du)[which(names(new_du)%in%paste0('orde_',2:eplen))]=paste0('factor(orde)',2:eplen)
      
      Vvar=as.matrix(vcovG(plmmod, cluster = "group", inner = "cluster", l = 0))
      Vvar=Vvar[rownames(out_result),rownames(out_result)]
      aVar_theta=diag(as.matrix(new_du[,colnames(Vvar)])%*%Vvar%*%t(as.matrix(new_du[,colnames(Vvar)])))
      varx_add=c()
      varx_add_compare=c()
      for(vadd in 1:(eplen-1))
      {
        v_part=V_conditions[[vadd]][elemen_need,elemen_need]
        coe_part=out_result$Estimate[match(colnames(v_part),rownames(out_result))]
        varx_add=c(varx_add,t(coe_part)%*%v_part%*%coe_part)
        if(k>1)
        {coe_part_compare=out_result$Estimate[match(colnames(v_part),rownames(out_result))]-coef0
        varx_add_compare=c(varx_add_compare,t(coe_part_compare)%*%v_part%*%coe_part_compare)}
      }
      #wrong: need var but not Avar
      #varx_add=varx_add*length(unique(datas1[[k]]$ep_id))
      Vadjust=aVar_theta+varx_add
      mean_or_ad[[k]]$adjusted=as.matrix(new_du[,rownames(out_result)])%*%out_result$Estimate
      mean_or_ad[[k]]$se=sqrt(Vadjust)
      if(k==1) 
      {
        adjust0=mean_or_ad[[k]]$adjusted
        coef0=out_result$Estimate[match(colnames(v_part),rownames(out_result))]
        aVar_theta0=aVar_theta
      }
      
      if(k>1)
      {
        Vadjust_compare=aVar_theta+aVar_theta0+varx_add_compare
        #difference between two years at hour 2:7
        comparetest=data.frame(orde=2:eplen,Years=years[k],Seasons=seasons[i],Sites=rnames[u],
                               Estimates=mean_or_ad[[k]]$adjusted-adjust0,
                               SE=sqrt(Vadjust_compare))
        compared=rbind(compared,comparetest)
      }
      
      #---------------------------------
      mean_or_ad[[k]]=rbind(data.frame(orde=1,num=mean_or_ad[[k]]$num[1],TEMP=0,PRES=0,DEWP=0,blh=0,HUMI=0,
                                       INws=0,ISws=0,
                                       epfmINws=0,epfNen=0,
                                       pollutant=0,year=years[k],adjusted=0,se=0),mean_or_ad[[k]])
      
      adjust_ba=rbind(adjust_ba,mean_or_ad[[k]])
      
      results[[k]]=list(num=mean_or_ad[[k]]$num,coefficients=out_result,
                        p.origin=mean_or_ad[[k]]$pollutant,p.adjusted=mean_or_ad[[k]]$adjusted,se=mean_or_ad[[k]]$se,
                        var_theta=aVar_theta,var_x=varx_add)
    }
    colnames(test_pv)=as.character(years)
    print(xtable(test_pv,
                 caption=paste0("P-values of Wooldridge's test and Breusch-Pagan test for models of panel data in ",seasons[i]," of ",rnames[u]),
                 digits=c(0,rep(3,length(years)))),
          include.rownames =T)
    #year变这里会改
    screenreg(list("2013"=modef1[[1]],"2014"=modef1[[2]],"2015"=modef1[[3]],
                   "2016"=modef1[[4]],"2017"=modef1[[5]],"2018"=modef1[[6]]))
    
    # screenreg(list("2013"=modeplm[[1]],"2014"=modeplm[[2]],"2015"=modeplm[[3]],
    #                "2016"=modeplm[[4]],"2017"=modeplm[[5]],"2018"=modeplm[[6]]))
    
    # texreg(list("2013"=modef1[[1]],"2014"=modef1[[2]],"2015"=modef1[[3]],
    #             "2016"=modef1[[4]],"2017"=modef1[[5]],"2018"=modef1[[6]]),
    #        caption=paste0("Results of linear models for data in ",seasons[i]," of ",rnames[u]))
    all_results=left_join(left_join(left_join(left_join(left_join(out_print[[1]],out_print[[2]]),out_print[[3]]),out_print[[4]]),out_print[[5]]),out_print[[6]])
    print(xtable(all_results,
                 caption=paste0("Results of selected models for ",seasons[i]," of ",rnames[u])),
          include.rownames =F)
    
    metero=data.frame()
    for(uuu in 1:length(elemen_need))
    {
      metero_ob=adjust_ba[,c('orde','year',elemen_need[uuu])]
      names(metero_ob)[3]='values'
      metero_ob$type=elemen_need[uuu]
      metero=rbind(metero,metero_ob)
    }
    
    adjust_ba$season=seasons[i]
    adjust_bas=rbind(adjust_bas,adjust_ba)
  }
  
  coeffis$Name=apply(as.matrix(coeffis$Variables),1,convert_variables)
  #-----------------------------------
  coeffis$Name=factor(coeffis$Name,levels=c("D(DEWP)","D(LogBLH)","D(LogHUMI)","D(PRES)","D(TEMP)",
                                            "D(CNWS)","D(CSWS)","MCNWS","SNWS",
                                            "Time dummies"))
  
  coeffis$shape=rep(NA,length(coeffis$Signifance))
  coeffis$shape[!is.na(coeffis$Signifance)&coeffis$Signifance==' ']='non-significant'
  coeffis$shape[!is.na(coeffis$Signifance)&!coeffis$Signifance==' ']='significant'
  coeffis$Sites=rnames[u]
  coeffises=rbind(coeffises,coeffis)
  
  adjust_bas$season=factor(adjust_bas$season,levels=seasons)
  adjust_bas$site=rnames[u]
  
  origin_adjust=rbind(origin_adjust,adjust_bas)
}
origin_adjust$rate=0
origin_adjust$rate[origin_adjust$orde>1]=origin_adjust$adjusted[origin_adjust$orde>1]/(origin_adjust$orde[origin_adjust$orde>1]-1)
compared$pv=1-pnorm(abs(compared$Estimates/compared$SE))
compared$significance=0
compared$significance[compared$pv<0.05]='T'
compared$significance[compared$pv>=0.05]='F'
compared$rate=0
compared$rate[compared$orde>1]=compared$Estimates[compared$orde>1]/(compared$orde[compared$orde>1]-1)

write.csv(origin_adjust,file=paste0(pathsaveform,pollutant_sp,'adjusted.csv'))
write.csv(compared,file=paste0(pathsaveform,pollutant_sp,'adjustedcompared.csv'))
write.csv(coeffises,file=paste0(pathsaveform,pollutant_sp,'coefficients.csv'))

origin_adjust$season=factor(origin_adjust$season,levels = c('spring','summer','autumn','winter'),
                            labels=c('Spring','Summer','Autumn','Winter'))
origin_adjust$sitename=factor(origin_adjust$site,levels =rnames,
                              labels = c('Beijing SE','Beijing NW','Tangshan','Baoding'))
ggplot(origin_adjust[origin_adjust$orde==eplen,],aes(x=factor(season),y=adjusted/(orde-1),fill=factor(year)))+
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(x =factor(season),ymin=(adjusted-se*qnorm(1-0.05/2))/(orde-1), ymax =(adjusted+se*qnorm(1-0.05/2))/(orde-1)), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
  mytheme_year +
  theme(legend.position="none")+
  scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#66CCFF','#3366CC'))+
  labs(x = 'Year',y =paste0('Adjusted Growth Rate of ',pollutant_sp), title = paste0('Growth rate of ',pollutant_sp))

colnames(allls)=c('Number','R2','Adj.R2')
allls=as.data.frame(allls)
allls$Sites=rep(rnames[3:4],each=length(seasons)*length(years))
allls$Seasons=rep(rep(seasons,each=length(years)),length(rnames[3:4]))
allls$Years=rep(years,length(rnames[3:4])*length(seasons))

allls$sitename=factor(allls$Sites,levels = rnames[3:4],
                      labels = c('Beijing SE','Beijing NW','Tangshan','Baoding')[3:4])
allls$Seasons=factor(allls$Seasons,levels = c('spring','summer','autumn','winter'),
                     labels=c('Spring','Summer','Autumn','Winter'))
write.csv(allls,file=paste0(pathsaveform,pollutant_sp,'R2.csv'))
#----------------
hour_try=5
ad_NO=read.csv(paste0(pathsaveform,"NO2adjusted.csv"),stringsAsFactors = F)
ad_SO=read.csv(paste0(pathsaveform,"SO2adjusted.csv"),stringsAsFactors = F)
ad_NO$Pollutant='NO2'
ad_SO$Pollutant='SO2'
ad_PNS=rbind(ad_NO,ad_SO)
pps=c('NO2','SO2')
ad_PNS$ad.lb=ad_PNS$adjusted-ad_PNS$se*qnorm(1-0.05/2)
ad_PNS$ad.ub=ad_PNS$adjusted+ad_PNS$se*qnorm(1-0.05/2)
# ad_PNS$subtitle=paste0(ad_PNS$year,':  ',
#                        rep(paste0(round(ad_PNS[ad_PNS$orde==eplen,'adjusted']/(eplen-1),1),'(',round(ad_PNS[ad_PNS$orde==eplen,'se']/(eplen-1),1),')'),each=eplen))

ad_PNS$subtitle=paste0(ad_PNS$year,' (',
                       rep(paste0(round(ad_PNS[ad_PNS$orde==eplen,'adjusted']/(eplen-1),1),', ',round(ad_PNS[ad_PNS$orde==eplen,'se']/(eplen-1),1),')'),each=eplen))
ad_PNS.o=ad_PNS[,c('orde','Pollutant','pollutant','year','season','site','Pollutant','subtitle')]
ad_PNS.o$type='origin'
ad_PNS.a=ad_PNS[,c('orde','Pollutant','adjusted','year','season','site','adjusted','subtitle')]
ad_PNS.a$type='adjusted'
colnames(ad_PNS.o)=colnames(ad_PNS.a)=c('orde','Pollutant','value','year','season','site','adjusted','subtitle','type')
ad_PNSs=rbind(ad_PNS.o,ad_PNS.a)

#---------Figure4------
ad_PNS$season=factor(ad_PNS$season,levels = c('spring','summer','autumn','winter'),
                     labels=c('Spring','Summer','Autumn','Winter'))
ad_PNS$sitename=factor(ad_PNS$site,levels =rnames,
                       labels = c('Beijing SE','Beijing NW','Tangshan','Baoding'))

ad_PNS[ad_PNS$orde>1,c('adjusted','ad.lb','ad.ub')]=ad_PNS[ad_PNS$orde>1,c('adjusted','ad.lb','ad.ub')]/(ad_PNS$orde[ad_PNS$orde>1]-1)
# ad_PNS$ad.lb[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted<(0))]=ad_PNS$adjusted[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted<(0))]
# ad_PNS$ad.lb[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted>(0))]=-3.5
ad_PNS$ad.lb[which(ad_PNS$adjusted<(0))]=ad_PNS$adjusted[which(ad_PNS$adjusted<(0))]
#ad_PNS$ad.lb[which(ad_PNS$ad.lb<(0)&ad_PNS$adjusted>=(0))]=0

mytheme=theme_bw()+theme(axis.title = element_text(size = 14), 
                         text = element_text(face = "bold"),
                         strip.text = element_text(size = 14,face = 'bold'),
                         strip.background = element_rect(color="black", fill="white", linetype="solid"),
                         #axis.title.x = element_blank(),
                         #axis.title.y = element_blank(),
                         axis.text.x = element_text(size=14,hjust = 0.5, face = 'bold'),
                         axis.text.y = element_text(size=14, face = 'bold'),
                         plot.title = element_text(size=17, face = 'bold', hjust = 0.5, vjust = 0.5),
                         legend.title = element_blank(),
                         legend.text = element_text(size=14, face = 'bold'),
                         legend.key.width  = unit(.3,"inches"),
                         legend.key.height = unit(.3,"inches"),
                         panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

# #Baoding 2013 is strange.
# ad_PNS[ad_PNS$Pollutant=="NO2"&ad_PNS$year==2013&ad_PNS$sitename=='Baoding'&ad_PNS$season=='Spring',c('adjusted','ad.lb','ad.ub')]=NA
# ad_PNS[ad_PNS$Pollutant=="SO2"&ad_PNS$year==2013&ad_PNS$sitename=='Baoding'&ad_PNS$season%in%c('Spring','Summer','Autumn'),c('adjusted','ad.lb','ad.ub')]=NA

pp ='NO2'
figure4b=ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
  geom_bar(stat='identity', position='dodge') +ylim(-0.6,12.1)+
  geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
  mytheme +
  scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#66CCFF','#3366CC'))+
  #scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
  labs(x = 'Season',y =expression(bold('Adjusted Average Growth Rate'~"("*mu*"g/"*m^3~"per hour)")), title = expression(bold('(b) ')*bold(NO[2])))

#------------------------
pp='SO2'
data_line=ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp,]
data_line$hl=rep(0,nrow(data_line))
data_line$hll=rep(0,nrow(data_line))
data_line$hl[data_line$sitename%in%c('Beijing SE','Beijing NW')]=6
data_line$hll[data_line$sitename%in%c('Beijing SE','Beijing NW')]=-0.25
data_line$hl[data_line$sitename%in%c('Tangshan','Baoding')]=30 #25
data_line$hll[data_line$sitename%in%c('Tangshan','Baoding')]=-1

figure4c=ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  geom_hline(data=data_line,aes(yintercept =hl),color='white')+
  geom_hline(data=data_line,aes(yintercept =hll),color='white')+
  facet_wrap(.~ sitename,ncol=4, scales = "free_y") +
  mytheme +
  scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#66CCFF','#3366CC'))+
  #scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
  labs(x = 'Season',y =expression(bold('Adjusted Average Growth Rate'~"("*mu*"g/"*m^3~"per hour)")), title = expression(bold('(c) ')*bold(SO[2])))

figure4=ggarrange(figure4b, figure4c,ncol = 1, nrow = 2,common.legend =T,legend ='bottom')
ggsave(figure4,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_try/Figure',be_PM2.5,'.png'),units="in",width=8, height=10, dpi=350)


