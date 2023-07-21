#按起始时刻对episodes 分类
#nodes按0~23顺序排列，如1~5,6~12,13~18,19~24,就输入1,7,13,19
nodes=c(6,19)
pathsavee="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes/"
pathsavees="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat/"

mytheme_pm=theme_bw()+theme(axis.title = element_text(size = 10), 
                          text = element_text(face = "bold"),
                          strip.text = element_text(size = 10,face = 'bold'),
                          strip.background = element_rect(color="black", fill="white", linetype="solid"),
                          #axis.title.x = element_blank(),
                          #axis.title.y = element_blank(),
                          axis.text.x = element_text(size=10,hjust = 0.5, face = 'bold'),
                          axis.text.y = element_text(size=10, face = 'bold'),
                          plot.title = element_blank(),
                          legend.title = element_blank(),
                          legend.text = element_text(size=10, face = 'bold'),
                          legend.key.width  = unit(.3,"inches"),
                          legend.key.height = unit(.3,"inches"),
                          panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

plot_period <- function(id_period,da,dataa,dataaa,u,i,j,h)
{
  da1=da[which(da$ep_id %in% id_period),]
  dataa1=dataa[which(dataa$ep_id %in% id_period),]
  dataaa1=dataaa[which(dataaa$ep_id %in% id_period),]
  # print(xyplot(PM2.5~ orde|as.factor(ep_id), type="b",cex=0.5, data=da1,ylab="PM2.5",
  #              xlab="time",main=paste0("PM2.5 in ",rnames[u],ysepn[(i-1)*length(years)+j],' period',h),
  #              panel=function(x,y){
  #                panel.grid(h=-1, v= 2)
  #                panel.points(x,y,col=1,size=0.5)}))
  # 
  # print(scatterplot(PM2.5 ~ orde|factor(ep_id), boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=da1,
  #                   legend=T,ylab="PM2.5",xlab="time",main=paste0("PM2.5 in ",rnames[u],ysepn[(i-1)*length(years)+j],' period',h)))
  # 
  # print(xyplot(PM2.5~ orde|as.factor(ep_id), type="b",cex=0.5, data=dataa1,ylab="PM2.5",
  #              xlab="time",main=paste0("Differenced PM2.5 in ",rnames[u],ysepn[(i-1)*length(years)+j],' period',h),
  #              panel=function(x,y){
  #                panel.grid(h=-1, v= 2)
  #                panel.points(x,y,col=1,size=0.5)}))
  # 
  # print(scatterplot(PM2.5 ~ orde|factor(ep_id), boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=dataa1,
  #                   legend=T,ylab="Differenced PM2.5",xlab="time",main=paste0("Differenced PM2.5 in ",
  #                   rnames[u],ysepn[(i-1)*length(years)+j],' period',h)))
  #mm=dataaa1[,c('orde','PM2.5')]
  mm=da1[,c('orde','PM2.5','PM10','SO2','NO2','O3','CO','hour')]
  mm$year=rep(years[j],nrow(mm))
  mm$period=rep(h,nrow(mm))
  mm
}

rnames=c("DongsiTiantanNongzhanguan","GuanyuanWanliuAotizhongxin","ShierzhongWuzijuLeidazhan","HuadianerquYouyongguanJiancezhan")
ddata=vector("list",length(rnames)*length(seasons))
epls=c()

for (u in 1:length(rnames))
{
  for(i in 1:length(seasons))
  {
    #pdf(file= paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/pm_period/plot_period_',rnames[u],seasons[i],'.pdf'),height = 12,width=12,family = 'GB1')
    count_period=c()
    pm_period=c()
    for(j in 1:length(years))
    {
      d<- read.csv(paste0(pathsavee,rnames[u],ysepn[(i-1)*length(years)+j],'_ep.csv'), stringsAsFactors = FALSE)
      sta=read.csv(paste0(pathsavees,rnames[u],ysepn[(i-1)*length(years)+j],'_epstat.csv'),
                   stringsAsFactors = FALSE)
      sta$epfpen[which(is.na(sta$epfpen))]=0
      sta$epfpen48[which(is.na(sta$epfpen48))]=0
      sta$X=1:nrow(sta)
      
      epls=rbind(epls,c(nrow(sta),length(which(sta$epl<eplen)),length(which(sta$epl>=eplen))))
      
      da=d[,c('year','month','day','hour','TEMP','PRES','DEWP','blh','HUMI','WSPM')]
      da$blh=log(d$blh)
      da$PM2.5=d$av_PM2.5; da$PM10=d$av_PM10; da$SO2=d$av_SO2; da$CO=d$av_CO; da$NO2=d$av_NO2; da$O3=d$av_O3
      da$ep_id=d$ep_id
      da$orde=d$orde
      da$type=rep(sta$type,times=sta$epl)
      
      origin=da[which(da$orde==1),c('TEMP','PRES','DEWP','blh','HUMI','WSPM','PM2.5')]
      repe=c()
      for(k in 1:max(da$ep_id))
      {
        repe=rbind(repe,matrix(unlist(rep(as.vector(origin[k,]),each=sta$epl[k])),ncol=7))
        
      }
      colnames(repe)=c('TEMP','PRES','DEWP','blh','HUMI','WSPM','PM2.5')
      
      dataa=as.matrix(da)  
      dataa[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM','PM2.5')]=dataa[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM','PM2.5')]-repe
      dataa=as.data.frame(dataa)#differenced
      
      dataaa=dataa[1<dataa$orde&dataa$orde<=eplen,]#differenced, 1<orde<=eplen
      
      # ggplot(da,aes(x =orde, y=PM2.5,group=factor(ep_id)))+
      #   geom_point(size=0.5)+geom_line()+
      #   geom_vline(xintercept=7,color = "gray",linetype = "dashed",size=1,alpha=1)+ 
      #   facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + 
      #   labs(x = "Date", y = 'PM2.5')
      
      d1=da[da$orde==1,]
      for(h in 1:(length(nodes)-1))
      {
        id_period=d1$ep_id[intersect(which(d1$hour %in% (nodes[h]:(nodes[h+1]-1))),
                                     which(table(da$ep_id)>=eplen))]

        if(length(id_period)>0)
        {
          data_period=plot_period(id_period,da,dataa,dataaa,u,i,j,h)
          grouped=as.data.frame(data_period %>%group_by(orde) %>%summarise(count=n()))
          grouped$period=rep(h,nrow(grouped))
          grouped$year=rep(years[j],nrow(grouped))
          count_period=rbind(count_period,grouped)
          pm_period=rbind(pm_period,data_period)
        }
      }
      h=h+1
      if(nodes[1]==0)
      {
        id_period=d1$ep_id[which(d1$hour %in% nodes[h]:23)]
        if(length(id_period)>0)
        {
          data_period=plot_period(id_period,da,dataa,dataaa,u,i,j,h)
          grouped=as.data.frame(data_period %>%group_by(orde) %>%summarise(count=n()))
          grouped$period=rep(h,nrow(grouped))
          grouped$year=rep(years[j],nrow(grouped))
          count_period=rbind(count_period,grouped)
          pm_period=rbind(pm_period,data_period)
        }
      }
      else
      {
        id_period=d1$ep_id[which(d1$hour %in% c(nodes[h]:23,0:(nodes[1]-1)))]

        if(length(id_period)>0)
        {
          data_period=plot_period(id_period,da,dataa,dataaa,u,i,j,h)
          grouped=as.data.frame(data_period %>%group_by(orde) %>%summarise(count=n()))
          grouped$period=rep(h,nrow(grouped))
          grouped$year=rep(years[j],nrow(grouped))
          count_period=rbind(count_period,grouped)
          pm_period=rbind(pm_period,data_period)
        }
      }

      print(j)
    }
    ddata[[length(seasons)*(u-1)+i]]=pm_period
    # print(ggplot(pm_period,aes(x=orde,y=PM2.5))+geom_line()+geom_point()+facet_grid(year~period)+
    #         #          ylim(-0.1,3)+
    #         labs(x="Time",y="PM2.5")+scale_x_continuous(breaks=1:eplen)+
    #         ggtitle(paste0('PM2.5 grouped by time for ',rnames[u],seasons[i])))
    
    pm_period$d_n=pm_period$period
    pm_period$d_n[pm_period$period==1]='day'
    pm_period$d_n[pm_period$period==2]='night'
    ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<2019,],aes(x=factor(orde),y=PM2.5,fill=factor(d_n)))+geom_boxplot()+
            #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
            facet_wrap(~year,ncol=6,scales = "free_y") +scale_fill_simpsons()+ 
            labs(x="Hour",y="PM2.5",fill='Start point')+mytheme_pm+
            theme(legend.position="top")+
      scale_x_discrete(labels=1:eplen-1)
    ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/pm_period/PM25',
                  rnames[u],seasons[i],'pattern.png'),units="in",width=12, height=4, dpi=300)
    
    ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<2019,],aes(x=factor(orde),y=PM10,fill=factor(d_n)))+geom_boxplot()+
      #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
      facet_wrap(~year,ncol=6,scales = "free_y") +scale_fill_simpsons()+ 
      labs(x="Hour",y="PM10",fill='Start point')+mytheme_pm+
      theme(legend.position="top")+
      scale_x_discrete(labels=1:eplen-1)
    ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/pm_period/PM10',
                  rnames[u],seasons[i],'pattern.png'),units="in",width=12, height=4, dpi=300)
    
    ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<2019,],aes(x=factor(orde),y=SO2,fill=factor(d_n)))+geom_boxplot()+
      #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
      facet_wrap(~year,ncol=6,scales = "free_y") +scale_fill_simpsons()+ 
      labs(x="Hour",y="SO2",fill='Start point')+mytheme_pm+
      theme(legend.position="top")+
      scale_x_discrete(labels=1:eplen-1)
    ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/pm_period/SO2',
                  rnames[u],seasons[i],'pattern.png'),units="in",width=12, height=4, dpi=300)
    
    ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<2019,],aes(x=factor(orde),y=CO,fill=factor(d_n)))+geom_boxplot()+
      #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
      facet_wrap(~year,ncol=6,scales = "free_y") +scale_fill_simpsons()+ 
      labs(x="Hour",y="CO",fill='Start point')+mytheme_pm+
      theme(legend.position="top")+
      scale_x_discrete(labels=1:eplen-1)
    ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/pm_period/CO',
                  rnames[u],seasons[i],'pattern.png'),units="in",width=12, height=4, dpi=300)
    
    ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<2019,],aes(x=factor(orde),y=NO2,fill=factor(d_n)))+geom_boxplot()+
      #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
      facet_wrap(~year,ncol=6,scales = "free_y") +scale_fill_simpsons()+ 
      labs(x="Hour",y="NO2",fill='Start point')+mytheme_pm+
      theme(legend.position="top")+
      scale_x_discrete(labels=1:eplen-1)
    ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/pm_period/NO2',
                  rnames[u],seasons[i],'pattern.png'),units="in",width=12, height=4, dpi=300)
    
    ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<2019,],aes(x=factor(orde),y=O3,fill=factor(d_n)))+geom_boxplot()+
      #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
      facet_wrap(~year,ncol=6,scales = "free_y") +scale_fill_simpsons()+ 
      labs(x="Hour",y="O3",fill='Start point')+mytheme_pm+
      theme(legend.position="top")+
      scale_x_discrete(labels=1:eplen-1)
    ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/pm_period/O3',
                  rnames[u],seasons[i],'pattern.png'),units="in",width=12, height=4, dpi=300)
    
    #dev.off()
  }
}
colnames(epls)=c('total',paste0('less than ',eplen),paste0('geq ',eplen))
rownames(epls)=paste0(rep(rnames,each=length(years)*length(seasons)),rep(years,length(seasons)*length(rnames)),
                      rep(rep(seasons,each=length(years)),length(rnames)))

