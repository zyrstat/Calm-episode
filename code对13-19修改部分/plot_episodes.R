mytheme_y=theme_bw()+theme(axis.title = element_text(size = 10), 
                 text = element_text(face = "bold"),
                 strip.text = element_text(size = 10,face = 'bold'),
                 strip.background = element_rect(color="black", fill="white", linetype="solid"),
                 axis.title.x = element_blank(),
                 #axis.title.y = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_text(size=10, face = 'bold'),
                 plot.title = element_blank(),
                 legend.title = element_blank(),
                 legend.text = element_text(size=10, face = 'bold'),
                 legend.key.width  = unit(.3,"inches"),
                 legend.key.height = unit(.3,"inches"),
                 panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

mytheme_x=theme_bw()+theme(axis.title = element_text(size = 10), 
                           text = element_text(face = "bold"),
                           strip.text = element_text(size = 10,face = 'bold'),
                           strip.background = element_rect(color="black", fill="white", linetype="solid"),
                           #axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.x = element_text(size=10, angle=45,hjust = 0.5, vjust=0.5,face = 'bold'),
                           axis.text.y = element_text(size=10, face = 'bold'),
                           plot.title = element_blank(),
                           legend.title = element_blank(),
                           legend.text = element_text(size=10, face = 'bold'),
                           legend.key.width  = unit(.3,"inches"),
                           legend.key.height = unit(.3,"inches"),
                           panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

plotting_episode=function(dat){
  dat$w_direction=NA
  dat$w_direction[dat$ISws>0]="CSWS"
  dat$w_direction[dat$INws>0]="CNWS"
  dat$w_direction=factor(dat$w_direction,levels=c('CSWS','CNWS','CCVWS'))
  dat$INSws=dat$INws+dat$ISws
  dat$seperation=factor(dat$seperation,labels=c('Calm episode','Uncalm period'))
  dat$date=as.POSIXct(paste0(dat$year,'-',dat$month,'-',dat$day,' ',dat$hour,':00'))
  j=ceiling(nrow(dat)/720)

  for(i in 1:(j-1))
  {
    # ggplot(data=dat[(720*(i-1)+1):min(720*i,nrow(dat)),],aes(x=date,y=av_PM2.5,color=seperation,group=1))+
    #   geom_line(size=1,show.legend = T)+
    #   #scale_x_datetime(date_breaks ="1 day",date_labels ="%Y-%m-%d %H:%M")+
    #   scale_x_datetime(date_breaks ="1 day",date_labels ="%y-%m-%d",limits =c(dat$date[(720*(i-1)+1)],dat$date[min(720*i,nrow(dat))]))+
    #   geom_hline(aes(yintercept=35),linetype = 'dashed')+
    #   mytheme_y+theme(legend.position =c(0.08,0.8))
    # ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/episodesPM25',i,'.png'),units="in",width=12, height=2.3, dpi=300)
    # 
    # ggplot(data=dat[(720*(i-1)+1):min(720*i,nrow(dat)),],aes(x=date,y=INSws,color=w_direction,group=1))+
    #   geom_line(size=1,show.legend = T)+
    #   #scale_x_datetime(date_breaks ="1 day",date_labels ="%Y-%m-%d %H:%M")+
    #   scale_x_datetime(date_breaks ="1 day",date_labels ="%y-%m-%d",limits =c(dat$date[(720*(i-1)+1)],dat$date[min(720*i,nrow(dat))]))+
    #   scale_color_manual(values=c("#9900CC",'#009966','#FFCC33'))+labs(x='Date',y='CNWS (CSWS)')+
    #   geom_hline(aes(yintercept=be_INws),linetype = 'dashed',color='#996666')+
    #   mytheme_year+theme(legend.position =c(0.08,0.7))
    # ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/episodesCNSWS',i,'.png'),units="in",width=12, height=2.3, dpi=300)
    
    ggplot(data=dat[(720*(i-1)+1):min(720*i,nrow(dat)),],aes(x=date,y=av_PM2.5,color=seperation,group=1))+
      geom_line(size=1,show.legend = T)+
      #scale_x_datetime(date_breaks ="1 day",date_labels ="%Y-%m-%d %H:%M")+
      scale_x_datetime(date_breaks ="1 day",date_labels ="%y-%m-%d",limits =c(dat$date[(720*(i-1)+1)],dat$date[min(720*i,nrow(dat))]))+
      geom_hline(aes(yintercept=35),linetype = 'dashed')+
      geom_hline(aes(yintercept=be_INws),linetype = 'dashed',color='#996666')+
      geom_line(data=dat[(720*(i-1)+1):min(720*i,nrow(dat)),],aes(x=date,y=INSws,color=w_direction,group=1),size=0.9,show.legend = T)+
      scale_color_manual(values=c('#F8766D','#009966',"#9900CC",'#00BFC4','#FFCC33'))+
      mytheme_x+theme(legend.position =c(0.08,0.8))
    ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/episodes',i,'.png'),units="in",width=12, height=4, dpi=300)
    
  }
  # ggplot(data=dat[(720*(j-1)+1):min(720*j,nrow(dat)),],aes(x=date,y=av_PM2.5,color=seperation,group=1))+
  #   geom_line(size=1,show.legend = T)+
  #   #scale_x_datetime(date_breaks ="1 day",date_labels ="%Y-%m-%d %H:%M")+
  #   scale_x_datetime(date_breaks ="1 day",date_labels ="%y-%m-%d",limits =c(dat$date[720*(j-1)+1],dat$date[720*(j-1)+1]+671*3600))+
  #   geom_hline(aes(yintercept=35),linetype = 'dashed')+
  #   mytheme_y+theme(legend.position =c(0.08,0.8))
  # ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/episodesPM25',j,'.png'),units="in",width=12, height=2.3, dpi=300)
  # 
  # ggplot(data=dat[(720*(j-1)+1):min(720*j,nrow(dat)),],aes(x=date,y=INSws,color=w_direction,group=1))+
  #   geom_line(size=1,show.legend = T)+
  #   #scale_x_datetime(date_breaks ="1 day",date_labels ="%Y-%m-%d %H:%M")+
  #   scale_x_datetime(date_breaks ="1 day",date_labels ="%y-%m-%d",limits =c(dat$date[720*(j-1)+1],dat$date[720*(j-1)+1]+671*3600))+
  #   scale_color_manual(values=c("#9900CC",'#009966','#FFCC33'))+labs(x='Date',y='CNWS (CSWS)')+
  #   geom_hline(aes(yintercept=be_INws),linetype = 'dashed',color='#996666')+
  #   mytheme_year+theme(legend.position =c(0.08,0.7))
  # ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/episodesCNSWS',j,'.png'),units="in",width=12, height=2.3, dpi=300)
  
  ggplot(data=dat[(720*(j-1)+1):min(720*j,nrow(dat)),],aes(x=date,y=av_PM2.5,color=seperation,group=1))+
    geom_line(size=1,show.legend = T)+
    #scale_x_datetime(date_breaks ="1 day",date_labels ="%Y-%m-%d %H:%M")+
    scale_x_datetime(date_breaks ="1 day",date_labels ="%y-%m-%d",limits =c(dat$date[720*(j-1)+1],dat$date[720*(j-1)+1]+671*3600))+
    geom_hline(aes(yintercept=35),linetype = 'dashed')+
    geom_hline(aes(yintercept=be_INws),linetype = 'dashed',color='#996666')+
    geom_line(data=dat[(720*(j-1)+1):min(720*j,nrow(dat)),],aes(x=date,y=INSws,color=w_direction,group=1),size=0.9,show.legend = T)+
    scale_color_manual(values=c('#F8766D','#009966',"#9900CC",'#00BFC4','#FFCC33'))+
    mytheme_x+theme(legend.position =c(0.08,0.8))
  ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/episodes',j,'.png'),units="in",width=12, height=4, dpi=300)
  
}

data_epglobal=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesmark/Dongsi2013winter_epm.csv",stringsAsFactors = F)
dat=data_epglobal
plotting_episode(dat)

csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin",
          "Shierzhong","Wuziju","Leidazhan",
          "Huadianerqu","Youyongguan","Jiancezhan")
csnames=matrix(csnames,ncol=3,byrow = T)

ep_len=c()
clean_begin=c()
statistic=c()
for(ss in 1:length(seasons))
{
    for(ii in 1:nrow(csnames))
    {
      sites=csnames[ii,]
      statses=c()
      gaps=c()
      for(jj in 1:length(sites))
      {
        for(yy in 1:length(years)){
          stats=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat/',sites[jj],years[yy],seasons[ss],'_epstat.csv'),stringsAsFactors = F)
          stats$site=sites[jj]
          statses=rbind(statses,stats)
          gaps=c(gaps,stats$begin_X[2:nrow(stats)]-stats$en_X[1:(nrow(stats)-1)])
        }
      }
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
statistic=arrange(statistic,sites,season)
statistic$length=paste0(round(statistic$length.m-1,1),'(',round(statistic$length.sd/sqrt(statistic$count),1),')')
statistic$range=paste0(round(statistic$range.m,1),'(',round(statistic$range.sd/sqrt(statistic$count),1),')')
statistic$lag=paste0(round(statistic$lag.m,1),'(',round(statistic$lag.sd/sqrt(statistic$count),1),')')
statistic$gap=paste0(round(statistic$gap.m,1),'(',round(statistic$gap.sd/sqrt(statistic$count),1),')')
statistic[,c('length.25','length.50','length.75')]=apply(statistic[,c('length.25','length.50','length.75')]-1,2,as.character)
#Table 1
print(xtable(statistic[,c('sites','season','count','day','night','length','length.25','length.50','length.75','range','lag','gap')],
             caption=paste0('statistics')), include.rownames =F)

colnames(clean_begin)=c('season','site','gap')
clean_begin=as.data.frame(clean_begin)
clean_begin$site=factor(clean_begin$site,levels=1:4,labels=c('Bejing SE','Bejing NW','Tangshan','Baoding'))
clean_begin$season=factor(clean_begin$season,levels=1:4,labels=c('spring','summer','autumn','winter'))
ggplot(clean_begin, aes(x=gap, color=season)) + 
  #geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.2)+ facet_wrap(.~ site,ncol=4)+
  labs(x='Time lag', y='Density')+mytheme 

ggplot(clean_begin, aes(x=gap, color=season)) +
  geom_histogram(fill="white", position="dodge",bins=length(unique(clean_begin$gap)))+
  facet_wrap(.~ site,ncol=4)+
  labs(x='Time lag', y='Count')+mytheme 

# ggplot(data=clean_begin, aes(x=gap, fill=season)) +
#   geom_bar(aes(x =gap, y = ..prop.., group = season), position=position_dodge())+
#   # scale_fill_manual(values=c('spring'='#CC0033','summer'='#FFCC33','autumn'='#009900','winter'='#006699'))+
#   scale_fill_manual(values=c('spring'='#9966CC','summer'='#CC0033','autumn'='#FFCC33','winter'='#006699'))+
#   facet_wrap(.~ site,ncol=4)+
#   labs(x='The time lag between the end of the cleaning  and  the starting time of the episode', y='Count')+scale_x_continuous(breaks=((-8):8))+
#   mytheme+theme(legend.position = 'top') 
# 
# ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/gap.png'),units="in",width=11, height=4, dpi=300)

#Figure S4
for(ss in seasons){
  ggplot(data=clean_begin[clean_begin$season==ss,], aes(x=gap)) +
    geom_bar(aes(x =gap, y = ..prop.., group = 1), position=position_dodge())+
    # scale_fill_manual(values=c('spring'='#CC0033','summer'='#FFCC33','autumn'='#009900','winter'='#006699'))+
    scale_fill_manual(values=c('spring'='#9966CC','summer'='#CC0033','autumn'='#FFCC33','winter'='#006699'))+
    facet_wrap(.~ site,ncol=4)+ylim(0,0.25)+
    labs(x=TeX('$t_{s}-t_{\\omega}$'), y='Frequency')+scale_x_continuous(breaks=c(-8,-6,-4,-2,0,2,4,6,8))+
    mytheme+theme(legend.position = 'top') 
  
  ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/gap',ss,'.png'),units="in",width=10, height=3, dpi=300)
  
}

colnames(ep_len)=c('season','site','len')
ep_len=as.data.frame(ep_len)
ep_len$site=factor(ep_len$site,levels=1:4,labels=c('Bejing SE','Bejing NW','Tangshan','Baoding'))
ep_len$season=factor(ep_len$season,levels=1:4,labels=c('spring','summer','autumn','winter'))
ggplot(ep_len, aes(x=len, color=season)) + 
  #geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.2)+ facet_wrap(.~ site,ncol=4)+
  labs(x='Time lag', y='Density')+mytheme 

ggplot(ep_len, aes(x=len, color=season)) +
  geom_histogram(fill="white", position="dodge")+
  facet_wrap(.~ site,ncol=4)+
  labs(x='Time lag', y='Count')+mytheme 

# ggplot(data=ep_len, aes(x=len, fill=season)) +
#   geom_bar(aes(x =len, y = ..prop.., group = season), position=position_dodge())+
#   # scale_fill_manual(values=c('spring'='#CC0033','summer'='#FFCC33','autumn'='#009900','winter'='#006699'))+
#   scale_fill_manual(values=c('spring'='#9966CC','summer'='#CC0033','autumn'='#FFCC33','winter'='#006699'))+
#   facet_wrap(.~ site,ncol=4)+
#   labs(x='The time lag between the end of the cleaning  and  the starting time of the episode', y='Count')+
#   mytheme+theme(legend.position = 'top')
# 
# ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/len.png'),units="in",width=11, height=4, dpi=300)

for(ss in seasons){
  ggplot(data=ep_len[ep_len$season==ss,], aes(x=len)) +
    geom_bar(aes(x =len, y = ..prop.., group = 1), position=position_dodge())+
    # scale_fill_manual(values=c('spring'='#CC0033','summer'='#FFCC33','autumn'='#009900','winter'='#006699'))+
    scale_fill_manual(values=c('spring'='#9966CC','summer'='#CC0033','autumn'='#FFCC33','winter'='#006699'))+
    facet_wrap(.~ site,ncol=4)+ylim(0,0.25)+
    labs(x='Length', y='Frequency')+
    mytheme+theme(legend.position = 'top') 
  
  ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/len',ss,'.png'),units="in",width=10, height=3, dpi=300)
  
}

