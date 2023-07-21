city=c('Beijing','Beijing','Tangshan','Baoding')
dat_all=c()
for(year in years)
{
  for(season in seasons)
  {
    for(i in 1:length(rnames))
    {
      clu=rnames[i]
      dat=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat/',clu,year,season,'_epstat.csv'),stringsAsFactors = F)
      dats=dat[,c('X',"epl","begin_hour","en_hour","clean_hour","begin_X","en_X","clean_X","loca")]
      dats$syear=year
      dats$season=season
      dats$loca=paste0(city[i],'_',dats$loca)
      dats$cluster=clu
      dat_all=rbind(dat_all,dats)}
  }
}

lag_calculate <- function(a,values)
{
  return(a-values[which.min(abs(values-a))])
}

stat_lag=c()
episode_BJ=dat_all[dat_all$cluster%in%c("DongsiTiantanNongzhanguan","GuanyuanWanliuAotizhongxin"),]

for(year in years)
{
  for(season in seasons)
  {
    for(clu in rnames[3:4])
    {
      data=dat_all[dat_all$cluster==clu&dat_all$syear==year&dat_all$season==season,]   
      lags=data.frame(cluster=clu,season=season,syear=year,lag=sapply(data$begin_X,lag_calculate,episode_BJ$begin_X))
      stat_lag=rbind(stat_lag,lags)
    }
  }
}
count_early_1=as.data.frame(stat_lag[stat_lag$lag> -24&stat_lag$lag <=0,]%>%group_by(cluster,season)%>%
                              summarise(c_early_1=n(),m_early_1=mean(lag,na.rm=T),se_early_1=sd(lag,na.rm=T)))
count_early_3=as.data.frame(stat_lag[stat_lag$lag<= -48,]%>%group_by(cluster,season)%>%
                          summarise(c_early_3=n(),m_early_3=mean(lag,na.rm=T),se_early_3=sd(lag,na.rm=T)))
count_early_2=as.data.frame(stat_lag[stat_lag$lag> -48&stat_lag$lag<= -24,]%>%group_by(cluster,season)%>%
                          summarise(c_early_2=n(),m_early_2=mean(lag,na.rm=T),se_early_2=sd(lag,na.rm=T)))
count_later_1=as.data.frame(stat_lag[stat_lag$lag> 0&stat_lag$lag<= 24,]%>%group_by(cluster,season)%>%
                          summarise(c_later_1=n(),m_later_1=mean(lag,na.rm=T),se_later_1=sd(lag,na.rm=T)))
count_later_2=as.data.frame(stat_lag[stat_lag$lag> 24&stat_lag$lag<= 48,]%>%group_by(cluster,season)%>%
                          summarise(c_later_2=n(),m_later_2=mean(lag,na.rm=T),se_later_2=sd(lag,na.rm=T)))
count_later_3=as.data.frame(stat_lag[stat_lag$lag>48,]%>%group_by(cluster,season)%>%
                          summarise(c_later_3=n(),m_later_3=mean(lag,na.rm=T),se_later_3=sd(lag,na.rm=T)))
                                                          
count_early_1$se_early_1=count_early_1$se_early_1/sqrt(count_early_1$c_early_1)
count_early_2$se_early_2=count_early_2$se_early_2/sqrt(count_early_2$c_early_2)
count_early_3$se_early_3=count_early_3$se_early_3/sqrt(count_early_3$c_early_3)
count_later_3$se_later_3=count_later_3$se_later_3/sqrt(count_later_3$c_later_3)
count_later_2$se_later_2=count_later_2$se_later_2/sqrt(count_later_2$c_later_2)
count_later_1$se_later_1=count_later_1$se_later_1/sqrt(count_later_1$c_later_1)

counts=left_join(left_join(left_join(left_join(left_join(count_early_1,count_early_2),count_early_3),count_later_1),count_later_2),count_later_3)
counts[,c('c_early_1','c_early_2','c_early_3','c_later_1','c_later_2','c_later_3')][is.na(counts[,c('c_early_1','c_early_2','c_early_3','c_later_1','c_later_2','c_later_3')])]=0
counts$Total=apply(counts[,c('c_early_1','c_early_2','c_early_3','c_later_1','c_later_2','c_later_3')],1,sum,na.rm=T)

counts$cluster=factor(counts$cluster,levels=c("ShierzhongWuzijuLeidazhan","HuadianerquYouyongguanJiancezhan"),
                         labels=c('Tangshan','Baoding'))
prin=counts
prin[,c('Total','c_early_1','c_early_2','c_early_3','c_later_1','c_later_2','c_later_3')]=
  apply(round(prin[,c('Total','c_early_1','c_early_2','c_early_3','c_later_1','c_later_2','c_later_3')]),
        2,as.character)
# prin[,c('Total','c_early_1','c_early_2','c_early_3','c_later_1','c_later_2','c_later_3')]=
#   apply(round(prin[,c('Total','c_early_1','c_early_2','c_early_3','c_later_1','c_later_2','c_later_3')]/length(years)/3),
#         2,as.character)
prin$early_1=paste0(round(prin$m_early_1,1),'(',round(prin$se_early_1,1),')')
prin$early_2=paste0(round(prin$m_early_2,1),'(',round(prin$se_early_2,1),')')
prin$early_3=paste0(round(prin$m_early_3,1),'(',round(prin$se_early_3,1),')')
prin$later_1=paste0(round(prin$m_later_1,1),'(',round(prin$se_later_1,1),')')
prin$later_2=paste0(round(prin$m_later_2,1),'(',round(prin$se_later_2,1),')')
prin$later_3=paste0(round(prin$m_later_3,1),'(',round(prin$se_later_3,1),')')

print(xtable(prin[,c("cluster","season","Total","c_early_3","early_3","c_early_2","early_2","c_early_1","early_1",
                     "c_later_1","later_1","c_later_2","later_2","c_later_3","later_3")]),include.rownames = F)

stat_lag$season=factor(stat_lag$season,levels=seasons,labels=c("Spring","Summer","Autumn","Winter"))
p1=ggplot(data=stat_lag[stat_lag$cluster=="ShierzhongWuzijuLeidazhan",], aes(x=lag)) +
  geom_bar(aes(x =lag, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='Lag (hours)',y='Frequency',title='(a) Tangshan')+
  scale_x_continuous(breaks = c(-168,-144,-120,-96,-72,-48,-24,0,24,48,72,96,120,144,168))+
  theme_bw()+theme(axis.title = element_text(size = 10), 
                   text = element_text(face = "bold"),
                   strip.text = element_text(size = 10,face = 'bold'),
                   strip.background = element_rect(color="black", fill="white", linetype="solid"),
                   #axis.title.x = element_blank(),
                   #axis.title.y = element_blank(),
                   axis.text.x = element_text(size=10, angle=45,hjust = 0.5,vjust = 0.5,face = 'bold'),
                   axis.text.y = element_text(size=10, face = 'bold'),
                   plot.title = element_text(size=15, face = 'bold', hjust = 0.5, vjust = 0.5),
                   legend.title = element_blank(),
                   legend.text = element_text(size=10, face = 'bold'),
                   legend.key.width  = unit(.3,"inches"),
                   legend.key.height = unit(.3,"inches"),
                   panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

p2=ggplot(data=stat_lag[stat_lag$cluster=="HuadianerquYouyongguanJiancezhan",], aes(x=lag)) +
  geom_bar(aes(x =lag, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='Lag (hours)',y='Frequency',title='(b) Baoding')+
  scale_x_continuous(breaks = c(-168,-144,-120,-96,-72,-48,-24,0,24,48,72,96,120,144,168))+
  theme_bw()+theme(axis.title = element_text(size = 10), 
                   text = element_text(face = "bold"),
                   strip.text = element_text(size = 10,face = 'bold'),
                   strip.background = element_rect(color="black", fill="white", linetype="solid"),
                   #axis.title.x = element_blank(),
                   #axis.title.y = element_blank(),
                   axis.text.x = element_text(size=10, angle=45,hjust = 0.5,vjust = 0.5,face = 'bold'),
                   axis.text.y = element_text(size=10, face = 'bold'),
                   plot.title = element_text(size=15, face = 'bold', hjust = 0.5, vjust = 0.5),
                   legend.title = element_blank(),
                   legend.text = element_text(size=10, face = 'bold'),
                   legend.key.width  = unit(.3,"inches"),
                   legend.key.height = unit(.3,"inches"),
                   panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

fig_lag=ggarrange(p1,p2,nrow=2,ncol=1)
ggsave(fig_lag,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_try/Lag.png'),units="in",width=14, height=7, dpi=350)
#-----------
intervals<- function(a,interval=24)
{
  return((a-interval):(a-1))
}

WDs=c()
WDs_1=c()
for(loca in unique(dat_all$loca))
{
  daNS_s=read.csv(paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/NSdata_update_new/",loca,'_NS.csv'), header = TRUE)
  for(season in seasons)
  {
    #672 is the difference of oX amd X in Beijing_Dongsi_cleaned.csv
    WD=data.frame(cbwd=daNS_s$cbwd[daNS_s$X%in%(as.vector(sapply(dat_all$begin_X[dat_all$loca==loca&dat_all$season==season],intervals,interval=24))+672)])
    WD$season=season
    WD$loca=loca
    WDs=rbind(WDs,WD)
    WD_1=data.frame(cbwd=daNS_s$cbwd[daNS_s$X%in%(as.vector(sapply(dat_all$begin_X[dat_all$loca==loca&dat_all$season==season],intervals,interval=12))+672)])
    WD_1$season=season
    WD_1$loca=loca
    WDs_1=rbind(WDs_1,WD_1)
  }
}
WDs$season=factor(WDs$season,levels=seasons,labels=c('Spring','Summer','Autumn','Winter'))
WDs_1$season=factor(WDs_1$season,levels=seasons,labels=c('Spring','Summer','Autumn','Winter'))

p1=ggplot(data=WDs[WDs$loca%in%c("Beijing_Dongsi","Beijing_Tiantan","Beijing_Nongzhanguan")&!is.na(WDs$cbwd),], aes(x=cbwd)) +
  geom_bar(aes(x =cbwd, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='cbwd',y='Frequency',title='(a) Beijng SE')+
  mytheme

p2=ggplot(data=WDs[WDs$loca%in%c("Beijing_Guanyuan","Beijing_Wanliu","Beijing_Aotizhongxin")&!is.na(WDs$cbwd),], aes(x=cbwd)) +
  geom_bar(aes(x =cbwd, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='cbwd',y='Frequency',title='(b) Beijng NW')+
  mytheme

p3=ggplot(data=WDs[WDs$loca%in%c("Tangshan_Shierzhong","Tangshan_Wuziju","Tangshan_Leidazhan")&!is.na(WDs$cbwd),], aes(x=cbwd)) +
  geom_bar(aes(x =cbwd, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='cbwd',y='Frequency',title='(c) Tangshan')+
  mytheme

p4=ggplot(data=WDs[WDs$loca%in%c("Baoding_Huadianerqu","Baoding_Youyongguan","Baoding_Jiancezhan")&!is.na(WDs$cbwd),], aes(x=cbwd)) +
  geom_bar(aes(x =cbwd, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='cbwd',y='Frequency',title='(d) Baoding')+
  mytheme

WD_24=ggarrange(p1,p2,p3,p4,nrow=4,ncol=1)
ggsave(WD_24,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_try/WD_24.png'),units="in",width=8, height=9, dpi=350)

p1=ggplot(data=WDs_1[WDs_1$loca%in%c("Beijing_Dongsi","Beijing_Tiantan","Beijing_Nongzhanguan")&!is.na(WDs_1$cbwd),], aes(x=cbwd)) +
  geom_bar(aes(x =cbwd, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='cbwd',y='Frequency',title='(a) Beijng SE')+
  mytheme

p2=ggplot(data=WDs_1[WDs_1$loca%in%c("Beijing_Guanyuan","Beijing_Wanliu","Beijing_Aotizhongxin")&!is.na(WDs_1$cbwd),], aes(x=cbwd)) +
  geom_bar(aes(x =cbwd, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='cbwd',y='Frequency',title='(b) Beijng NW')+
  mytheme

p3=ggplot(data=WDs_1[WDs_1$loca%in%c("Tangshan_Shierzhong","Tangshan_Wuziju","Tangshan_Leidazhan")&!is.na(WDs_1$cbwd),], aes(x=cbwd)) +
  geom_bar(aes(x =cbwd, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='cbwd',y='Frequency',title='(c) Tangshan')+
  mytheme

p4=ggplot(data=WDs_1[WDs_1$loca%in%c("Baoding_Huadianerqu","Baoding_Youyongguan","Baoding_Jiancezhan")&!is.na(WDs_1$cbwd),], aes(x=cbwd)) +
  geom_bar(aes(x =cbwd, y = ..prop.., group = 1), position=position_dodge())+
  facet_wrap(.~ season,ncol=4,scales='fix')+labs(x='cbwd',y='Frequency',title='(d) Baoding')+
  mytheme

WD_12=ggarrange(p1,p2,p3,p4,nrow=4,ncol=1)
ggsave(WD_12,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_try/WD_12.png'),units="in",width=8, height=9, dpi=350)


