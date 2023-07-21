#------------Figure 1----------------------
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
year2=2018#year2=2019
years=year1:year2

#--------------Figure3--------------------
mytheme=theme_bw()+theme(axis.title = element_text(size = 10), 
                         text = element_text(face = "bold"),
                         strip.text = element_text(size = 10,face = 'bold'),
                         strip.background = element_rect(color="black", fill="white", linetype="solid"),
                         #axis.title.x = element_blank(),
                         #axis.title.y = element_blank(),
                         axis.text.x = element_text(size=10,hjust = 0.5, face = 'bold'),
                         axis.text.y = element_text(size=10, face = 'bold'),
                         plot.title = element_text(size=13, face = 'bold', hjust = 0.5, vjust = 0.5),
                         legend.title = element_blank(),
                         legend.text = element_text(size=10, face = 'bold'),
                         legend.key.width  = unit(.3,"inches"),
                         legend.key.height = unit(.3,"inches"),
                         panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

mytheme_ytitle=theme_bw()+theme(axis.title = element_text(size = 10), 
                                text = element_text(face = "bold"),
                                strip.text = element_text(size = 10,face = 'bold'),
                                strip.background = element_rect(color="black", fill="white", linetype="solid"),
                                axis.text.x = element_text(size=10,hjust = 0.5, face = 'bold'),
                                axis.text.y = element_text(size=10, face = 'bold'),
                                axis.title.x = element_blank(),
                                #axis.title.y = element_blank(),
                                plot.title = element_blank(),
                                #legend.title = element_blank(),
                                legend.text = element_text(size=10, face = 'bold'),
                                legend.key.width  = unit(.3,"inches"),
                                legend.key.height = unit(.3,"inches"),
                                panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

mytheme_null=theme_bw()+theme(axis.title = element_text(size = 10), 
                              text = element_text(face = "bold"),
                              strip.text = element_text(size = 15,face = 'bold'),
                              strip.background = element_rect(color="black", fill="white", linetype="solid"),
                              axis.text.x = element_text(size=13,hjust = 0.5, face = 'bold'),
                              axis.text.y = element_text(size=15, face = 'bold'),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_text(size=15,hjust = 0.5, face = 'bold'),
                              legend.title = element_blank(),
                              legend.text = element_text(size=10, face = 'bold'),
                              legend.key.width  = unit(.3,"inches"),
                              legend.key.height = unit(.3,"inches"),
                              panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

mytheme_null1=theme_bw()+theme(axis.title = element_text(size = 10), 
                               text = element_text(face = "bold"),
                               strip.text = element_text(size = 15,face = 'bold'),
                               strip.background = element_rect(color="black", fill="white", linetype="solid"),
                               axis.text.x = element_text(size=10,hjust = 0.5, face = 'bold'),
                               axis.text.y = element_text(size=10, face = 'bold'),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               plot.title = element_text(size=15,hjust = 0.5, face = 'bold'),
                               legend.title = element_blank(),
                               legend.text = element_text(size=10, face = 'bold'),
                               legend.key.width  = unit(.3,"inches"),
                               legend.key.height = unit(.3,"inches"),
                               panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

hour_try=5

ad_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/PM25adjusted.csv",stringsAsFactors = F)
ad_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/NO2adjusted.csv",stringsAsFactors = F)
ad_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/SO2adjusted.csv",stringsAsFactors = F)
ad_PM$Pollutant='PM2.5'
ad_NO$Pollutant='NO2'
ad_SO$Pollutant='SO2'
ad_PNS=rbind(ad_PM,ad_NO,ad_SO)
pps=c('PM2.5','NO2','SO2')
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

pp=1; u=2
{
  i=1
  p1=ggplot(data=ad_PNSs[ad_PNSs$Pollutant==pps[pp]&ad_PNSs$site==rnames[u]&ad_PNSs$season==seasons[i],])+
    geom_line(aes(x=orde, y=value,group=type,color=type))+
    geom_ribbon(data=ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
    scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
    facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNSs$year)), scales = "fixed") + 
    scale_color_manual(values=c('origin'='red','adjusted'='blue'))+
    labs(x = "Hour", y = expression(bold('Average Growth'~"("*mu*"g/"*m^3*")")),title='(a) Spring')+mytheme
  i=2
  p2=ggplot(data=ad_PNSs[ad_PNSs$Pollutant==pps[pp]&ad_PNSs$site==rnames[u]&ad_PNSs$season==seasons[i],])+
    #geom_point(size=0.5,color="blue")+
    geom_line(aes(x=orde, y=value,group=type,color=type))+
    geom_ribbon(data=ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
    scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
    facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNSs$year)), scales = "fixed") + 
    scale_color_manual(values=c('origin'='red','adjusted'='blue'))+
    labs(x = "Hour", y = expression(bold('Average Growth'~"("*mu*"g/"*m^3*")")),title='(b) Summer')+mytheme
  i=3
  p3=ggplot(data=ad_PNSs[ad_PNSs$Pollutant==pps[pp]&ad_PNSs$site==rnames[u]&ad_PNSs$season==seasons[i],])+
    #geom_point(size=0.5,color="blue")+
    geom_line(aes(x=orde, y=value,group=type,color=type))+
    geom_ribbon(data=ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
    scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
    facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNSs$year)), scales = "fixed") + 
    scale_color_manual(values=c('origin'='red','adjusted'='blue'))+
    labs(x = "Hour", y = expression(bold('Average Growth'~"("*mu*"g/"*m^3*")")),title='(c) Autumn')+mytheme
  i=4
  p4=ggplot(data=ad_PNSs[ad_PNSs$Pollutant==pps[pp]&ad_PNSs$site==rnames[u]&ad_PNSs$season==seasons[i],])+
    #geom_point(size=0.5,color="blue")+
    geom_line(aes(x=orde, y=value,group=type,color=type))+
    geom_ribbon(data=ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
    scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
    facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNSs$year)), scales = "fixed") + 
    scale_color_manual(values=c('origin'='red','adjusted'='blue'))+
    labs(x = "Hour", y = expression(bold('Average Growth'~"("*mu*"g/"*m^3*")")),title='(d) Winter')+mytheme
  figure3=ggarrange(p1, p2, p3,p4,ncol = 1, nrow = 4,common.legend=T,
                    legend='bottom')
  
  annotate_figure(figure3,bottom = text_grob(expression(bold(
    atop('Average hourly growth of'~PM[2.5]~'in the first six hours of the selected calm episodes','after sustained northerly cleaning in Beijing NW from 2013 to 2018'))), 
    face = "bold", size = 14))
  
  ggsave(filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_bld/abstract.png'),units="in",width=8, height=8.3, dpi=300)
  ggsave(filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_bld/abstract.pdf'),units="in",width=8, height=8.3, dpi=300)
}

for(u in 1:length(rnames))
{
  for(pp in 1:length(pps))
  {
    i=1
    p1=ggplot(data=ad_PNSs[ad_PNSs$Pollutant==pps[pp]&ad_PNSs$site==rnames[u]&ad_PNSs$season==seasons[i],])+
      geom_line(aes(x=orde, y=value,group=type,color=type))+
      geom_ribbon(data=ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
      scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
      facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNSs$year)), scales = "fixed") + 
      scale_color_manual(values=c('origin'='red','adjusted'='blue'))+
      labs(x = "Hour", y = expression(bold('Average Growth'~"("*mu*"g/"*m^3*")")),title='(a) Spring')+mytheme
    i=2
    p2=ggplot(data=ad_PNSs[ad_PNSs$Pollutant==pps[pp]&ad_PNSs$site==rnames[u]&ad_PNSs$season==seasons[i],])+
      #geom_point(size=0.5,color="blue")+
      geom_line(aes(x=orde, y=value,group=type,color=type))+
      geom_ribbon(data=ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
      scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
      facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNSs$year)), scales = "fixed") + 
      scale_color_manual(values=c('origin'='red','adjusted'='blue'))+
      labs(x = "Hour", y = expression(bold('Average Growth'~"("*mu*"g/"*m^3*")")),title='(b) Summer')+mytheme
    i=3
    p3=ggplot(data=ad_PNSs[ad_PNSs$Pollutant==pps[pp]&ad_PNSs$site==rnames[u]&ad_PNSs$season==seasons[i],])+
      #geom_point(size=0.5,color="blue")+
      geom_line(aes(x=orde, y=value,group=type,color=type))+
      geom_ribbon(data=ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
      scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
      facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNSs$year)), scales = "fixed") + 
      scale_color_manual(values=c('origin'='red','adjusted'='blue'))+
      labs(x = "Hour", y = expression(bold('Average Growth'~"("*mu*"g/"*m^3*")")),title='(c) Autumn')+mytheme
    i=4
    p4=ggplot(data=ad_PNSs[ad_PNSs$Pollutant==pps[pp]&ad_PNSs$site==rnames[u]&ad_PNSs$season==seasons[i],])+
      #geom_point(size=0.5,color="blue")+
      geom_line(aes(x=orde, y=value,group=type,color=type))+
      geom_ribbon(data=ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
      scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
      facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNSs$year)), scales = "fixed") + 
      scale_color_manual(values=c('origin'='red','adjusted'='blue'))+
      labs(x = "Hour", y = expression(bold('Average Growth'~"("*mu*"g/"*m^3*")")),title='(d) Winter')+mytheme
    figure3=ggarrange(p1, p2, p3,p4,ncol = 1, nrow = 4,common.legend=T,
                      legend='bottom')
    
    ggsave(figure3,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_bld/',pps[pp],rnames[u],'adjustedfacet.png'),units="in",width=8, height=8.3, dpi=300)
    
  }
}

#---------Figure4------
ad_PNS$season=factor(ad_PNS$season,levels = c('spring','summer','autumn','winter'),
                     labels=c('Spring','Summer','Autumn','Winter'))
ad_PNS$sitename=factor(ad_PNS$site,levels =rnames,
                       labels = c('Beijing SE','Beijing NW','Tangshan','Baoding'))

ad_PNS[ad_PNS$orde>1,c('adjusted','ad.lb','ad.ub')]=ad_PNS[ad_PNS$orde>1,c('adjusted','ad.lb','ad.ub')]/(ad_PNS$orde[ad_PNS$orde>1]-1)
# ad_PNS$ad.lb[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted<(0))]=ad_PNS$adjusted[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted<(0))]
# ad_PNS$ad.lb[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted>(0))]=-3.5
ad_PNS$ad.lb[which(ad_PNS$adjusted<(0))]=ad_PNS$adjusted[which(ad_PNS$adjusted<(0))]
ad_PNS$ad.lb[which(ad_PNS$ad.lb<(0)&ad_PNS$adjusted>=(0))]=0

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

pp ='PM2.5'
# min(ad_PNS$ad.lb[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp])
# max(ad_PNS$ad.ub[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp])

figure4a=ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
  geom_bar(stat='identity', position='dodge') +ylim(0,17)+
  geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
  mytheme +
  scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#66CCFF','#3366CC'))+
  #scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
  labs(x = 'Season',y =expression(bold('Average Growth Rate'~"("*mu*"g/"*m^3~"per hour)")), title = expression(bold('(a) ')*bold(PM[2.5])))+
  theme(legend.position = 'bottom')+guides(fill = guide_legend(nrow = 1))

#----------------------
# #Baoding 2013 is strange.
# ad_PNS[ad_PNS$Pollutant=="NO2"&ad_PNS$year==2013&ad_PNS$sitename=='Baoding'&ad_PNS$season=='Spring',c('adjusted','ad.lb','ad.ub')]=NA
# ad_PNS[ad_PNS$Pollutant=="SO2"&ad_PNS$year==2013&ad_PNS$sitename=='Baoding'&ad_PNS$season%in%c('Spring','Summer','Autumn'),c('adjusted','ad.lb','ad.ub')]=NA

pp ='NO2'
figure4b=ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
  geom_bar(stat='identity', position='dodge') +ylim(0,12.1)+
  geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
  mytheme +
  scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#66CCFF','#3366CC'))+
  #scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
  labs(x = 'Season',y =expression(bold('Average Growth Rate'~"("*mu*"g/"*m^3~"per hour)")), title = expression(bold('(b) ')*bold(NO[2])))+
  theme(legend.position = 'bottom')+guides(fill = guide_legend(nrow = 1))

#------------------------
pp='SO2'
data_line=ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp,]
data_line$hl=rep(0,nrow(data_line))
data_line$hl[data_line$sitename%in%c('Beijing SE','Beijing NW')]=6
data_line$hl[data_line$sitename%in%c('Tangshan','Baoding')]=30 #25

figure4c=ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  geom_hline(data=data_line,aes(yintercept =hl),color='white')+
  facet_wrap(.~ sitename,ncol=4, scales = "free_y") +
  mytheme +
  scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#66CCFF','#3366CC'))+
  #scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
  labs(x = 'Season',y =expression(bold('Average Growth Rate'~"("*mu*"g/"*m^3~"per hour)")), title = expression(bold('(c) ')*bold(SO[2])))+
  theme(legend.position = 'bottom')+guides(fill = guide_legend(nrow = 1))

figure4=ggarrange(figure4a, figure4b, figure4c,ncol = 1, nrow = 3,common.legend =T,legend ='bottom')
ggsave(figure4,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_bld/Figure4.png'),units="in",width=16, height=17.6, dpi=350)

#----------------Figure5-----------------------
d_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/PM25adjustedcompared.csv",stringsAsFactors = F)
d_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/NO2adjustedcompared.csv",stringsAsFactors = F)
d_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/SO2adjustedcompared.csv",stringsAsFactors = F)

arrange(as.data.frame(d_PM[d_PM$orde==eplen&d_PM$Years==2018,c('Seasons','Sites','rate')]%>%group_by(Sites)%>%summarise(ratemax=round(max(-rate),1))),Sites)
arrange(as.data.frame(d_PM[d_PM$orde==eplen&d_PM$Years==2018,c('Seasons','Sites','rate')]%>%group_by(Sites)%>%summarise(ratemin=round(min(-rate),1))),Sites)
arrange(as.data.frame(d_NO[d_NO$orde==eplen&d_NO$Years==2018,c('Seasons','Sites','rate')]%>%group_by(Sites)%>%summarise(ratemax=round(max(-rate),1))),Sites)
arrange(as.data.frame(d_NO[d_NO$orde==eplen&d_NO$Years==2018,c('Seasons','Sites','rate')]%>%group_by(Sites)%>%summarise(ratemin=round(min(-rate),1))),Sites)
arrange(as.data.frame(d_SO[d_SO$orde==eplen&d_SO$Years==2018,c('Seasons','Sites','rate')]%>%group_by(Sites)%>%summarise(ratemax=round(max(-rate),1))),Sites)
arrange(as.data.frame(d_SO[d_SO$orde==eplen&d_SO$Years==2018,c('Seasons','Sites','rate')]%>%group_by(Sites)%>%summarise(ratemin=round(min(-rate),1))),Sites)

d_PM$Pollutant='PM2.5'
d_NO$Pollutant='NO2'
d_SO$Pollutant='SO2'
d_PNS=rbind(d_PM,d_NO,d_SO)

p.compare=d_PNS
p.compare$Seasons=factor(p.compare$Seasons,levels = c('spring','summer','autumn','winter'),
                         labels=str_to_title(c('spring','summer','autumn','winter')))
p.compare$Pollutant=factor(p.compare$Pollutant,levels = c('NO2','PM2.5','SO2'))
p.compare$significance=factor(p.compare$significance,levels = c('FALSE','TRUE'),labels=c('Nonsignificant','Significant'))
pd <- position_dodge(0.56)
ddd=1
figure5a=ggplot(p.compare[p.compare$Sites==rnames[ddd]&p.compare$orde==eplen,],aes(x=Years,y=Estimates/(orde-1),color=Pollutant))+
  geom_point(aes(x=Years,y=Estimates/(orde-1),color=Pollutant,shape=significance,size=significance),position=pd)+geom_line(position=pd,size=0.4) +
  geom_errorbar(aes(x =Years, ymin=(Estimates-qnorm(1-0.05/2)*SE)/(orde-1),
                    ymax=(Estimates+qnorm(1-0.05/2)*SE)/(orde-1)), alpha = 1, width = 0.25, position=pd)+
  geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
  facet_wrap(.~ Seasons,ncol=4, scales = "fixed")+
  scale_x_continuous(breaks=years[-1])+
  scale_shape_manual(values=c(20,8))+
  scale_color_manual(values=c('#CC0000',"#009966",'#3366CC'),labels=c(expression(bold(NO[2])),expression(bold(PM[2.5])),expression(bold(SO[2]))))+
  scale_size_manual(values=c(2,2.5))+
  mytheme+
  #mytheme_null1 +
  labs(x = 'Year',y =paste0('Seasonal Differences'), title = paste0('(a) Beijing SE'))
ddd=2
figure5b=ggplot(p.compare[p.compare$Sites==rnames[ddd]&p.compare$orde==eplen,],aes(x=Years,y=Estimates/(orde-1),color=Pollutant))+
  geom_point(aes(x=Years,y=Estimates/(orde-1),color=Pollutant,shape=significance,size=significance),position=pd)+geom_line(position=pd,size=0.4) +
  geom_errorbar(aes(x =Years, ymin=(Estimates-qnorm(1-0.05/2)*SE)/(orde-1),
                    ymax=(Estimates+qnorm(1-0.05/2)*SE)/(orde-1)), alpha = 1, width = 0.25, position=pd)+
  geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
  facet_wrap(.~ Seasons,ncol=4, scales = "fixed")+
  scale_x_continuous(breaks=years[-1])+
  scale_shape_manual(values=c(20,8))+
  scale_color_manual(values=c('#CC0000',"#009966",'#3366CC'),labels=c(expression(bold(NO[2])),expression(bold(PM[2.5])),expression(bold(SO[2]))))+
  scale_size_manual(values=c(2,2.5))+
  mytheme+
  #mytheme_null1 +
  labs(x = 'Year',y =paste0('Seasonal Differences'), title = paste0('(b) Beijing NW'))

ddd=3
figure5c=ggplot(p.compare[p.compare$Sites==rnames[ddd]&p.compare$orde==eplen,],aes(x=Years,y=Estimates/(orde-1),color=Pollutant))+
  geom_point(aes(x=Years,y=Estimates/(orde-1),color=Pollutant,shape=significance,size=significance),position=pd)+geom_line(position=pd,size=0.4) +
  geom_errorbar(aes(x =Years, ymin=(Estimates-qnorm(1-0.05/2)*SE)/(orde-1),
                    ymax=(Estimates+qnorm(1-0.05/2)*SE)/(orde-1)), alpha = 1, width = 0.25, position=pd)+
  geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
  facet_wrap(.~ Seasons,ncol=4, scales = "fixed")+
  scale_x_continuous(breaks=years[-1])+
  scale_shape_manual(values=c(20,8))+
  scale_color_manual(values=c('#CC0000',"#009966",'#3366CC'),labels=c(expression(bold(NO[2])),expression(bold(PM[2.5])),expression(bold(SO[2]))))+
  scale_size_manual(values=c(2,2.5))+
  mytheme+
  #mytheme_null1 +
  labs(x = 'Year',y =paste0('Seasonal Differences'), title = paste0('(c) Tangshan'))

ddd=4
figure5d=ggplot(p.compare[p.compare$Sites==rnames[ddd]&p.compare$orde==eplen,],aes(x=Years,y=Estimates/(orde-1),color=Pollutant))+
  geom_point(aes(x=Years,y=Estimates/(orde-1),color=Pollutant,shape=significance,size=significance),position=pd)+geom_line(position=pd,size=0.4) +
  geom_errorbar(aes(x =Years, ymin=(Estimates-qnorm(1-0.05/2)*SE)/(orde-1),
                    ymax=(Estimates+qnorm(1-0.05/2)*SE)/(orde-1)), alpha = 1, width = 0.25, position=pd)+
  geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
  facet_wrap(.~ Seasons,ncol=4, scales = "fixed")+
  scale_x_continuous(breaks=years[-1])+
  scale_shape_manual(values=c(20,8))+
  scale_color_manual(values=c('#CC0000',"#009966",'#3366CC'),labels=c(expression(bold(NO[2])),expression(bold(PM[2.5])),expression(bold(SO[2]))))+
  scale_size_manual(values=c(2,2.5))+
  mytheme+
  #mytheme_null1 +
  labs(x = 'Year',y =paste0('Seasonal Differences'), title = paste0('(d) Baoding'))

figure5=ggarrange(figure5a, figure5b, figure5c, figure5d,ncol = 1, nrow = 4,common.legend =T,legend ='bottom')
ggsave(figure5,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_bld/Figure5.png'),units="in",width=11, height=11.3, dpi=300)

#----------------FigureS10-S12--------------------
mytheme_null_year=theme_bw()+theme(axis.title = element_text(size = 10), 
                                   text = element_text(face = "bold"),
                                   strip.text = element_text(size = 12,face = 'bold'),
                                   strip.background = element_rect(color="black", fill="white", linetype="solid"),
                                   axis.text.x = element_text(size=10, angle=45,hjust = 0.5, vjust=0.5,face = 'bold'),
                                   axis.text.y = element_text(size=10, face = 'bold'),
                                   axis.title.x = element_blank(),
                                   axis.title.y = element_blank(),
                                   plot.title = element_text(size=15,hjust = 0.5, face = 'bold'),
                                   legend.title = element_blank(),
                                   legend.text = element_text(size=10, face = 'bold'),
                                   legend.key.width  = unit(.3,"inches"),
                                   legend.key.height = unit(.3,"inches"),
                                   panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

coef_PM=read.csv('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/PM25coefficients.csv',stringsAsFactors = F)
coef_PM$pollu='PM25'
coef_SO=read.csv('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/SO2coefficients.csv',stringsAsFactors = F)
coef_SO$pollu='SO2'
coef_NO=read.csv('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/NO2coefficients.csv',stringsAsFactors = F)
coef_NO$pollu='NO2'
coefs=rbind(coef_PM,coef_SO,coef_NO)
coefs$Name=as.factor(coefs$Name)
coefs$Seasons=factor(coefs$Seasons,levels=c('spring','summer','autumn','winter'))
for(nn in c('(CNWS)','(CSWS)','(DEWP)','(LogBLD)','(LogBLH)','(LogHUMI)','(PRES)','(TEMP)'))
  levels(coefs$Name)[levels(coefs$Name)==paste0('D',nn)]=paste0('Delta ',nn)
for(poll in c('PM25','SO2','NO2'))
{
  u=1
  coeffis=coefs[coefs$pollu==poll&coefs$Sites==rnames[u],]
  figure1=ggplot(coeffis[!is.na(coeffis$Estimates)&coeffis$Name!='Time dummies',],aes(x=Years, y=Estimates,group=Seasons,color=factor(Seasons)))+
    geom_point(aes(shape=factor(shape)),size=1.5)+geom_line(size=0.5)+
    geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
    facet_wrap(~Name,ncol=10,scales = "free_y",labeller = label_parsed) + 
    labs(x = "Year", y ='Estimates of Coefficients',title='(a) Beijing SE',color='Seasons')+mytheme_null_year
  u=2
  coeffis=coefs[coefs$pollu==poll&coefs$Sites==rnames[u],]
  figure2=ggplot(coeffis[!is.na(coeffis$Estimates)&coeffis$Name!='Time dummies',],aes(x=Years, y=Estimates,group=Seasons,color=factor(Seasons)))+
    geom_point(aes(shape=factor(shape)),size=1.5)+geom_line(size=0.5)+
    geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
    facet_wrap(~Name,ncol=10,scales = "free_y",labeller = label_parsed) + 
    labs(x = "Year", y ='Estimates of Coefficients',title='(b) Beijing NW',color='Seasons')+mytheme_null_year
  u=3
  coeffis=coefs[coefs$pollu==poll&coefs$Sites==rnames[u],]
  figure3=ggplot(coeffis[!is.na(coeffis$Estimates)&coeffis$Name!='Time dummies',],aes(x=Years, y=Estimates,group=Seasons,color=factor(Seasons)))+
    geom_point(aes(shape=factor(shape)),size=1.5)+geom_line(size=0.5)+
    geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
    facet_wrap(~Name,ncol=10,scales = "free_y",labeller = label_parsed) + 
    labs(x = "Year", y ='Estimates of Coefficients',title='(c) Tangshan',color='Seasons')+mytheme_null_year
  u=4
  coeffis=coefs[coefs$pollu==poll&coefs$Sites==rnames[u],]
  figure4=ggplot(coeffis[!is.na(coeffis$Estimates)&coeffis$Name!='Time dummies',],aes(x=Years, y=Estimates,group=Seasons,color=factor(Seasons)))+
    geom_point(aes(shape=factor(shape)),size=1.5)+geom_line(size=0.5)+
    geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
    facet_wrap(~Name,ncol=10,scales = "free_y",labeller = label_parsed) + 
    labs(x = "Year", y ='Estimates of Coefficients',title='(d) Baoding',color='Seasons')+mytheme_null_year
  
  figureS10=ggarrange(figure1, figure2, figure3, figure4,ncol = 1, nrow = 4,common.legend =T,legend ='bottom')
  ggsave(figureS10,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_bld/',poll,'coefficients.png'),
         units="in",width=14.2, height=11, dpi=300)
  
}

#---------------Figure S9--------------
#按起始时刻对episodes 分类
#nodes按0~23顺序排列，如1~5,6~12,13~18,19~24,就输入1,7,13,19
library(ggsci)
nodes=c(6,19)
pathsavee="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes/"
pathsavees="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat/"
eplen=7

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
    
    pm_period$d_n=pm_period$period
    pm_period$d_n[pm_period$period==1]='day'
    pm_period$d_n[pm_period$period==2]='night'
    figure1=ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<year2+1,],aes(x=factor(orde),y=PM2.5,fill=factor(d_n)))+geom_boxplot()+
      #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
      facet_wrap(~year,ncol=length(years),scales = "free_y") +scale_fill_simpsons()+ 
      labs(x="Hour",y=expression(bold(PM[2.5]~"("*mu*"g/"*m^3*")")),title = expression(bold('(a) ')*bold(PM[2.5])),fill='Start point')+mytheme+
      scale_x_discrete(labels=1:eplen-1)
    
    figure2=ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<year2+1,],aes(x=factor(orde),y=SO2,fill=factor(d_n)))+geom_boxplot()+
      #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
      facet_wrap(~year,ncol=length(years),scales = "free_y") +scale_fill_simpsons()+ 
      labs(x="Hour",y=expression(bold(SO[2]~"("*mu*"g/"*m^3*")")),title = expression(bold('(c) ')*bold(SO[2])),fill='Start point')+mytheme+
      scale_x_discrete(labels=1:eplen-1)
    
    figure3=ggplot(pm_period[pm_period$orde<=eplen&pm_period$year<year2+1,],aes(x=factor(orde),y=NO2,fill=factor(d_n)))+geom_boxplot()+
      #geom_text(data=count_period[count_period$orde<=eplen,], aes(x=factor(orde), y=0.5, label=count),color='blue',vjust=0.7) +
      stat_summary(fun.y = "mean",geom = "point",aes(group=period),position=position_dodge(width=0.75),shape=23,size=1.5,fill="white")+
      facet_wrap(~year,ncol=length(years),scales = "free_y") +scale_fill_simpsons()+ 
      labs(x="Hour",y=expression(bold(NO[2]~"("*mu*"g/"*m^3*")")),title = expression(bold('(b) ')*bold(NO[2])),fill='Start point')+mytheme+
      scale_x_discrete(labels=1:eplen-1)
    
    figureS9=ggarrange(figure1, figure3, figure2, ncol = 1, nrow = 3,common.legend =T,legend ='bottom')
    ggsave(figureS9,filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/paper-figures/',rnames[u],seasons[i],'pattern.png'),
           units="in",width=12, height=9, dpi=300)
    
  }
}
colnames(epls)=c('total',paste0('less than ',eplen),paste0('geq ',eplen))
rownames(epls)=paste0(rep(rnames,each=length(years)*length(seasons)),rep(years,length(seasons)*length(rnames)),
                      rep(rep(seasons,each=length(years)),length(rnames)))

#-------------------Table S2 and S3----------------
convert_sites <- function(x)
{
  if(x=="DongsiTiantanNongzhanguan")
    return('Beijing SE')
  else if(x=="GuanyuanWanliuAotizhongxin")
    return('Beijing NW')
  else if(x=="ShierzhongWuzijuLeidazhan")
    return('Tangshan')
  else if(x=="HuadianerquYouyongguanJiancezhan")
    return('Baoding')
}
pps=c('PM25','NO2','SO2')

tb_NO=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/NO2adjusted.csv'),stringsAsFactors = F)
rate_SE=tb_NO$rate[tb_NO$orde==eplen&tb_NO$site=="DongsiTiantanNongzhanguan"&tb_NO$season=='winter']
#1-mean(rate_SE[3:6])/mean(rate_SE[2])
1-rate_SE[6]/rate_SE[2]
rate_NW=tb_NO$rate[tb_NO$orde==eplen&tb_NO$site=="GuanyuanWanliuAotizhongxin"&tb_NO$season=='winter']
1-rate_NW[6]/rate_NW[2]

tb_summary=c()
tbcompare_summary=c()
for(pp in 1:length(pps))
{
  tb=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/',pps[pp],'adjusted.csv'),stringsAsFactors = F)
  tb_compare=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/',pps[pp],'adjustedcompared.csv'),stringsAsFactors = F)
  tb_compare$rate.se=0
  tb_compare$rate.se=tb_compare$SE/(tb_compare$orde-1)
  for(i in 1:length(seasons))
  {
    for(u in 1:length(rnames))
    {
      tb_part=tb[tb$site==rnames[u]&tb$season==seasons[i]&tb$orde==eplen,c('adjusted','se')]/(eplen-1)
      tb_part2=tb[tb$site==rnames[u]&tb$season==seasons[i]&tb$orde==hour_try,c('adjusted','se')]/(hour_try-1)
      tb_summary=rbind(tb_summary, c(pps[pp],seasons[i],rnames[u],paste0(round(tb_part2$adjusted,1),'(',round(tb_part2$se,1),'), ',round((tb_part2$adjusted/tb_part2$adjusted[1]-1)*100,1)),
                                     paste0(round(tb_part$adjusted,1),'(',round(tb_part$se,1),'), ',round((tb_part$adjusted/tb_part$adjusted[1]-1)*100,1))))
      #---------------------------------------
      tbcompare_part=tb_compare[tb_compare$Sites==rnames[u]&tb_compare$Seasons==seasons[i]&tb_compare$orde==eplen,]
      tbcompare_part2=tb_compare[tb_compare$Sites==rnames[u]&tb_compare$Seasons==seasons[i]&tb_compare$orde==hour_try,]
      #-----------------
      aver=paste0(-round(mean(tbcompare_part$rate),1),'(',-round(mean(tbcompare_part$rate/abs(tb_part$adjusted[1]))*100,1),'%)')
      aver2=paste0(-round(mean(tbcompare_part2$rate),1),'(',-round(mean(tbcompare_part2$rate/abs(tb_part2$adjusted[1]))*100,1),'%)')
      #2015-2018跟2014比
      avers=paste0(-round(mean(tb_part$adjusted[3:6])-tb_part$adjusted[2],1),
                   '(',-round((mean(tb_part$adjusted[3:6])-tb_part$adjusted[2])/abs(tb_part$adjusted[2])*100,1),'%)')
      #------------
      # The change point that is the year when the significant reduction in the growth rate compared to that in 2013 happened 
      # and after which significant increase in the growth rate did not happen in subsequent year.
      sig_increase=max(c(0,which(tbcompare_part$significance==T&tbcompare_part$rate>0)))
      
      if(sig_increase<length(years)-1)
      {
        tbcompare_potent=tbcompare_part[(sig_increase+1):(length(years)-1),]
        if(any(tbcompare_potent$significance==T&tbcompare_potent$rate<0)) 
        {
          point=min(which(tbcompare_potent$significance==T&tbcompare_potent$rate<0))
          stay=c(tbcompare_potent$Years[point],paste0(round(-tbcompare_potent$rate[point],1),
                                                      '(',round(tbcompare_potent$rate.se[point],1),')'))
          
        }
        else stay=c('---','---')        
      }
      else stay=c('---','---')
      
      sig_increase2=max(c(0,which(tbcompare_part2$significance==T&tbcompare_part2$rate>0)))
      
      if(sig_increase2<length(years)-1)
      {
        tbcompare_potent2=tbcompare_part2[(sig_increase2+1):(length(years)-1),]
        if(any(tbcompare_potent2$significance==T&tbcompare_potent2$rate<0)) 
        {
          point2=min(which(tbcompare_potent2$significance==T&tbcompare_potent2$rate<0))
          stay2=c(tbcompare_potent2$Years[point2],paste0(round(-tbcompare_potent2$rate[point2],1),
                                                         '(',round(tbcompare_potent2$rate.se[point2],1),')'))
          
        }
        else stay2=c('---','---')        
      }
      else stay2=c('---','---')
      #------------
      small=which.min(tbcompare_part$rate)
      small2=which.min(tbcompare_part2$rate)
      
      if(min(tbcompare_part2$rate)<0)
        minn2=c(tbcompare_part2$Years[small2],paste0(round(-tbcompare_part2$rate[small2],1),'(',round(tbcompare_part2$rate.se[small2],1),')'))
      else minn2=c('---','---')
      if(min(tbcompare_part$rate)<0) 
        minn=c(tbcompare_part$Years[small],paste0(round(-tbcompare_part$rate[small],1),'(',round(tbcompare_part$rate.se[small],1),')'))
      else minn=c('---','---')
      
      tbcompare_summary=rbind(tbcompare_summary,c(pps[pp],seasons[i],rnames[u],stay2,minn2,aver2,stay,minn,aver,avers))
    }
  }
}
tb_summary=as.data.frame(tb_summary)
colnames(tb_summary)=c('Pollutant','Season','Cluster',years,years)
tb_summary$Cluster=apply(as.matrix(tb_summary$Cluster),1,convert_sites)
tb_summary$Season=str_to_title(tb_summary$Season)
#Table S5
tb_summary[,c(1:3,1:length(years)+3+length(years))]
print(xtable(tb_summary[,c(1:3,1:length(years)+3+length(years))],
             caption=paste0("Growth Rate")),
      include.rownames =F)

tbcompare_summary=as.data.frame(tbcompare_summary)
colnames(tbcompare_summary)=c('Pollutant','Season','Cluster','Year','Difference','Year','Difference','Average reduction',
                              'Year','Difference','Year','Difference','Average reduction','Average reduction(14)')
tbcompare_summary$Cluster=apply(as.matrix(tbcompare_summary$Cluster),1,convert_sites)
tbcompare_summary$Season=str_to_title(tbcompare_summary$Season)
#Table S4
print(xtable(tbcompare_summary[,c(1:3,9:14)],
             caption=paste0("The largest reduction in growth rate compared with that in 2013")),
      include.rownames =F)
#----------------------------
reduces=c()
for(pp in 1:length(pps))
{
  tb=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/',pps[pp],'adjusted.csv'),stringsAsFactors = F)
  for(u in 1:length(rnames))
  {
    year_part=tb[tb$site==rnames[u]&tb$orde==eplen,c('season','year','rate')]
    rate_year=as.data.frame(year_part%>%group_by(year)%>%summarise(count=n(),MEAN=mean(rate)))
    rate_year$reduce=rate_year$MEAN[1]-rate_year$MEAN
    rate_year$reduce_re=-(rate_year$MEAN/rate_year$MEAN[1]-1)*100
    rate_year$reduce1=rate_year$MEAN[2]-rate_year$MEAN
    rate_year$reduce1_re=-(rate_year$MEAN/rate_year$MEAN[2]-1)*100
    rate_year$pollutant=pps[pp]
    rate_year$cluster=rnames[u]
    reduces=rbind(reduces,c(pps[pp],rnames[u],
                            paste0(round(rate_year$MEAN,1)),
                            paste0(round(rate_year$reduce[rate_year$year==2018],1),'(',
                                   round(rate_year$reduce_re[rate_year$year==2018],1),'%)'),
                            paste0(round(rate_year$reduce1[rate_year$year==2018],1),'(',
                                   round(rate_year$reduce1_re[rate_year$year==2018],1),'%)'),
                            #paste0(round(rate_year$MEAN,1),'(',c(0,round(rate_year$reduce_re,1)[-1]),'%)'),
    paste0(-round(mean(rate_year$MEAN[rate_year$year%in%c(2014:2018)])-rate_year$MEAN[rate_year$year==2013],1),'(',
           round((1-mean(rate_year$MEAN[rate_year$year%in%c(2014:2018)])/rate_year$MEAN[rate_year$year==2013])*100,1),'%)'),
    paste0(-round(mean(rate_year$MEAN[rate_year$year%in%c(2015:2018)])-rate_year$MEAN[rate_year$year==2014],1),'(',
           round((1-mean(rate_year$MEAN[rate_year$year%in%c(2015:2018)])/rate_year$MEAN[rate_year$year==2014])*100,1),'%)')))
    
  }
}

reduces=as.data.frame(reduces)
colnames(reduces)=c('Pollutant','Cluster',as.character(year1:year2),'13','14','Aver.13','Aver.14')
reduces$Cluster=apply(as.matrix(reduces$Cluster),1,convert_sites)
#Table 4
print(xtable(reduces[,1:10]),include.rownames = F)
d_plot=data.frame(Pollutant=rep(reduces$Pollutant,length(years)),Cluster=rep(reduces$Cluster,length(years)),
                  Year=rep(years,each=nrow(reduces)),
           Rate=c(as.numeric(as.character(reduces$`2013`)),as.numeric(as.character(reduces$`2014`)),as.numeric(as.character(reduces$`2015`)),
                  as.numeric(as.character(reduces$`2016`)),as.numeric(as.character(reduces$`2017`)),as.numeric(as.character(reduces$`2018`))))
d_plot$Cluster=factor(d_plot$Cluster,levels =c('Beijing SE','Beijing NW','Tangshan','Baoding') )
ggplot(d_plot,aes(x=Year, y=Rate,color=factor(Pollutant)))+
  geom_point()+geom_line()+
  facet_wrap(~Cluster,ncol=4,scales = "fixed") + 
  scale_color_manual(values=c('#CC0000',"#009966",'#3366CC'),labels=c(expression(bold(NO[2])),expression(bold(PM[2.5])),expression(bold(SO[2]))))+
  labs(x = "Year", y =expression(bold('Average Growth Rate'~"("*mu*"g/"*m^3~"per hour)")))+mytheme+
  theme(legend.position = 'bottom')
ggsave(filename=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/figures_bld/ppt.png'),units="in",width=11.5, height=5, dpi=300)

pp=1
tb=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new_bld/',pps[pp],'adjusted.csv'),stringsAsFactors = F)
year_part=tb[tb$site%in%rnames[1:2]&tb$orde==eplen,c('season','year','rate')]
rate_year=as.data.frame(year_part%>%group_by(year)%>%summarise(count=n(),MEAN=mean(rate)))
rate_year$reduce=rate_year$MEAN[1]-rate_year$MEAN
rate_year$reduce_re=-(rate_year$MEAN/rate_year$MEAN[1]-1)*100
rate_year$reduce1=rate_year$MEAN[2]-rate_year$MEAN
rate_year$reduce1_re=-(rate_year$MEAN/rate_year$MEAN[2]-1)*100
paste0(round(rate_year$reduce[rate_year$year==2018],1),'(',
       round(rate_year$reduce_re[rate_year$year==2018],1),'%)')
paste0(round(rate_year$reduce1[rate_year$year==2018],1),'(',
       round(rate_year$reduce1_re[rate_year$year==2018],1),'%)')

