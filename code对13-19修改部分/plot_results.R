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
                              plot.title = element_blank(),
                              #legend.title = element_blank(),
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
                              plot.title = element_blank(),
                              #legend.title = element_blank(),
                              legend.text = element_text(size=10, face = 'bold'),
                              legend.key.width  = unit(.3,"inches"),
                              legend.key.height = unit(.3,"inches"),
                              panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

hour_try=5

ad_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/PM2.5adjusted.csv",stringsAsFactors = F)
ad_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/NO2adjusted.csv",stringsAsFactors = F)
ad_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/SO2adjusted.csv",stringsAsFactors = F)
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

for(u in 1:length(rnames))
{
  for(pp in 1:length(pps))
  {
    for(i in 1:length(seasons))
    {
      ggplot(ad_PNS[ad_PNS$Pollutant==pps[pp]&ad_PNS$site==rnames[u]&ad_PNS$season==seasons[i],],aes(x=orde, y=adjusted,group=factor(year)))+
        #geom_point(size=0.5,color="blue")+
        geom_line(color="blue")+
        geom_ribbon(aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
        geom_line(aes(x=orde, y=pollutant),size=0.4,alpha=1,color="red")+
        scale_x_continuous(breaks =2*(0:ceiling(eplen/2))+1,labels=2*(0:ceiling(eplen/2)))+
        facet_wrap(.~ factor(subtitle),ncol=length(unique(ad_PNS$year)), scales = "fixed") + 
        labs(x = "Hour", y = paste0('Average Growth'))+mytheme
      ggsave(paste0(pathsavegp,pps[pp],rnames[u],seasons[i],'adjustedfacet.png'),units="in",width=8, height=1.73, dpi=300)
      
    }
  }
}

ad_PNS$season=factor(ad_PNS$season,levels = c('spring','summer','autumn','winter'),
                     labels=c('Spring','Summer','Autumn','Winter'))
ad_PNS$sitename=factor(ad_PNS$site,levels =rnames,
                       labels = c('Beijing SE','Beijing NW','Tangshan','Baoding'))

ad_PNS[ad_PNS$orde>1,c('adjusted','ad.lb','ad.ub')]=ad_PNS[ad_PNS$orde>1,c('adjusted','ad.lb','ad.ub')]/(ad_PNS$orde[ad_PNS$orde>1]-1)
# ad_PNS$ad.lb[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted<(0))]=ad_PNS$adjusted[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted<(0))]
# ad_PNS$ad.lb[which(ad_PNS$ad.lb<(-3.5)&ad_PNS$adjusted>(0))]=-3.5
ad_PNS$ad.lb[which(ad_PNS$adjusted<(0))]=ad_PNS$adjusted[which(ad_PNS$adjusted<(0))]
#ad_PNS$ad.lb[which(ad_PNS$ad.lb<(0)&ad_PNS$adjusted>=(0))]=0

pp ='PM2.5'
# min(ad_PNS$ad.lb[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp])
# max(ad_PNS$ad.ub[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp])
for(ss in unique(ad_PNS$site))
{
  ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp&ad_PNS$site==ss,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
    geom_bar(stat='identity', position='dodge') +ylim(0,17)+
    geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
    facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
    mytheme_null +
    theme(legend.position="none")+
    scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
    labs(x = 'Season',y =paste0('Adjusted Growth Rate of ',pp), title = paste0('Growth rate of ',pp))
  ggsave(paste0(pathsavegp,ss,pp,'adjusted.png'),units="in",width=3.5, height=4.5, dpi=300)
  
  ggplot(ad_PNS[ad_PNS$orde==hour_try&ad_PNS$Pollutant==pp&ad_PNS$site==ss,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
    geom_bar(stat='identity', position='dodge') +ylim(0,17.6)+
    geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
    facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
    mytheme_null +
    theme(legend.position="none")+
    scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
    labs(x = 'Season',y =paste0('Adjusted Growth Rate of ',pp), title = paste0('Growth rate of ',pp))
  ggsave(paste0(pathsavegp,ss,pp,'adjusted2.png'),units="in",width=3.5, height=4.5, dpi=300)
  
}  
#----------------------
# #Baoding 2013 is strange.
# ad_PNS[ad_PNS$Pollutant=="NO2"&ad_PNS$year==2013&ad_PNS$sitename=='Baoding'&ad_PNS$season=='Spring',c('adjusted','ad.lb','ad.ub')]=NA
# ad_PNS[ad_PNS$Pollutant=="SO2"&ad_PNS$year==2013&ad_PNS$sitename=='Baoding'&ad_PNS$season%in%c('Spring','Summer','Autumn'),c('adjusted','ad.lb','ad.ub')]=NA

pp ='NO2'
for(ss in unique(ad_PNS$site))
{
  ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp&ad_PNS$site==ss,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
    geom_bar(stat='identity', position='dodge') +ylim(-0.6,12.1)+
    geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
    facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
    mytheme_null +
    theme(legend.position="none")+
    scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
    labs(x = 'Season',y =paste0('Adjusted Growth Rate of ',pp), title = paste0('Growth rate of ',pp))
  ggsave(paste0(pathsavegp,ss,pp,'adjusted.png'),units="in",width=3.5, height=4.5, dpi=300)
  
  ggplot(ad_PNS[ad_PNS$orde==hour_try&ad_PNS$Pollutant==pp&ad_PNS$site==ss,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
    geom_bar(stat='identity', position='dodge') +ylim(-2.7,13.1)+
    geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
    facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
    mytheme_null +
    theme(legend.position="none")+
    scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
    labs(x = 'Season',y =paste0('Adjusted Growth Rate of ',pp), title = paste0('Growth rate of ',pp))
  ggsave(paste0(pathsavegp,ss,pp,'adjusted2.png'),units="in",width=3.5, height=4.5, dpi=300)
}  
#------------------------
pp='SO2'
for(ss in unique(ad_PNS$site)[1:2])
{
  ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp&ad_PNS$site==ss,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
    geom_bar(stat='identity', position='dodge') +
    geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+ylim(-0.25,6)+
    facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
    mytheme_null +
    theme(legend.position="none")+
    scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
    labs(x = 'Season',y =paste0('Adjusted Growth Rate of ',pp), title = paste0('Growth rate of ',pp))
  ggsave(paste0(pathsavegp,ss,pp,'adjusted.png'),units="in",width=3.5, height=4.5, dpi=300)
  
  ggplot(ad_PNS[ad_PNS$orde==hour_try&ad_PNS$Pollutant==pp&ad_PNS$site==ss,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
    geom_bar(stat='identity', position='dodge') +
    geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+ylim(-0.22,5.3)+
    facet_wrap(.~ sitename,ncol=4, scales = "free_y") +
    mytheme_null +
    theme(legend.position="none")+
    scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
    labs(x = 'Season',y =paste0('Adjusted Growth Rate of ',pp), title = paste0('Growth rate of ',pp))
  ggsave(paste0(pathsavegp,ss,pp,'adjusted2.png'),units="in",width=3.5, height=4.5, dpi=300)
  
}
#----------------------------------------------
pp='SO2'
for(ss in unique(ad_PNS$site)[3:4])
{
  ggplot(ad_PNS[ad_PNS$orde==eplen&ad_PNS$Pollutant==pp&ad_PNS$site==ss,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
    geom_bar(stat='identity', position='dodge') +
    geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+ylim(-1,29.9)+
    facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
    mytheme_null +
    theme(legend.position="none")+
    scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
    labs(x = 'Season',y =paste0('Adjusted Growth Rate of ',pp), title = paste0('Growth rate of ',pp))
  ggsave(paste0(pathsavegp,ss,pp,'adjusted.png'),units="in",width=3.5, height=4.5, dpi=300)
  
  ggplot(ad_PNS[ad_PNS$orde==hour_try&ad_PNS$Pollutant==pp&ad_PNS$site==ss,],aes(x=factor(season),y=adjusted,fill=factor(year)))+
    geom_bar(stat='identity', position='dodge') +
    geom_errorbar(aes(x =factor(season), ymin=ad.lb, ymax=ad.ub), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+ylim(-1.67,27.9)+
    facet_wrap(.~ sitename,ncol=4, scales = "free_y") +
    mytheme_null +
    theme(legend.position="none")+
    scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
    labs(x = 'Season',y =paste0('Adjusted Growth Rate of ',pp), title = paste0('Growth rate of ',pp))
  ggsave(paste0(pathsavegp,ss,pp,'adjusted2.png'),units="in",width=3.5, height=4.5, dpi=300)
  
}

#---------------------------------------
d_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/PM2.5adjustedcompared.csv",stringsAsFactors = F)
d_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/NO2adjustedcompared.csv",stringsAsFactors = F)
d_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/SO2adjustedcompared.csv",stringsAsFactors = F)
# #Baoding 2013 strange, delete some results.
# d_NO_delete=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/NO2adjustedcompareddelete.csv",stringsAsFactors = F)
# d_NO=d_NO[-which(d_NO$Sites=='HuadianerquYouyongguanJiancezhan'&d_NO$Years==2014&d_NO$Seasons=='spring'),]
# d_NO[d_NO$Sites=='HuadianerquYouyongguanJiancezhan'&d_NO$Seasons=='spring',-1]=d_NO_delete[,-1]
# d_SO_delete=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/SO2adjustedcompareddelete.csv",stringsAsFactors = F)
# d_SO=d_SO[-which(d_SO$Sites=='HuadianerquYouyongguanJiancezhan'&d_SO$Years==2014&d_SO$Seasons%in%c('spring','summer','autumn')),]
# d_SO[d_SO$Sites=='HuadianerquYouyongguanJiancezhan'&d_SO$Seasons%in%c('spring','summer','autumn'),-1]=d_SO_delete[,-1]

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

pd <- position_dodge(0.56)
#pd <- position_dodge(0.25)
for(ddd in 1:length(rnames))
{
  ggplot(p.compare[p.compare$Sites==rnames[ddd]&p.compare$orde==eplen,],aes(x=Years,y=Estimates/(orde-1),color=Pollutant))+
    geom_point(aes(x=Years,y=Estimates/(orde-1),color=Pollutant,shape=significance,size=significance),position=pd)+geom_line(position=pd,size=0.4) +
    geom_errorbar(aes(x =Years, ymin=(Estimates-qnorm(1-0.05/2)*SE)/(orde-1),
                      ymax=(Estimates+qnorm(1-0.05/2)*SE)/(orde-1)), alpha = 1, width = 0.25, position=pd)+
    geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
    facet_wrap(.~ Seasons,ncol=4, scales = "fixed")+
    scale_shape_manual(values=c(20,8))+
    scale_color_manual(values=c('#CC0000',"#009966",'#3366CC'))+
    scale_size_manual(values=c(2,2.5))+
    mytheme_null1 +theme(legend.position="none")+
    labs(x = 'Year',y =paste0('Difference of the Adjusted Growth Rate'), title = paste0('Difference of the Adjusted Growth Rate'))
  ggsave(paste0(pathsavegp,rnames[ddd],'differences.png'),units="in",width=8.8, height=2.4, dpi=300)
  
  # data1=p.compare[p.compare$Sites==rnames[ddd]&p.compare$orde==eplen,]
  # ggplot()+
  #   geom_line(data =data1 ,
  #             aes(x=Years,y=Estimates/(orde-1),color=Pollutant),linetype = "dashed",size = 1.0,show.legend = FALSE)+
  #   ## solid line
  #   geom_line(data =data1[data1$significance==T,],aes(x=Years,y=Estimates/(orde-1),color=Pollutant),size = 1.5)+
  #   geom_point(data = data1,aes(x=Years,y=Estimates/(orde-1),color=Pollutant),size=3.0)+
  #   scale_x_discrete(label= c("2014   ","2015   ","2016   ","2017   ","2018   "))+
  #   facet_wrap(.~ Seasons,ncol=4, scales = "fixed")+mytheme
  
  ggplot(p.compare[p.compare$Sites==rnames[ddd]&p.compare$orde==hour_try,],aes(x=Years,y=Estimates/(orde-1),color=Pollutant))+
    geom_point(aes(x=Years,y=Estimates/(orde-1),color=Pollutant,shape=significance,size=significance),position=pd)+geom_line(position=pd,size=0.4) +
    geom_errorbar(aes(x =Years, ymin=(Estimates-qnorm(1-0.05/2)*SE)/(orde-1),
                      ymax=(Estimates+qnorm(1-0.05/2)*SE)/(orde-1)), alpha = 1, width = 0.25,position=pd)+
    geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
    facet_wrap(.~ Seasons,ncol=4, scales = "fixed")+
    scale_shape_manual(values=c(20,8))+
    scale_color_manual(values=c('#CC0000',"#009966",'#3366CC'))+
    scale_size_manual(values=c(2,2.5))+
    mytheme_null1 +theme(legend.position="none")+
    labs(x = 'Year',y =paste0('Difference of the Adjusted Growth Rate'), title = paste0('Difference of the Adjusted Growth Rate'))
  ggsave(paste0(pathsavegp,rnames[ddd],'differences2.png'),units="in",width=8.8, height=2.4, dpi=300)

}

#-------------------------------------
R2_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/PM2.5R2.csv",stringsAsFactors = F)
R2_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/NO2R2.csv",stringsAsFactors = F)
R2_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/SO2R2.csv",stringsAsFactors = F)
R2_PM$Pollutant='PM25'
R2_NO$Pollutant='NO2'
R2_SO$Pollutant='SO2'
R2_PNS=rbind(R2_PM,R2_NO,R2_SO)
pollutant_names=c('PM25','NO2','SO2')
min(R2_PNS$R2)
max(R2_PNS$R2)
for(ppp in 1:length(pollutant_names))
{
  ggplot(R2_PNS[R2_PNS$Pollutant==pollutant_names[ppp],],aes(x=Years,y=R2,color=factor(Seasons)))+
    geom_point()+
    #geom_line() +
    facet_wrap(.~ sitename,ncol=4, scales = "fixed")+
    mytheme_year +theme(legend.position="none")+ylim(0.3,1)+
    labs(x = 'Year',y ='R Squared', title = paste0('R Squared of ',pollutant_names[ppp]))
  ggsave(paste0(pathsavegp,pollutant_names[ppp],'R2.png'),units="in",width=8, height=2.8, dpi=300)
}

#------------------------------------
coef_PM=read.csv('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/PM2.5coefficients.csv',stringsAsFactors = F)
coef_PM$pollu='PM25'
coef_SO=read.csv('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/SO2coefficients.csv',stringsAsFactors = F)
coef_SO$pollu='SO2'
coef_NO=read.csv('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/SO2coefficients.csv',stringsAsFactors = F)
coef_NO$pollu='NO2'
coefs=rbind(coef_PM,coef_SO,coef_NO)
coefs$Name=as.factor(coefs$Name)
coefs$Seasons=factor(coefs$Seasons,levels=c('spring','summer','autumn','winter'))
for(nn in c('(CNWS)','(CSWS)','(DEWP)','(LogBLH)','(LogHUMI)','(PRES)','(TEMP)'))
  levels(coefs$Name)[levels(coefs$Name)==paste0('D',nn)]=paste0('Delta ',nn)
for(u in 1:length(rnames))
{
  for(poll in c('PM25','SO2','NO2'))
  {
    coeffis=coefs[coefs$pollu==poll&coefs$Sites==rnames[u],]
    ggplot(coeffis[!is.na(coeffis$Estimates)&coeffis$Name!='Time dummies',],aes(x=Years, y=Estimates,group=Seasons,color=factor(Seasons)))+
      geom_point(aes(shape=factor(shape)),size=1.5)+geom_line(size=0.5)+
      #geom_text(aes(x=Years,y=Estimates,label=Signifance),color='black',vjust=0)+
      geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
      facet_wrap(~Name,ncol=9,scales = "free_y",labeller = label_parsed) + 
      labs(x = "Year", y ='Estimates of Coefficients',color='Seasons')+mytheme_null_year+
      theme(legend.position="none")
    ggsave(paste0(pathsavegp,poll,rnames[u],'coefficients.png'),units="in",width=14, height=2.6, dpi=300)
    
  }
}


# vs_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/PM2.5selectedvariables.csv",stringsAsFactors = F)
# vs_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/NO2selectedvariables.csv",stringsAsFactors = F)
# vs_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/SO2selectedvariables.csv",stringsAsFactors = F)
# vs_total=rbind(t(vs_PM[,-(1:2)]),t(vs_NO[,-(1:2)]),t(vs_SO[,-(1:2)]))
# colnames(vs_total)=vs_PM$Variables
# print(xtable(vs_total,
#              caption=paste0("The number of variables selected in models for PM2.5")),
#       include.rownames =F)
# n_delete=rbind(apply(is.na(vs_PM[,-(1:2)]),1,sum),apply(is.na(vs_NO[,-(1:2)]),1,sum),apply(is.na(vs_SO[,-(1:2)]),1,sum))
# colnames(n_delete)=vs_PM$Variables
# n_delete=as.data.frame(n_delete)
# n_delete$pollutant=c('PM2.5','NO2','SO2')
# print(xtable(n_delete[,c(11,1:10)],
#              caption=paste0("The number of variables eliminated from the full model")),
#       include.rownames =F)
