#change of meteorological variable 
mytheme_null=theme_bw()+theme(axis.title = element_text(size = 10), 
                              text = element_text(face = "bold"),
                              strip.text = element_text(size = 15,face = 'bold'),
                              strip.background = element_rect(color="black", fill="white", linetype="solid"),
                              axis.text.x = element_text(size=13,hjust = 0.5, face = 'bold'),
                              axis.text.y = element_text(size=13, face = 'bold'),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_blank(),
                              #legend.title = element_blank(),
                              legend.text = element_text(size=10, face = 'bold'),
                              legend.key.width  = unit(.3,"inches"),
                              legend.key.height = unit(.3,"inches"),
                              panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

calculate.se <- function(x)
{
  x=as.vector(na.omit(x))
  if(length(x)<2)
    return(0)
  else return(sd(x)/sqrt(length(x)))
}

# city, station list
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin",
          "Huadianerqu","Youyongguan","Jiancezhan",
          "Shierzhong","Wuziju","Leidazhan")
csnames=matrix(csnames,ncol=3,byrow = T)
years=2013:2018
seasons=c('spring','summer','autumn','winter')
# kinds=c('before','middle','after')
# kis=c(-1,0,1)
# mark=c('earlier','equal','later')

# j=ss=kk=1
# for(j in 1:nrow(csnames))
# {
#   sites=csnames[j,]
#   meterolos=c()
#   for(ss in 1:length(seasons))
#   {
#     season=seasons[ss]
#     td=c()
#     for(kk in 1:length(kinds))
#     {
#       kind=kinds[kk]
#       dat=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episode_form_bma/',str_c(sites,collapse=""),season,kind,'.csv'),
#                    stringsAsFactors = F)
# 
#       td=rbind(td,dat[,c('TEMP','DEWP','HUMI','PRES','blh','INws','ISws','kind','orde')])
#     }
#       td_before=td[-4<td$orde&td$orde<1,]
#       td_after=td[0<td$orde&td$orde<5,]
#       meterolo=rbind(inner_join(as.data.frame(td_before %>% group_by(orde)%>%summarise(TEMP=mean(TEMP,na.rm = T),DEWP=mean(DEWP,na.rm = T),
#                                              HUMI=mean(HUMI,na.rm = T),PRES=mean(PRES,na.rm = T), BLH=mean(blh,na.rm = T),
#                                              INws=mean(INws,na.rm = T),ISws=mean(ISws,na.rm = T))),
#                      as.data.frame(td_before %>% group_by(orde)%>%summarise(TEMP.se=calculate.se(TEMP),DEWP.se=calculate.se(DEWP),
#                                                               HUMI.se=calculate.se(HUMI),PRES.se=calculate.se(PRES),BLH.se=calculate.se(blh),
#                                                               INws.se=calculate.se(INws),ISws.se=calculate.se(ISws)))),
#                      inner_join(as.data.frame(td_after %>% group_by(orde)%>%summarise(TEMP=mean(TEMP,na.rm = T),DEWP=mean(DEWP,na.rm = T),
#                                                                                        HUMI=mean(HUMI,na.rm = T),PRES=mean(PRES,na.rm = T), BLH=mean(blh,na.rm = T),
#                                                                                        INws=mean(INws,na.rm = T),ISws=mean(ISws,na.rm = T))),
#                                 as.data.frame(td_after %>% group_by(orde)%>%summarise(TEMP.se=calculate.se(TEMP),DEWP.se=calculate.se(DEWP),
#                                                                                        HUMI.se=calculate.se(HUMI),PRES.se=calculate.se(PRES),BLH.se=calculate.se(blh),
#                                                                                        INws.se=calculate.se(INws),ISws.se=calculate.se(ISws)))))
#       meterolo$season=season
#       meterolo$site=str_c(sites,collapse="")
#     
#       meterolos=rbind(meterolos,meterolo)
#     # dat_kind=datplots[datplots$kind==-1,c('type',"TEMP","DEWP","HUMI","PRES","BLH","INws","ISws")] 
#     # plot1=dat_kind[,-1]
#     # rownames(plot1)=dat_kind[,1]
#     # plot1%>% as_tibble(rownames = "group")%>% 
#     #     mutate_at(vars(-group), rescale)-> plot1
#     # plot1
#     # ggplot(data=datplots[datplots$kind==-1,],aes(x=variable, y=mm, group=type, color=type)) + 
#     #   geom_point(size=1) + 
#     #   geom_line() + 
#     #   xlab("Decils") + 
#     #   ylab("difference in ") + 
#     #   #scale_x_discrete(limits=c(orden_deciles)) +
#     #   coord_polar()
#     
#   }
#   # meterolos$HUMI=log(meterolos$HUMI)
#   # meterolos$BLH=log(meterolos$BLH)
#   datplot=data.frame(orde=rep(meterolos$orde,7),season=rep(meterolos$season,7),site=rep(meterolos$site,7),
#                      aver=as.vector(as.matrix(meterolos[,c("TEMP","DEWP","HUMI","PRES","BLH","INws","ISws")])),
#                      se=as.vector(as.matrix(meterolos[,c("TEMP.se","DEWP.se","HUMI.se","PRES.se","BLH.se","INws.se","ISws.se")])),
#                      variables=rep(c("TEMP","DEWP","HUMI","PRES","BLH","INws","ISws"),each=nrow(meterolos)))
#   datplot$variables=factor(datplot$variables,levels=c('BLH','DEWP','HUMI','PRES','TEMP','INws','ISws'),
#                            labels=c('BLH','DEWP','HUMI','PRES','TEMP','CNWS','CSWS'))
#   datplot$season=factor(datplot$season,levels=c('summer','spring','winter','autumn'))
#   datplot$up=datplot$aver+qnorm(0.975)*datplot$se
#   datplot$low=datplot$aver-qnorm(0.975)*datplot$se
#   po_label=as.data.frame(datplot%>%group_by(variables)%>%summarise(po.label=max(up)+(max(up)-min(low))*0.3))
#   po_label=rbind(po_label,po_label)
#   po_label$x.loc=rep(c(-3,1),each=nrow(po_label)/2)
#   po_label$labels=rep(c('Four hours before\nthe calm episode','The first four hours\nin the calm episode'),each=nrow(po_label)/2)
#   po_label$po.label=po_label$po.label
#   ggplot(datplot,aes(x=orde, y=aver, color=season)) +
#     geom_line() +
#     geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
#     geom_text(data=po_label,
#       aes(x.loc, po.label, label = labels), color='black',
#       hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
#     scale_x_continuous(breaks=unique(meterolos$orde))+
#     geom_vline(xintercept = 0.5,linetype="dashed")+
#     facet_wrap(.~ variables,ncol=2, scales = "free") +
#     mytheme_null+theme(legend.position="none")
#     
#   ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/',str_c(sites,collapse=""),'meterchange.png'),units="in",width=9.5, height=10.5, dpi=300)
#   
# }
l_before=4
l_after=4
j=ss=h=yy=ii=1
for(j in 1:nrow(csnames))
{
  sites=csnames[j,]
  meterolos=c()
  data_cs=c()
  for(h in 1:length(sites))
  {
    sit=sites[h]
    td=c()
    for(yy in 1:length(years))
    {
      for(ss in 1:length(seasons))
      {
        dat=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesmark/',sit,years[yy],seasons[ss],'_epm.csv'),
                     stringsAsFactors = F)
        dat$seasons=seasons[ss]
        td=rbind(td,dat[,c('X','seasons','TEMP','DEWP','HUMI','PRES','blh','bld','INws','ISws','seperation')])
      }
    }
    td=arrange(td,X)
    ep_start_X=intersect(td$X[td$seperation=="episode"],td$X[td$seperation=="nonepisode"]+1)
    for(ii in 1:length(ep_start_X))
    {
      data_part=td[td$X%in%(ep_start_X[ii]-l_before):(ep_start_X[ii]+l_after),]
      data_part$orde=data_part$X-ep_start_X[ii]
      data_part$year=years[yy]
      data_part$sit=sit
      data_part$site=str_c(sites,collapse="")
      data_cs=rbind(data_cs,data_part)
    }
  }
  
  for(ss in 1:length(seasons))
  {
    season=seasons[ss]
    td_all=data_cs[data_cs$seasons==season,]
    
    meterolo=inner_join(as.data.frame(td_all %>% group_by(orde)%>%summarise(TEMP=mean(TEMP,na.rm = T),DEWP=mean(DEWP,na.rm = T),
                                                                            HUMI=mean(HUMI,na.rm = T),PRES=mean(PRES,na.rm = T), BLH=mean(blh,na.rm = T),
                                                                            BLD=mean(bld,na.rm = T),INws=mean(INws,na.rm = T),ISws=mean(ISws,na.rm = T))),
                        as.data.frame(td_all %>% group_by(orde)%>%summarise(TEMP.se=calculate.se(TEMP),DEWP.se=calculate.se(DEWP),
                                                                            HUMI.se=calculate.se(HUMI),PRES.se=calculate.se(PRES),BLH.se=calculate.se(blh),
                                                                            BLD.se=calculate.se(bld),INws.se=calculate.se(INws),ISws.se=calculate.se(ISws))))
    meterolo$season=season
    meterolo$site=data_cs$site[1]
    
    meterolos=rbind(meterolos,meterolo)
  }
  # meterolos$HUMI=log(meterolos$HUMI)
  # meterolos$BLH=log(meterolos$BLH)
  datplot=data.frame(orde=rep(meterolos$orde,8),season=rep(meterolos$season,8),site=rep(meterolos$site,8),
                     aver=as.vector(as.matrix(meterolos[,c("TEMP","DEWP","HUMI","PRES","BLH","BLD","INws","ISws")])),
                     se=as.vector(as.matrix(meterolos[,c("TEMP.se","DEWP.se","HUMI.se","PRES.se","BLH.se",'BLD.se',"INws.se","ISws.se")])),
                     variables=rep(c("TEMP","DEWP","HUMI","PRES","BLH","BLD","INws","ISws"),each=nrow(meterolos)))
  #six panels
  #datplot=datplot[datplot$variables!='PRES',]
  datplot$variables=factor(datplot$variables,levels=c('BLH',"BLD",'DEWP','HUMI','PRES','TEMP','INws','ISws'),
                           labels=c('BLH',"BLD",'DEWP','HUMI','PRES','TEMP','CNWS','CSWS'))
  datplot$season=factor(datplot$season,levels=c('summer','spring','winter','autumn'))
  datplot$up=datplot$aver+qnorm(0.975)*datplot$se
  datplot$low=datplot$aver-qnorm(0.975)*datplot$se
  datplot$season=factor(datplot$season,levels=c('spring','summer','autumn','winter'))
  po_label=as.data.frame(datplot%>%group_by(variables)%>%summarise(po.label=max(up)+(max(up)-min(low))*0.3))
  po_label=rbind(po_label,po_label)
  po_label$x.loc=rep(c(-3.7,0.3),each=nrow(po_label)/2)
  po_label$labels=rep(c('Four hours before\nthe calm episode','The first four hours\nin the calm episode'),each=nrow(po_label)/2)
  po_label$po.label=po_label$po.label
  p1=ggplot(datplot[datplot$variables=='BLH',],aes(x=orde, y=aver, color=season)) +
    geom_line() +
    geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
    scale_color_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    scale_fill_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    geom_text(data=po_label[po_label$variables=='BLH',],
              aes(x.loc, po.label, label = labels), color='black',
              hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
    scale_x_continuous(breaks=unique(meterolos$orde))+
    geom_vline(xintercept = 0,linetype="dashed")+labs(x='Hour',y='BLH (m)',title = 'Average BLH')+
    #facet_wrap(.~ variables,ncol=2, scales = "free") +
    mytheme
  p2=ggplot(datplot[datplot$variables=='DEWP',],aes(x=orde, y=aver, color=season)) +
    geom_line() +
    geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
    scale_color_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    scale_fill_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    geom_text(data=po_label[po_label$variables=='DEWP',],
              aes(x.loc, po.label, label = labels), color='black',
              hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
    scale_x_continuous(breaks=unique(meterolos$orde))+
    geom_vline(xintercept = 0,linetype="dashed")+labs(x='Hour',y=expression(bold('DEWP ('*degree*'C)')),title = 'Average DEWP')+
    #facet_wrap(.~ variables,ncol=2, scales = "free") +
    mytheme
  p3=ggplot(datplot[datplot$variables=='HUMI',],aes(x=orde, y=aver, color=season)) +
    geom_line() +
    geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
    scale_color_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    scale_fill_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    geom_text(data=po_label[po_label$variables=='HUMI',],
              aes(x.loc, po.label, label = labels), color='black',
              hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
    scale_x_continuous(breaks=unique(meterolos$orde))+
    geom_vline(xintercept = 0,linetype="dashed")+labs(x='Hour',y=expression(bold('HUMI (%)')),title = 'Average HUMI')+
    #facet_wrap(.~ variables,ncol=2, scales = "free") +
    mytheme
  p4=ggplot(datplot[datplot$variables=='TEMP',],aes(x=orde, y=aver, color=season)) +
    geom_line() +
    geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
    scale_color_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    scale_fill_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    geom_text(data=po_label[po_label$variables=='TEMP',],
              aes(x.loc, po.label, label = labels), color='black',
              hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
    scale_x_continuous(breaks=unique(meterolos$orde))+
    geom_vline(xintercept = 0,linetype="dashed")+labs(x='Hour',y=expression(bold('TEMP ('*degree*'C)')),title = 'Average TEMP')+
    #facet_wrap(.~ variables,ncol=2, scales = "free") +
    mytheme
  p5=ggplot(datplot[datplot$variables=='CNWS',],aes(x=orde, y=aver, color=season)) +
    geom_line() +
    geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
    scale_color_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    scale_fill_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    geom_text(data=po_label[po_label$variables=='CNWS',],
              aes(x.loc, po.label, label = labels), color='black',
              hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
    scale_x_continuous(breaks=unique(meterolos$orde))+
    geom_vline(xintercept = 0,linetype="dashed")+labs(x='Hour',y='CNWS (m/s)',title = 'Average CNWS')+
    #facet_wrap(.~ variables,ncol=2, scales = "free") +
    mytheme
  p6=ggplot(datplot[datplot$variables=='CSWS',],aes(x=orde, y=aver, color=season)) +
    geom_line() +
    geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
    scale_color_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    scale_fill_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    geom_text(data=po_label[po_label$variables=='CSWS',],
              aes(x.loc, po.label, label = labels), color='black',
              hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
    scale_x_continuous(breaks=unique(meterolos$orde))+
    geom_vline(xintercept = 0,linetype="dashed")+labs(x='Hour',y='CSWS (m/s)',title = 'Average CSWS')+
    #facet_wrap(.~ variables,ncol=2, scales = "free") +
    mytheme
  p7=ggplot(datplot[datplot$variables=='BLD',],aes(x=orde, y=aver, color=season)) +
    geom_line() +
    geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
    scale_color_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    scale_fill_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    geom_text(data=po_label[po_label$variables=='BLD',],
              aes(x.loc, po.label, label = labels), color='black',
              hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
    scale_x_continuous(breaks=unique(meterolos$orde))+
    geom_vline(xintercept = 0,linetype="dashed")+labs(x='Hour',y=expression(bold('BLD (J m'^-2*')')),title = 'Average BLD')+
    #facet_wrap(.~ variables,ncol=2, scales = "free") +
    mytheme
  p8=ggplot(datplot[datplot$variables=='PRES',],aes(x=orde, y=aver, color=season)) +
    geom_line() +
    geom_ribbon(aes(x=orde, ymin=low, ymax=up,fill=season),linetype=0,alpha = 0.3) +
    scale_color_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    scale_fill_manual(values=c('spring'='#7CAE00','summer'='#F8766D','autumn'='#C77CFF','winter'='#00BFC4'))+
    geom_text(data=po_label[po_label$variables=='PRES',],
              aes(x.loc, po.label, label = labels), color='black',
              hjust = 0, vjust = 1, size = 4.5,fontface = "bold")+
    scale_x_continuous(breaks=unique(meterolos$orde))+
    geom_vline(xintercept = 0,linetype="dashed")+labs(x='Hour',y='PRES (hPa)',title = 'Average PRES')+
    #facet_wrap(.~ variables,ncol=2, scales = "free") +
    mytheme
  ggarrange(p7,p2,p1,p3,p4,p8,p5,p6,ncol=2,nrow=4,common.legend = T,legend='bottom')
  #ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/',str_c(sites,collapse=""),'meterchange.png'),units="in",width=9.5, height=10.5, dpi=300)
  ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/',str_c(sites,collapse=""),'meterchange.png'),units="in",width=9.5, height=11, dpi=300)
  
}


