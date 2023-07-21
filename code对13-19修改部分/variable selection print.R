
pathsavee="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes/"
pathsavees="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat/"
pathmdatasave="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/modeldata/"

rnames=c("DongsiTiantanNongzhanguan","GuanyuanWanliuAotizhongxin","ShierzhongWuzijuLeidazhan","HuadianerquYouyongguanJiancezhan")
allnames=paste0(rep(rnames,each=length(ysepn)),rep(ysepn,length(rnames)))

alllength=data.frame(name=allnames,num=rep(0,length(allnames)))
for(m in 1:length(allnames))
{
  ep=read.csv(paste0(pathsavee,allnames[m],'_ep.csv'), stringsAsFactors = FALSE)
  ep1=ep[ep$orde==1,c('ep_id','orde','hour')]
  
  id_period=ep1$ep_id[which(ep1$hour %in% berange)]
  alllength[m,2]=length(id_period)
}

library(fastDummies)
library(gglasso)
library(plm)
library(panelAR)
library(lmtest)
library(apsrtable)
library(texreg)
library(stargazer)
library(car)
library(lattice)
library(AICcmodavg)
library(MASS)
library(ggsci)
library(ggpubr)

path_min <- function(x)
{
  return(min(which(x^2>0)))
}
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
#---------------------------
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
    return(paste0('Time dummies'))
}
convert_sites <- function(x)
{
  if(x=="DongsiTiantanNongzhanguan")
    return('DNT, Beijing')
  else if(x=="GuanyuanWanliuAotizhongxin")
    return('AGW, Beijing')
  else if(x=="ShierzhongWuzijuLeidazhan")
    return('LSW, Tangshan')
  else if(x=="HuadianerquYouyongguanJiancezhan")
    return('HJY, Baoding')
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

set.seed(10)
berange=6:18#range of the beginning of episodes 
years=year1:year2
pollutant_sp='PM2.5'#'NO2' #'SO2' 
ysepn <-paste0(rep(years,length(seasons)),rep(seasons,each=length(years)))

#--------------------------------------------
candidate=c('TEMP','PRES','DEWP','blh','HUMI',
            'INws','ISws',
            'epfmINws','epfNen')

v_rank_seasons=c()
allls=c()
select_nums=c()
select_trys=c()
for(u in 1:length(rnames))
{
  elemen_season=vector("list",length(seasons))
  v_rank_site=c()
  for(i in 1:length(seasons))
  {
    datas1=vector("list",length(years))
    v_rank=data.frame(variable=c('factor(orde)',candidate))
    #pdf(file= paste0(pathsavegp,'gls23',rnames[u],'_',seasons[i],'.pdf'),height = 12,width=12,family = 'GB1')
    for(j in 1:length(years))
    {
      d<- read.csv(paste0(pathmdatasave,rnames[u],ysepn[(i-1)*length(years)+j],'_eptreat.csv'),
                   stringsAsFactors = FALSE)
      d$pollutant=d[,paste0('av_',pollutant_sp)]
      idna=unique(d$ep_id[is.na(d$pollutant)])
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
      id_period=d1$ep_id[which((d1$hour %in% berange)& poll_s<poll_6)]#starting time in the range
      
      print(length(id_period))
      if(length(id_period)>0)
      {
        d=d[which(d$ep_id%in%id_period),]
        sta=sta[which(sta$X%in%id_period),]
      }
      
      # stas[[j]]=sta
      # #HUMI, blh
      # plot(density(d$blh))
      # plot(density(log(d$blh)))
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
      #   facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + theme_bw()+
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
      
      data_coef=dataaa1[,c(candidate,'ep_id','orde',"pollutant")]
      data_coef[,candidate]=as.data.frame(scale(data_coef[,candidate],center = TRUE, scale = TRUE))
      model_null=lm(pollutant~-1,data_coef)
      com1 <- update(model_null, paste("~ .+ factor(orde)+", paste(candidate,collapse='+')), evaluate = FALSE)
      com1=eval.parent(com1)
      rank_forw=step(model_null,scope=formula(com1),k=log(nobs(com1)),trace=0,direction="forward")
      v_select=str_sub(rank_forw$anova$Step[-1],3)
      v_rank=left_join(v_rank,data.frame(variable=v_select,rank=1:length(v_select)))
      colnames(v_rank)[j+1]=years[j]
      datas1[[j]]=data_coef
    }
    v_rank_im=v_rank
    v_rank_im_sub=v_rank_im
    v_rank_im_sub[is.na(v_rank_im_sub)]=nrow(v_rank)
    v_rank_im_sub$average=apply(v_rank_im_sub[,-1],1,mean)
    v_rank_im$average=v_rank_im_sub$average
    v_rank_season=arrange(v_rank_im,average)
    element_order=as.character(v_rank_im$variable[order(v_rank_im$average)])
    colnames(v_rank_im)[-1]=paste0(seasons[i],colnames(v_rank_im)[(ncol(v_rank_im)-length(years)):ncol(v_rank_im)])
    if(i==1) v_rank_site=v_rank_im
    else if(i>1) v_rank_site=left_join(v_rank_site,v_rank_im)
    
    
    select_try=c()
    try_re=c()
    
    R2s=AICs=BICs=c()
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
      
      R2s=rbind(R2s,signi$R2)
      AICs=rbind(AICs,signi$AIC)
      BICs=rbind(BICs,signi$BIC)
      
      try_re=rbind(try_re,apply(signi,2,mean))
      signi$num=vv
      signi$var=element_order[vv]
      signi$year=years
      select_try=rbind(select_try,signi)
    }
    try_re=as.data.frame(try_re)
    partial=which(apply(!is.na(v_rank_season[,as.character(years)]),1,sum)>0&max(try_re$R2)-try_re$R2<=0.05)
    decide=partial[which.min(round(try_re$BIC[partial]))]
    print(element_order[decide])
    
    colnames(R2s)=paste0('R2',years)
    colnames(AICs)=paste0('AIC',years)
    colnames(BICs)=paste0('BIC',years)
    v_rank_season=cbind(v_rank_season,R2s,AICs,BICs,try_re[,c("R2","AIC","BIC")])
    v_rank_season[,c('average')]=round(v_rank_season[,c('average')],1)
    v_rank_season[,c(colnames(AICs),colnames(BICs),'AIC','BIC')]=round(v_rank_season[,c(colnames(AICs),colnames(BICs),'AIC','BIC')],0)
    v_rank_season[,c(colnames(R2s),'R2')]=round(v_rank_season[,c(colnames(R2s),'R2')],2)
    v_rank_season$variable=sapply(v_rank_season$variable,convert_variables)
    print_need=v_rank_season[,c('variable',as.character(years),'average','R2','AIC','BIC')]
    v_rank_season[is.na(v_rank_season)]='---'
    # print(xtable(apply(v_rank_season[,c(1:8,as.vector(matrix(8+1:(3*length(years)),byrow=T,nrow=3)),27:29)],2,as.character),
    #              caption=paste0(pollutant_sp,' ',rnames[u],seasons[i])),
    #       include.rownames =F)
    #Table 2
    print(xtable(apply(v_rank_season[,c('variable',as.character(years),'average','R2','AIC','BIC')],2,as.character),
                 caption=paste0(pollutant_sp,' ',rnames[u],seasons[i])),
          include.rownames =F)
    print_need$rank=1:nrow(print_need)
    print_need$season=seasons[i]
    print_need$site=rnames[u]
    v_rank_seasons=rbind(v_rank_seasons,print_need[1:decide,])
    
    select_try$season=seasons[i]
    select_try$sites=rnames[u]
    select_trys=rbind(select_trys,select_try)
    ggplot(select_try,aes(x=num,y=R2))+geom_line(aes(color=factor(year),group=factor(year)))+
      geom_vline(xintercept = decide,linetype='dashed')+labs(x='Variable',y='R Squared')+ylim(0,1)+
      #geom_text(aes(x = num, y = R2, label=paste0('AIC:',round(AIC,0),'\n','BIC:',round(BIC,0))))+
      scale_x_continuous(breaks=1:10,labels=paste0(sapply(element_order,convert_variables),'\n(',
                                                   round(v_rank_im_sub$average[order(v_rank_im_sub$average)],1),')'))+mytheme+
      theme(legend.position="bottom")
    ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/',pollutant_sp,rnames[u],seasons[i],'.png'),units="in",width=10, height=7, dpi=300)
    
    elemen_season[[i]]=element_order[1:decide]
    allls=rbind(allls,select_try[select_try$num==decide,])
  }
  tb_print=cbind(v_rank_site[,-c(seq(0,by=length(years)+1,length.out = length(seasons))+2+length(years))],
                 v_rank_site[,c(seq(0,by=length(years)+1,length.out = length(seasons))+2+length(years))])
  tb_print[,paste0(seasons,'average')]=round(tb_print[,paste0(seasons,'average')],1)
  tb_print$variable=sapply(tb_print$variable,convert_variables) 
  tb_print[is.na(tb_print)]='___'
  tb_print=apply(tb_print,2,as.character)
  print(xtable(tb_print,
               caption=paste0("Ranks of variables removed from full models for ",pollutant_sp)),
        include.rownames =F)
  #----------------------------------------
  select_num=data.frame(Variables=c('factor(orde)',candidate))
  for(yyy in 1:length(seasons))
  {select_num=left_join(select_num,data.frame(Variables=elemen_season[[yyy]],count=rep(1,length(elemen_season[[yyy]]))))
  names(select_num)[ncol(select_num)]=paste0(rnames[u],'_',seasons[yyy])}
  if(u==1) select_nums=select_num
  if(u>1) select_nums=left_join(select_nums,select_num)
}
write.csv(select_nums,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/',pollutant_sp,'selectedvariables.csv'))
write.csv(select_trys,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/',pollutant_sp,'selected.csv'))
write.csv(v_rank_seasons,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/',pollutant_sp,'rankselected.csv'))

allls$Sites=rep(rnames,each=length(seasons)*length(years))
allls$Seasons=rep(rep(seasons,each=length(years)),length(rnames))
allls$Years=rep(years,length(rnames)*length(seasons))
allls$sitename=apply(as.matrix(allls$Sites),1,convert_sites)
allls$sitename=factor(allls$sitename,levels = c('DNT, Beijing','AGW, Beijing','LSW, Tangshan','HJY, Baoding'))
allls$Seasons=factor(allls$Seasons,levels = c('spring','summer','autumn','winter'))
write.csv(allls,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/',pollutant_sp,'R2.csv'))

#------------------------------------
R2_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/PM25R2.csv",stringsAsFactors = F)
R2_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/NO2R2.csv",stringsAsFactors = F)
R2_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/SO2R2.csv",stringsAsFactors = F)
R2_PM$Pollutant='PM25'
R2_NO$Pollutant='NO2'
R2_SO$Pollutant='SO2'
R2_PNS=rbind(R2_PM,R2_NO,R2_SO)
pollutant_names=c('PM25','NO2','SO2')
min(R2_PNS$R2)
max(R2_PNS$R2)
R2_PNS$Sites=factor(R2_PNS$Sites,levels=c("DongsiTiantanNongzhanguan","GuanyuanWanliuAotizhongxin",
                                          "ShierzhongWuzijuLeidazhan","HuadianerquYouyongguanJiancezhan"),
                    labels=c('Beijing1','Beijing2','Tangshan','Baoding'))
for(ppp in 1:length(pollutant_names))
{
  ggplot(R2_PNS[R2_PNS$Pollutant==pollutant_names[ppp],],aes(x=Years,y=R2,color=factor(Seasons)))+
    geom_point()+
    #geom_line() +
    facet_wrap(.~ Sites,ncol=4, scales = "fixed")+
    mytheme_year +theme(legend.position="none")+ylim(0,1)+
    labs(x = 'Year',y ='R Squared', title = paste0('R Squared of ',pollutant_names[ppp]))
  ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/',pollutant_names[ppp],'R2.png'),units="in",width=8, height=2.8, dpi=300)
}

vs_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/PM25selectedvariables.csv",stringsAsFactors = F)
vs_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/NO2selectedvariables.csv",stringsAsFactors = F)
vs_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/SO2selectedvariables.csv",stringsAsFactors = F)
vs_total=rbind(t(vs_PM[,-(1:2)]),t(vs_NO[,-(1:2)]),t(vs_SO[,-(1:2)]))
vs_total[vs_total==1]=0
vs_total[is.na(vs_total)]=1
#1:16 17:32 33:48
part_pm=as.data.frame(vs_total[33:48,])
part_pm$season=rep(seasons,4)
for(ss in seasons)
{
  print(paste(apply(part_pm[part_pm$season==ss,paste0('V',1:10)],2,sum),collapse='&'))
}

colnames(vs_total)=sapply(vs_PM$Variables,convert_variables)
vs_total=rbind(vs_total,apply(vs_total,2,sum))
rownames(vs_total)[dim(vs_total)[1]]='Total'
vs_total[,-1]=matrix(as.character(vs_total[,-1]),nrow=nrow(vs_total))
print(xtable(vs_total,
             caption=paste0("The times of variables removed from full models for PM2.5")),
      include.rownames =T)

vr_PM=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/PM25rankselected.csv",stringsAsFactors = F)
vr_NO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/NO2rankselected.csv",stringsAsFactors = F)
vr_SO=read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/variableselectionBIC/SO2rankselected.csv",stringsAsFactors = F)
vr_PM$Pollutant='PM25'
vr_NO$Pollutant='NO2'
vr_SO$Pollutant='SO2'
vr_PNS=rbind(vr_PM,vr_NO,vr_SO)
pollutant_names=c('PM25','NO2','SO2')

r_count=data.frame(vb=sapply(c('factor(orde)',candidate),convert_variables))
n_count=1
for(pp in pollutant_names)
{
  for(ss in seasons)
  {
    freq=table(vr_PNS[vr_PNS$Pollutant==pp&vr_PNS$season==ss&vr_PNS$rank<7,'variable'])
    freq=as.data.frame(freq)
    colnames(freq)=c('vb',paste0('Freq',n_count))
    r_count=left_join(r_count,freq)
    n_count=n_count+1
  }
}
r_count$total=apply(r_count[,-1],1,sum,na.rm=T)
r_count=arrange(r_count,desc(total))
num_print=t(r_count[,-1])
colnames(num_print)=r_count$vb
num_print=as.data.frame(num_print)
num_print$season=num_print$pollu=''
num_print$season[1:(length(pollutant_names)*length(seasons))]=rep(seasons,length(pollutant_names))
num_print$pollu[1:(length(pollutant_names)*length(seasons))]=rep(pollutant_names,each=length(seasons))
print(sort(round(apply(num_print[1:4,1:10],2,sum,na.rm=T)/length(rnames)/length(seasons),2),decreasing = T))
ord1=order(round(apply(num_print[1:4,1:10],2,sum,na.rm=T)/length(rnames)/length(seasons),2),decreasing = T)
print(sort(round(apply(num_print[5:8,1:10],2,sum,na.rm=T)/length(rnames)/length(seasons),2),decreasing = T))
ord2=order(round(apply(num_print[5:8,1:10],2,sum,na.rm=T)/length(rnames)/length(seasons),2),decreasing = T)
print(sort(round(apply(num_print[9:12,1:10],2,sum,na.rm=T)/length(rnames)/length(seasons),2),decreasing = T))
ord3=order(round(apply(num_print[9:12,1:10],2,sum,na.rm=T)/length(rnames)/length(seasons),2),decreasing = T)

num_print[1:(length(pollutant_names)*length(seasons)),1:10]=round(num_print[1:(length(pollutant_names)*length(seasons)),1:10]/length(rnames),2)
num_print[nrow(num_print),1:10]=round(num_print[nrow(num_print),1:10]/(length(rnames)*length(pollutant_names)*length(seasons)),2)

print(xtable(num_print[,c(11:12,1:10)],
             caption='frequency'),
      include.rownames =F)
print(xtable(num_print[1:4,ord1],
             caption='frequency'), include.rownames =F)
print(xtable(num_print[5:8,ord2],
             caption='frequency'), include.rownames =F)
print(xtable(num_print[9:12,ord3],
             caption='frequency'), include.rownames =F)

for(i in 2:ncol(r_count))
  print(as.character(r_count$vb[order(r_count[,i],decreasing=T)]))

fre_print=c()
for(i in 1:12)
{
  ords=order(num_print[i,1:10],decreasing = T)
  fre_print=rbind(fre_print,colnames(num_print)[ords])
  fre_print=rbind(fre_print,as.character(num_print[i,ords]))
}
fre_print[fre_print=='NA']='0'
#Table 3
print(xtable(fre_print,
             caption='frequency'), include.rownames =F)
