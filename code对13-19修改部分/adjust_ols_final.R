#-------------map-------------------
# install.packages("remotes")
# remotes::install_github("badbye/baidumap")
# library(baidumap)
library(ggmap)
# options(baidumap.key = 'X8acHNtnlKfIcHS9QagCkAuOGBGVlASv')
# lon_lats$color=rep(c(2,1,3,4),each=3)
# ggmap(getBaiduMap(location = c(lon=116.38145,lat=39.93303),zoom = 9))+
#   geom_point(data = lon_lats[lon_lats$province=='Beijing',],aes(x = lon, y =lat,color=factor(color)),size = 2)

ggmap::register_google(key = "AIzaSyDqQgt85Q6rDDdlYjQj0teyfVd3Zm6KG_Y")
devtools::install_github("fresques/ggmap")
# check if key is saved
has_goog_key()
#> [1] TRUE
library(httr)
library(plyr)
#vpn 启用HTTP
set_config(use_proxy(url="http://localhost", port=10818)) 

# cities=c('beijing','baoding','tangshan')
# locations=as.data.frame(adply(cities,1,geocode))
# locations$city=cities
mapbg <- get_map(location = c(116.5,39.5), 
                 zoom = 7,
                 maptype = "terrain",
                 source = 'google',language = "en-EN")
ggmap(mapbg,extent =  "device") +
  geom_point(data = lon_lats,aes(x = lon, y =lat, color=factor(color)),size = 1) +
  # geom_vline(xintercept = c(115.75, 116, 116.25, 116.5, 116.75, 117, 117.25), 
  #            linetype = 'dashed', color = 'black')+
  # geom_hline(yintercept = c(39.5, 39.75, 40, 40.25, 40.5, 40.75),
  #            linetype = 'dashed', color = 'black') + 
  labs(x= 'Longitude',y='Latitude',color='Cluster')+
  scale_color_manual(values=c("#FF0000","#3300FF",'#CC9933','#009900'))+
  theme(legend.position = 'bottom')
ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/location.png',units="in",width=4, height=4, dpi=300)

pathsavee="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes/"
pathsavees="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat/"
pathmdatasave="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/modeldata/"
eplen=7
#eplen=9 #overall median of the length of episode
rnames=c("DongsiTiantanNongzhanguan","GuanyuanWanliuAotizhongxin","ShierzhongWuzijuLeidazhan","HuadianerquYouyongguanJiancezhan")
allnames=paste0(rep(rnames,each=length(ysepn)),rep(ysepn,length(rnames)))

alllength=data.frame(name=allnames,num=rep(0,length(allnames)))
for(m in 1:length(allnames))
{
  ep=read.csv(paste0(pathsavee,allnames[m],'_ep.csv'), stringsAsFactors = FALSE)
  ep1=ep[ep$orde==1,c('ep_id','orde','hour')]
  
  id_period=ep1$ep_id[intersect(which(ep1$hour %in% berange),which(table(ep$ep_id)>=eplen))]
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
library(xtable)

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
#eplen=9 #overall median of the length of episode
pathsavegp="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/olsplot_new/"
pathsaveform="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/"
berange=6:18#range of the beginning of episodes 
years=year1:year2
seasons=c('spring','summer','autumn','winter')
pollutant_sp='PM2.5' #'NO2' #'SO2'
ysepn <-paste0(rep(years,length(seasons)),rep(seasons,each=length(years)))

#--------------------------------------------
candidate=c('TEMP','PRES','DEWP','blh','HUMI',
            'INws','ISws',
            'epfmINws','epfNen')
#---------------------------------------------
variables_all=c(candidate,paste0('factor(orde)',2:eplen))

allls=c()
origin_adjust=c()
compared=c()
coeffises=c()
u=i=j=1
for(u in 1:length(rnames))
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
      id_period=d1$ep_id[which((d1$hour %in% berange)& poll_s<poll_6)]#starting time in the range
      
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
      
      # #A joint LM test for random effects and serial correlation under normality and homoskedasticity of the idiosyncratic errors
      # pbsytest(pollutant ~ factor(orde) + TEMP + PRES +
      #            DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"),
      #          test = "j")
      # pbsytest(pollutant ~ factor(orde) + TEMP + PRES +
      #                  DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"),
      #                test = "re")
      # pbsytest(pollutant ~ factor(orde) + TEMP + PRES +
      #                  DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"),
      #                test = "ar")
      # #Wooldridge’s test for serial correlation in “short” FE panels
      # pwartest(pollutant ~ factor(orde) + TEMP + PRES +
      #            DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"))
      # #Modified BNF–Durbin–Watson Test for AR(1) disturbances in panel models.
      # pbnftest(pollutant ~ factor(orde) + TEMP + PRES +
      #            DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"),model="pooling")
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
      # #residuals
      # ggplot(p.residual,aes(x=orde, y=residuals))+
      #   geom_point(size=0.5)+geom_line()+
      #   geom_hline(yintercept=0,color = "black",linetype=3,size=0.5,alpha=1)+
      #   facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + mytheme1+
      #   labs(x = "Hour", y = 'Residuals')
      # ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],years[k],'residuals.png'),units="in",width=8, height=8, dpi=300)
      # 
      # #fitted
      # ggplot(datas1[[k]],aes(x=orde, y=pollutant,group=factor(ep_id)))+
      #   geom_point(size=0.5)+geom_line()+
      #   geom_line(aes(x=orde, y=fitted,group=factor(ep_id)),color = "red",size=0.4,alpha=1)+
      #   facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + mytheme1+
      #   labs(x = "Hour", y = 'Fitted values')
      # ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],years[k],'fitted.png'),units="in",width=5,
      #        height=ceiling(length(unique(datas1[[k]]$ep_id))/8)*1.4, dpi=300)
      
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
                   "2016"=modef1[[4]],"2017"=modef1[[5]],"2018"=modef1[[6]],"2019"=modef1[[7]]))
    
    # screenreg(list("2013"=modeplm[[1]],"2014"=modeplm[[2]],"2015"=modeplm[[3]],
    #                "2016"=modeplm[[4]],"2017"=modeplm[[5]],"2018"=modeplm[[6]],"2019"=modeplm[[7]]))
    
    # texreg(list("2013"=modef1[[1]],"2014"=modef1[[2]],"2015"=modef1[[3]],
    #             "2016"=modef1[[4]],"2017"=modef1[[5]],"2018"=modef1[[6]],"2019"=modef1[[7]]),
    #        caption=paste0("Results of linear models for data in ",seasons[i]," of ",rnames[u]))
    all_results=left_join(left_join(left_join(left_join(left_join(out_print[[1]],out_print[[2]]),out_print[[3]]),out_print[[4]]),out_print[[5]]),out_print[[6]])
    print(xtable(all_results,
                 caption=paste0("Results of selected models for ",seasons[i]," of ",rnames[u])),
          include.rownames =F)
    
    # ggplot(p_residuals[p_residuals$orde<=eplen,],aes(x = factor(years), y =residuals, fill =factor(orde)))+ 
    #   geom_boxplot()+labs(x = "Year", y = 'Residuals',fill = "Hour")+scale_fill_npg()+
    #   #theme(legend.position = c(0.7, 0.2),legend.direction = "horizontal")+
    #   mytheme1 
    # # +stat_summary(fun.y="mean", geom="point", size=5,
    # #              position=position_dodge(width=0.75), shape = 8, size = 1, color="white")
    # ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'residuals.png'),units="in",width=5, height=4, dpi=300)
    # 
    # ggplot(adjust_ba,aes(x=orde, y=adjusted,group=factor(year)))+
    #   #geom_point(size=0.5,color="blue")+
    #   geom_line(color="blue")+
    #   geom_ribbon(aes(x =orde, ymin=adjusted-se*qnorm(1-0.05/2), ymax =adjusted+se*qnorm(1-0.05/2)),linetype=2,fill='blue',alpha = 0.2) +
    #   geom_line(aes(x=orde, y=pollutant),size=0.4,alpha=1,color="red")+
    #   scale_x_continuous(breaks =2*(1:ceiling(eplen/2)))+
    #   facet_wrap(.~ year,ncol=6, scales = "fixed") + 
    #   labs(x = "Hour", y = paste0('Average Growth'))+mytheme
    # ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'adjustedfacet.png'),units="in",width=6, height=1.5, dpi=300)
    # 
    # ggplot(adjust_ba,aes(x=orde, y=adjusted,group=factor(year),color=factor(year)))+
    #   geom_point(size=0.5)+geom_line()+
    #   geom_ribbon(aes(x =orde, ymin=adjusted-se*qnorm(1-0.05/2), ymax =adjusted+se*qnorm(1-0.05/2),fill=factor(year)),linetype=2,alpha = 0.2) +
    #   labs(x = "Hour", y = paste0('Adjusted Growth of ',pollutant_sp))+mytheme+
    #   scale_fill_brewer(palette = "RdBu")+scale_color_brewer(palette = "RdBu")
    # ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'adjusted.png'),units="in",width=4, height=3, dpi=300)
    
    metero=data.frame()
    for(uuu in 1:length(elemen_need))
    {
      metero_ob=adjust_ba[,c('orde','year',elemen_need[uuu])]
      names(metero_ob)[3]='values'
      metero_ob$type=elemen_need[uuu]
      metero=rbind(metero,metero_ob)
    }
    # ggplot(metero[metero$orde>1,],aes(x=orde, y=values,color=factor(year)))+
    #   geom_point(size=0.5)+geom_line()+
    #   facet_wrap(.~ type,ncol=7, scales = "free") + 
    #   labs(x = "Hour", y = paste0('The Average Change of Meteorological Variables'))+mytheme
    # ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'meteorological.png'),units="in",width=7, height=4, dpi=300)
    
    adjust_ba$season=seasons[i]
    adjust_bas=rbind(adjust_bas,adjust_ba)
  }
  ggplot(adjust_bas,aes(x=orde, y=adjusted))+
    #geom_point(size=0.5,color="blue")+
    geom_line(color="blue")+
    geom_ribbon(aes(x =orde,ymin=adjusted-se*qnorm(1-0.05/2), ymax =adjusted+se*qnorm(1-0.05/2)),linetype=2,fill='blue',alpha = 0.2) +
    geom_line(aes(x=orde, y=pollutant),size=0.4,alpha=1,color='red')+
    facet_grid(season~year,scales = "fixed")+
    labs(x = "Hour", y = paste0('Average Growth'))+mytheme
  ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],'adjustedfacet.png'),units="in",width=8, height=8, dpi=300)
  
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
  
  ggplot(coeffis[!is.na(coeffis$Estimates)&coeffis$Name!='Time dummies',],aes(x=Years, y=Estimates,group=Seasons,color=factor(Seasons)))+
    geom_point(aes(shape=factor(shape)),size=1.5)+geom_line(size=0.5)+
    #geom_text(aes(x=Years,y=Estimates,label=Signifance),color='black',vjust=0)+
    geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
    facet_wrap(~Name,ncol=5,scales = "free_y") + 
    labs(x = "Year", y ='Estimates of Coefficients',color='Seasons')+mytheme_year+
    theme(legend.position="top")
  ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],'coefficients.png'),units="in",width=10, height=5, dpi=300)
  #----------------------------------------
  
  adjust_bas$season=factor(adjust_bas$season,levels=seasons)
  adjust_bas$site=rnames[u]
  # ggplot(adjust_bas[adjust_bas$orde==eplen,],aes(x=factor(season),y=adjusted/(orde-1),fill=factor(year)))+
  #   geom_bar(stat='identity', position='dodge') +
  #   geom_errorbar(aes(x =factor(season), ymin=(adjusted-se*qnorm(1-0.05/2))/(orde-1), ymax =(adjusted+se*qnorm(1-0.05/2))/(orde-1)), color = "grey40",
  #                 alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  #   mytheme_year +theme(legend.position="top")+
  #   scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#66CCFF','#3366CC'))+
  #   # scale_fill_brewer(palette = "RdBu")+
  #   # scale_color_brewer(palette = "RdBu")+
  #   labs(x = 'Year',y =paste0('Growth Rate of ',pollutant_sp), title = paste0('Growth rate of ',pollutant_sp,' for ',rnames[u]))
  # ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],'adjusted.png'),units="in",width=5, height=4, dpi=300)
  
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
  scale_fill_manual(values=c('#CC0000',"#CC99FF",'#FF9999','#FFCC66','#00CC33','#66CCFF','#3366CC'))+
  labs(x = 'Year',y =paste0('Adjusted Growth Rate of ',pollutant_sp), title = paste0('Growth rate of ',pollutant_sp))
ggsave(paste0(pathsavegp,pollutant_sp,'adjusted.png'),units="in",width=8, height=3.6, dpi=300)


colnames(allls)=c('Number','R2','Adj.R2')
allls=as.data.frame(allls)
allls$Sites=rep(rnames,each=length(seasons)*length(years))
allls$Seasons=rep(rep(seasons,each=length(years)),length(rnames))
allls$Years=rep(years,length(rnames)*length(seasons))

allls$sitename=factor(allls$Sites,levels = rnames,
                      labels = c('Beijing SE','Beijing NW','Tangshan','Baoding'))
allls$Seasons=factor(allls$Seasons,levels = c('spring','summer','autumn','winter'),
                     labels=c('Spring','Summer','Autumn','Winter'))
write.csv(allls,file=paste0(pathsaveform,pollutant_sp,'R2.csv'))
