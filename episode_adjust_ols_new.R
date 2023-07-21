install.packages("remotes")
remotes::install_github("badbye/baidumap")
library(baidumap)
library(ggmap)
options(baidumap.key = 'X8acHNtnlKfIcHS9QagCkAuOGBGVlASv')
ggmap(getBaiduMap(location = c(lon=116.89100, lat=39.41987),zoom = 12))
mapbg <- get_map(location = (apply(lon_lats[,c('lon','lat')],2,min)+apply(lon_lats[,c('lon','lat')],2,max))/2, 
                 zoom = 9,
                 maptype = 'terrain',
                 source = 'google')
g <- ggmap(mapbg) +
  geom_segment(data = windarrow, 
               aes(x = lon_start, xend = lon_end, y = lat_start, yend = lat_end),
               arrow = arrow(length = unit(0.5,"cm"))) +
  geom_point(data = stadata, 
             aes(x = lng, y =lat, color = pollevel),size = 3) +
  geom_vline(xintercept = (c(115.75, 116, 116.25, 116.5, 116.75, 117, 117.25) - halfsize), 
             linetype = 'dashed', color = 'black')+
  geom_hline(yintercept = (c(39.5, 39.75, 40, 40.25, 40.5, 40.75) - halfsize),
             linetype = 'dashed', color = 'black') + 
  scale_x_continuous(name = 'Longitude') + 
  scale_y_continuous(name = 'Latitude', breaks = c(39.5, 39.75, 40, 40.25, 40.5, 40.75))+
  scale_color_manual(name = '污染等级',
                     values=levelcolorlist[['color']])+
  ggtitle(label = paste0(year,'-',month,'-',day,' ', hour,':00:00'))+
  theme(legend.position = 'bottom')

pathsavee="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_new/"
pathsavees="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodesstat_new/"
pathmdatasave="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/modeldata_new/"
eplen=7
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
convert_variables <- function(x)
{
  if(x=='(Intercept)')
    return('Hour2')
  else if(x=='epfmINws')
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
  else if(x%in%c('TEMP','PRES','DEWP'))
    return(paste0('D(',x,')'))
  else 
    return(paste0('Hour',substr(x,13,13),'-Hour2'))
}
convert_sites <- function(x)
{
  if(x=="DongsiTiantanNongzhanguan")
    return('Southeast of Beijing')
  else if(x=="GuanyuanWanliuAotizhongxin")
    return('Northwest of Beijing')
  else if(x=="ShierzhongWuzijuLeidazhan")
    return('Tangshan')
  else if(x=="HuadianerquYouyongguanJiancezhan")
    return('Baoding')
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
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_blank(),
                              #legend.title = element_blank(),
                              legend.text = element_text(size=10, face = 'bold'),
                              legend.key.width  = unit(.3,"inches"),
                              legend.key.height = unit(.3,"inches"),
                              panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 
set.seed(0)
pathsavegp="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/olsplot_new/"
berange=6:18#range of the beginning of episodes 
years=2013:2018
pollutant_sp='PM2.5'
if(pollutant_sp=='PM2.5') len_elem=9
if(pollutant_sp=='NO2') len_elem=7
if(pollutant_sp=='SO2') len_elem=9
ysepn <-paste0(rep(years,length(seasons)),rep(seasons,each=length(years)))
variables_all=c("(Intercept)","TEMP","PRES","DEWP","blh","HUMI","WSPM",
                "uwind","vwind","Iwu","Iwv","epfmINws","epfNen","pmf35",
                "factor(orde)3","factor(orde)4","factor(orde)5","factor(orde)6","factor(orde)7")

allls=c()

origin_adjust=c()
compared=c()
for(u in 1:length(rnames))
{
  adjust_bas=c()
  elemen_season=vector("list",length(seasons))
  coeffis=c()
  for(i in 1:length(seasons))
  {
    #datas=vector("list",length(years))
    datas1=vector("list",length(years))
    
    #modef=vector("list",length(years))
    modeo=vector("list",length(years))
    modef1=vector("list",length(years))
    modeli1=vector("list",length(years))
    modeplm=vector("list",length(years))
    out_print=vector("list",length(years))
    
    new=matrix(rep(0,(eplen-1)*24),nrow=eplen-1)#value for ajustment
    
    mean_or_ad=vector("list",length(years))
    adjust_ba=c()
    #elemen1=c()
    elemen2=c()
    elemen_year=c()
    elements=vector("list",length(years))
    ranks=c()
    test_pv=c()
    p_residuals=c()
    #pdf(file= paste0(pathsavegp,'gls23',rnames[u],'_',seasons[i],'.pdf'),height = 12,width=12,family = 'GB1')
    for(j in 1:length(years))
    {
      d<- read.csv(paste0(pathmdatasave,rnames[u],ysepn[(i-1)*length(years)+j],'_eptreat.csv'),
                   stringsAsFactors = FALSE)
      d$pollutant=d[,pollutant_sp]
      d_orde=d[d$orde<=eplen,]
      idna=unique(d_orde$ep_id[is.na(d_orde$pollutant)])
      d=d[!d$ep_id%in%idna,]
      sta=read.csv(paste0(pathsavees,rnames[u],ysepn[(i-1)*length(years)+j],'_epstat.csv'),
                   stringsAsFactors = FALSE)
      sta$epfpen[which(is.na(sta$epfpen))]=0
      sta$epfpen48[which(is.na(sta$epfpen48))]=0
      sta$X=1:nrow(sta)
      sta=sta[setdiff(sta$X,idna),]
      
      d1=d[d$orde==1,c('ep_id','orde','hour')]
      id_period=d1$ep_id[intersect(which(d1$hour %in% (berange)),
                                   which(table(d$ep_id)>=eplen))]
      
      print(length(id_period))
      if(length(id_period)>0)
      {
        d=d[which(d$ep_id%in%id_period),]
        d=d[d$orde<=eplen,]
        sta=sta[which(sta$X%in%id_period),]
      }
      
      # stas[[j]]=sta
      # #HUMI, blh
      # plot(density(d$blh))
      # plot(density(log(d$blh)))
      da=d[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv',
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
      
      origin=da[which(da$orde==1),c('TEMP','PRES','DEWP','blh','HUMI','WSPM','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv','pollutant')]
      repe=c()
      for(k in 1:length(id_period))
      {
        repe=rbind(repe,matrix(unlist(rep(as.vector(origin[k,]),each=eplen)),nrow=eplen))
      }
      colnames(repe)=c('TEMP','PRES','DEWP','blh','HUMI','WSPM','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv','pollutant')
      
      dataa=as.matrix(da)  
      col_sub=which(colnames(dataa)%in%c('TEMP','PRES','DEWP','blh','HUMI','WSPM','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv','pollutant'))
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
      
      # step(lm(pollutant~TEMP+PRES+DEWP+blh+HUMI+WSPM+uwind+vwind+uIws+vIws+Iwu+Iwv+IIwu+IIwv+
      #           epfmINws+epfpn+epfNen+epfpen+pmf35+epfmINws48+epfpn48+epfpen48+epfNen48+
      #           factor(orde),dataaa1))
      
      dataaa1_dummy=dummy_cols(dataaa1,select_columns = c("orde"),remove_first_dummy = TRUE)
      # dataaa1_feature=dataaa1_dummy[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM',
      #                 'uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv',
      #                 'epfmINws','epfpn','epfNen','epfpen','pmf35','epfmINws48','epfpn48',
      #                 'epfpen48','epfNen48',paste0('orde_',3:eplen))]
      #-----------------------------------------
      dataaa1_feature=cbind(as.data.frame(scale(dataaa1_dummy[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM',
                                                                 'uwind','vwind','Iwu','Iwv',
                                                                 'epfmINws','epfNen')],center = TRUE, scale = TRUE)),
                            dataaa1_dummy[,paste0('orde_',3:eplen)])
      dataaa1_group=c(1:(ncol(dataaa1_feature)-eplen+2),rep(ncol(dataaa1_feature)-eplen+3,eplen-2))
      dataaa1_cv <- cv.gglasso(x=as.matrix(dataaa1_feature),y=dataaa1_dummy$pollutant,
                               group=dataaa1_group,loss="ls",
                               pred.loss="L2", nfolds=5)
      
      dataaa1_try <- gglasso(x=as.matrix(dataaa1_feature),y=dataaa1_dummy$pollutant,group=dataaa1_group,loss="ls")
      #最先出来的那个labmbda对应的path
      rank_ob=apply(dataaa1_try$beta,1,path_min)
      rank_ob[is.infinite(rank_ob)]=NA
      ranks=rbind(ranks,rank(rank_ob,na.last="keep"))
      # the coefficients at lambda = lambda.1se
      coeff2=coef(dataaa1_cv$gglasso.fit,s=dataaa1_cv$lambda.min)
      elemen2=c(elemen2,names(coeff2[setdiff(which(coeff2!=0),c(1,(length(coeff2)-eplen+3):length(coeff2))),]))
      ele_selected=names(coeff2[setdiff(which(coeff2!=0),1),])
      if(length(ele_selected)>0) {elements[[j]]=ele_selected
      elemen_year=c(elemen_year,ele_selected)}
      # #Functions rstandard and rstudent give the standardized and Studentized residuals respectively.
      # dataaa$residual=rstandard(lm(pollutant ~ TEMP + DEWP + blh + HUMI + WSPM + vwind + 
      #                                vIws + Iwu + IIwu + IIwv + epfpn + epfpen + factor(orde), 
      #                              data = dataaa))
      # dataaa1$residual=rstandard(lm(pollutant ~ PRES + WSPM + vwind + vIws + 
      #                                 Iwu + IIwu + epfmINws + epfpn + epfNen + epfpen + epfpn48 + 
      #                                 epfpen48 + epfNen48 + factor(orde), data = dataaa1))
      # datas[[j]]=dataaa
      datas1[[j]]=dataaa1
      #original
      
      mm=as.data.frame(dataaa1 %>%
                         group_by(orde) %>%
                         summarise(TEMP = mean(TEMP),PRES=mean(PRES),DEWP=mean(DEWP),blh=mean(blh),HUMI = mean(HUMI),WSPM = mean(WSPM),
                                   uwind=mean(uwind),vwind=mean(vwind),uIws= mean(uIws),vIws=mean(vIws),Iwu=mean(Iwu),Iwv = mean(Iwv),
                                   IIwu=mean(IIwu),IIwv = mean(IIwv),epfmINws=mean(epfmINws),epfpn=mean(epfpn),epfNen= mean(epfNen),
                                   epfpen=mean(epfpen),pmf35=mean(pmf35),epfmINws48=mean(epfmINws48),epfpn48=mean(epfpn48),
                                   epfpen48=mean(epfpen48),epfNen48=mean(epfNen48),pollutant=mean(pollutant)))
      mm=inner_join(mm,as.data.frame(dataaa1 %>%group_by(orde) %>%
                                       summarise(SE=sd(pollutant)/sqrt(length(unique(dataaa1$ep_id))))))
      mm$year=rep(years[j],eplen-1)
      mean_or_ad[[j]]=mm
      
      new=new+as.matrix(mm[,c('orde','TEMP','PRES','DEWP','blh','HUMI','WSPM',
                              'uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv',
                              'epfmINws','epfpn','epfNen','epfpen','pmf35','epfmINws48','epfpn48',
                              'epfpen48','epfNen48')])
    }
    #sort(table(elemen1),decreasing = T)
    sort(table(elemen2),decreasing = T)
    elemen_season[[i]]=sort(table(elemen_year),decreasing = T)
    new=new/length(years)
    (name_rank=names(sort(apply(ranks[,-c((ncol(ranks)-eplen+3):ncol(ranks))],2,sum),na.last=T)))
    
    # #AIC and BIC
    # ABICs=c()
    # for(ltt in 1:length(unique(elemen2))) {
    #   ABIC=c(0,0)
    #   for(kkk in 1:length(years))
    #   {
    #     elemen_try=intersect(unique(elemen2),name_rank[1:ltt])
    #     M0=lm(pollutant ~ 1,data=datas1[[kkk]])
    #     com0 <- update(M0, paste("~ .+ factor(orde)+", paste(elemen_try,collapse='+')), evaluate = FALSE)
    #     com0=eval.parent(com0)
    #     ABIC=ABIC+c(AIC(com0),BIC(com0))
    #   }
    #   ABICs=rbind(ABICs,c(ltt,ABIC))
    # }
    # ltt_choose=apply(ABICs[,2:3],2,which.min)
    # len_elem=ltt_choose[2]
    
    #five-folds CV
    ABICs=c()
    for(ltt in 1:length(unique(elemen2))) {
      ABIC=0
      for(kkk in 1:length(years))
      {
        elemen_try=intersect(unique(elemen2),name_rank[1:ltt])
        M0=lm(pollutant ~ 1,data=datas1[[kkk]])
        com0 <- update(M0, paste("~ .+ factor(orde)+", paste(elemen_try,collapse='+')), evaluate = FALSE)
        com0=eval.parent(com0)
        ols_cv=train(formula(com0),data=datas1[[kkk]],trControl=trainControl(method="cv", number=5), method = "lm")
        ABIC=ABIC+ols_cv$results[2]
      }
      ABICs=rbind(ABICs,c(ltt,ABIC))
    }
    ltt_choose=which.min(ABICs[,2])
    len_elem=ltt_choose
    
    elemen_final=intersect(unique(elemen2),name_rank[1:len_elem])
    
    k=1
    {
      # M0=lm(pollutant ~ 1,data=datas[[k]])
      # com <- update(M0, paste("~ .+ factor(orde)+", paste(unique(elemen1),collapse='+')), evaluate = FALSE)
      # com=eval.parent(com)
      # modef[[k]]=com
      
      # modeo[[k]]=lm(pollutant ~ TEMP +PRES+ DEWP + blh + HUMI + WSPM + 
      #                 uwind+vwind + Iwu+Iwv+
      #                 epfmINws+ epfNen +pmf35+ factor(orde),data=datas1[[k]])
      
      M1=lm(pollutant ~ 1,data=datas1[[k]])
      com1 <- update(M1, paste("~ .+ factor(orde)+", paste(elemen_final,collapse='+')), evaluate = FALSE)
      com1=eval.parent(com1)
      plmmod=plm(formula(com1), data=datas1[[k]],index=c("ep_id","orde"),model = "pooling")
      
      #final linear model
      modef1[[k]]=com1
      #individual linear model
      if(length(elements[[k]])>0)
        modeli1[[k]]=eval.parent(update(M1, paste("~ .+ factor(orde)+", paste(setdiff(elements[[k]],paste0('orde_',3:eplen)),collapse='+')), evaluate = FALSE))
      
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
      plmtest(plmmod, c("individual"), type=("bp"))
      #Semi-parametric test for the presence of individual unobserved effects in panel models.
      test1=pwtest(plmmod)
      print(test1)
      #test the presence of heteroskedasticity
      test2=bptest(plmmod,studentize=F)
      print(test2)
      #Modified BNF–Durbin–Watson Test for AR(1) disturbances in panel models.
      test3=pbnftest(plmmod)
      print(test3)
      
      test_pv=cbind(test_pv,c(test1$p.value,test2$p.value))
      modeplm[[k]]=plmmod
      residual=plmmod$residuals
      datas1[[k]]$residuals=residual[order(as.numeric(names(residual)))]
      datas1[[k]]$fitted=datas1[[k]]$pollutant-datas1[[k]]$residuals
      
      # as.matrix(vcovG(plmmod, cluster = "group", inner = "cluster", l = 0))
      # coeftest(plmmod, vcov = Vcx)
      
      boxplot(residuals~orde, data=datas1[[k]])
      p.residual=datas1[[k]][,c('ep_id','orde')]
      p.residual=rbind(p.residual,p.residual)
      if(length(elements[[k]])==0) p.residual$residuals=c(rep(NA,nrow(datas1[[k]])),modef1[[k]]$residuals)
      else p.residual$residuals=c(modeli1[[k]]$residuals,modef1[[k]]$residuals)
      
      p.residual$models=rep(c('individual lm model','final lm model'),each=nrow(datas1[[k]]))
      p.residual$years=years[k]
      p_residuals=rbind(p_residuals,p.residual)
      #residuals
      ggplot(p.residual,aes(x=orde, y=residuals,color=models))+
        geom_point(size=0.5)+geom_line()+
        geom_hline(yintercept=0,color = "black",linetype=3,size=0.5,alpha=1)+
        facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + mytheme1+
        labs(x = "Hour", y = 'Residuals',Color="Models")
      ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],years[k],'residuals.png'),units="in",width=8, height=8, dpi=300)
      
      #fitted
      ggplot(datas1[[k]],aes(x=orde, y=pollutant,group=factor(ep_id)))+
        geom_point(size=0.5)+geom_line()+
        geom_line(aes(x=orde, y=fitted,group=factor(ep_id)),color = "red",size=0.4,alpha=1)+
        facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + mytheme1+
        labs(x = "Hour", y = 'Fitted values')
      ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],years[k],'fitted.png'),units="in",width=5, 
             height=ceiling(length(unique(datas1[[k]]$ep_id))/8)*1.4, dpi=300)
      
      out_result=as.data.frame(coeftest(plmmod, vcov = Vcx)[,1:4])
      out_print[[k]]=data.frame(Variables=c(rownames(out_result),'R2','Adj.R2','Number of panels','RMSE'),
                                Estimate=c(paste0(round(out_result$Estimate,2),'(',round(out_result$`Std. Error`,2),')',apply(as.matrix(out_result$`Pr(>|t|)`),1,significance)),
                                           round(summary(modeplm[[k]])$r.squared[1],2),round(summary(modeplm[[k]])$r.squared[2],2),length(unique(datas1[[k]]$ep_id)),
                                           round(sqrt(sum((summary(modeplm[[k]])$residuals)^2)/summary(modeplm[[k]])$df[2]),2)))
      names(out_print[[k]])[2]=as.character(years[k])
      coeffi=data.frame(Variables=variables_all,Estimates=NA,Signifance=NA,Years=years[k],Seasons=seasons[i])
      coeffi$Estimates[match(rownames(out_result),variables_all)]=out_result$Estimate
      coeffi$Signifance[match(rownames(out_result),variables_all)]=apply(as.matrix(out_result$`Pr(>|t|)`),1,significance)
      coeffis=rbind(coeffis,coeffi)
      
      allls=rbind(allls,c(length(unique(datas1[[k]]$ep_id)),summary(modeplm[[k]])$r.squared))
      # var_ob=matrix(rep(0,2*(eplen-1)),ncol=2)
      # for(uuu in 1:length(years))
      # {
      #   var_add=data.frame(orde=datas1[[uuu]]$orde,
      #                      pro=as.matrix(datas1[[uuu]][,elemen_final])%*%out_result$Estimate[which(rownames(out_result)%in%elemen_final)])
      #   var_add_ob=as.matrix(var_add%>%group_by(orde)%>%summarise(var=var(pro)))
      #   var_add_ob[,2]=var_add_ob[,2]/length(unique(datas1[[uuu]]$ep_id))
      #   var_ob=var_ob+var_add_ob
      # }
      # varx_add=var_ob[,2]/length(years)^2
      
      var_ob=matrix(rep(0,(eplen-1)*(eplen-1)),ncol=eplen-1)
      for(uuu in 1:length(years))
      {
        var_add=data.frame(orde=datas1[[uuu]]$orde,
                           pro=as.matrix(datas1[[uuu]][,elemen_final])%*%out_result$Estimate[which(rownames(out_result)%in%elemen_final)])
        var_ob=var_ob+var(matrix(var_add$pro,ncol=eplen-1,byrow=T))/length(unique(datas1[[uuu]]$ep_id))
      }
      varx_add=var_ob/length(years)^2
      
      new_du=dummy_cols(as.data.frame(new),select_columns = c("orde"),remove_first_dummy = TRUE)[,-1]
      names(new_du)[which(names(new_du)%in%paste0('orde_',3:eplen))]=paste0('factor(orde)',3:eplen)
      new_du$intercept=rep(1,nrow(new_du))
      names(new_du)[ncol(new_du)]='(Intercept)'
      
      Vvar=as.matrix(vcovG(plmmod, cluster = "group", inner = "cluster", l = 0))
      AVar_theta=as.matrix(new_du[,colnames(Vvar)])%*%Vvar%*%t(as.matrix(new_du[,colnames(Vvar)]))
      Vadjust=AVar_theta+varx_add
      mean_or_ad[[k]]$adjusted=as.matrix(new_du[,rownames(out_result)])%*%out_result$Estimate
      mean_or_ad[[k]]$se=sqrt(diag(Vadjust))
      mean_or_ad[[k]]$ad.lb=mean_or_ad[[k]]$adjusted-mean_or_ad[[k]]$se*qnorm(1-0.05/2)
      mean_or_ad[[k]]$ad.ub=mean_or_ad[[k]]$adjusted+mean_or_ad[[k]]$se*qnorm(1-0.05/2)
      mean_or_ad[[k]]=rbind(data.frame(orde=1,TEMP=0,PRES=0,DEWP=0,blh=0,HUMI=0,WSPM=0,
                                       uwind=0,vwind=0,uIws=0,vIws=0,Iwu=0,Iwv=0,IIwu=0,IIwv=0,
                                       epfmINws=0,epfpn=0,epfNen=0,epfpen=0,pmf35=0,
                                       epfmINws48=0,epfpn48=0,epfpen48=0,epfNen48=0,
                                       pollutant=0,SE=0,year=years[k],adjusted=0,ad.lb=0,ad.ub=0,se=0),mean_or_ad[[k]])
      
      adjust_ba=rbind(adjust_ba,mean_or_ad[[k]])
      
      {
        AVar0=AVar_theta
        theta0=out_result$Estimate
      }
    }
    
    for(k in 2:length(years))
    {
      # M0=lm(pollutant ~ 1,data=datas[[k]])
      # com <- update(M0, paste("~ .+ factor(orde)+", paste(unique(elemen1),collapse='+')), evaluate = FALSE)
      # com=eval.parent(com)
      # modef[[k]]=com
      
      # modeo[[k]]=lm(pollutant ~ TEMP +PRES+ DEWP + blh + HUMI + WSPM + 
      #                 uwind+vwind + Iwu+Iwv+
      #                 epfmINws+ epfNen +pmf35+ factor(orde),data=datas1[[k]])
      
      M1=lm(pollutant ~ 1,data=datas1[[k]])
      com1 <- update(M1, paste("~ .+ factor(orde)+", paste(elemen_final,collapse='+')), evaluate = FALSE)
      com1=eval.parent(com1)
      plmmod=plm(formula(com1), data=datas1[[k]],index=c("ep_id","orde"),model = "pooling")
      
      #final linear model
      modef1[[k]]=com1
      #individual linear model
      if(length(elements[[k]])>0)
        modeli1[[k]]=eval.parent(update(M1, paste("~ .+ factor(orde)+", paste(setdiff(elements[[k]],paste0('orde_',3:eplen)),collapse='+')), evaluate = FALSE))
      
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
      plmtest(plmmod, c("individual"), type=("bp"))
      #Semi-parametric test for the presence of individual unobserved effects in panel models.
      test1=pwtest(plmmod)
      print(test1)
      #test the presence of heteroskedasticity
      test2=bptest(plmmod,studentize=F)
      print(test2)
      #Modified BNF–Durbin–Watson Test for AR(1) disturbances in panel models.
      test3=pbnftest(plmmod)
      print(test3)
      
      test_pv=cbind(test_pv,c(test1$p.value,test2$p.value))
      modeplm[[k]]=plmmod
      residual=plmmod$residuals
      datas1[[k]]$residuals=residual[order(as.numeric(names(residual)))]
      datas1[[k]]$fitted=datas1[[k]]$pollutant-datas1[[k]]$residuals
      
      # as.matrix(vcovG(plmmod, cluster = "group", inner = "cluster", l = 0))
      # coeftest(plmmod, vcov = Vcx)
      
      boxplot(residuals~orde, data=datas1[[k]])
      p.residual=datas1[[k]][,c('ep_id','orde')]
      p.residual=rbind(p.residual,p.residual)
      if(length(elements[[k]])==0) p.residual$residuals=c(rep(NA,nrow(datas1[[k]])),modef1[[k]]$residuals)
      else p.residual$residuals=c(modeli1[[k]]$residuals,modef1[[k]]$residuals)
      p.residual$models=rep(c('individual lm model','final lm model'),each=nrow(datas1[[k]]))
      p.residual$years=years[k]
      p_residuals=rbind(p_residuals,p.residual)
      #residuals
      ggplot(p.residual,aes(x=orde, y=residuals,color=models))+
        geom_point(size=0.5)+geom_line()+
        geom_hline(yintercept=0,color = "black",linetype=3,size=0.5,alpha=1)+
        facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + mytheme1+
        labs(x = "Hour", y = 'Residuals',color='Model')
      ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],years[k],'residuals.png'),units="in",width=8, height=8, dpi=300)
      
      #fitted
      ggplot(datas1[[k]],aes(x=orde, y=pollutant,group=factor(ep_id)))+
        geom_point(size=0.5)+geom_line()+
        geom_line(aes(x=orde, y=fitted,group=factor(ep_id)),color = "red",size=0.4,alpha=1)+
        facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + mytheme1+
        labs(x = "Hour", y = 'Fitted values')
      ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],years[k],'fitted.png'),units="in",width=5, 
             height=ceiling(length(unique(datas1[[k]]$ep_id))/8)*1.4, dpi=300)
      
      out_result=as.data.frame(coeftest(plmmod, vcov = Vcx)[,1:4])
      out_print[[k]]=data.frame(Variables=c(rownames(out_result),'R2','Adj.R2','Number of panels','RMSE'),
                                Estimate=c(paste0(round(out_result$Estimate,2),'(',round(out_result$`Std. Error`,2),')',apply(as.matrix(out_result$`Pr(>|t|)`),1,significance)),
                                           round(summary(modeplm[[k]])$r.squared[1],2),round(summary(modeplm[[k]])$r.squared[2],2),length(unique(datas1[[k]]$ep_id)),
                                           round(sqrt(sum((summary(modeplm[[k]])$residuals)^2)/summary(modeplm[[k]])$df[2]),2)))
      names(out_print[[k]])[2]=as.character(years[k])
      coeffi=data.frame(Variables=variables_all,Estimates=NA,Signifance=NA,Years=years[k],Seasons=seasons[i])
      coeffi$Estimates[match(rownames(out_result),variables_all)]=out_result$Estimate
      coeffi$Signifance[match(rownames(out_result),variables_all)]=apply(as.matrix(out_result$`Pr(>|t|)`),1,significance)
      coeffis=rbind(coeffis,coeffi)
      
      allls=rbind(allls,c(length(unique(datas1[[k]]$ep_id)),summary(modeplm[[k]])$r.squared))
      
      # var_ob=matrix(rep(0,2*(eplen-1)),ncol=2)
      # for(uuu in 1:length(years))
      # {
      #   var_add=data.frame(orde=datas1[[uuu]]$orde,
      #                      pro=as.matrix(datas1[[uuu]][,elemen_final])%*%out_result$Estimate[which(rownames(out_result)%in%elemen_final)])
      #   var_add_ob=as.matrix(var_add%>%group_by(orde)%>%summarise(var=var(pro)))
      #   var_add_ob[,2]=var_add_ob[,2]/length(unique(datas1[[uuu]]$ep_id))
      #   var_ob=var_ob+var_add_ob
      # }
      # varx_add=var_ob[,2]/length(years)^2
      
      var_ob=matrix(rep(0,(eplen-1)*(eplen-1)),ncol=eplen-1)
      {var_ob_compare=matrix(rep(0,(eplen-1)*(eplen-1)),ncol=eplen-1)}
      for(uuu in 1:length(years))
      {
        var_add=data.frame(orde=datas1[[uuu]]$orde,
                           pro=as.matrix(datas1[[uuu]][,elemen_final])%*%out_result$Estimate[which(rownames(out_result)%in%elemen_final)])
        {var_add_compare=data.frame(orde=datas1[[uuu]]$orde,
                                    pro=as.matrix(datas1[[uuu]][,elemen_final])%*%(out_result$Estimate-theta0)[which(rownames(out_result)%in%elemen_final)])
        }
        var_ob=var_ob+var(matrix(var_add$pro,ncol=eplen-1,byrow=T))/length(unique(datas1[[uuu]]$ep_id))
        {var_ob_compare=var_ob_compare+var(matrix(var_add_compare$pro,ncol=eplen-1,byrow=T))/length(unique(datas1[[uuu]]$ep_id))}
      }
      varx_add=var_ob/length(years)^2
      {varx_add_compare=var_ob_compare/length(years)^2}
      
      new_du=dummy_cols(as.data.frame(new),select_columns = c("orde"),remove_first_dummy = TRUE)[,-1]
      names(new_du)[which(names(new_du)%in%paste0('orde_',3:eplen))]=paste0('factor(orde)',3:eplen)
      new_du$intercept=rep(1,nrow(new_du))
      names(new_du)[ncol(new_du)]='(Intercept)'
      
      Vvar=as.matrix(vcovG(plmmod, cluster = "group", inner = "cluster", l = 0))
      AVar_theta=as.matrix(new_du[,colnames(Vvar)])%*%Vvar%*%t(as.matrix(new_du[,colnames(Vvar)]))
      Vadjust=AVar_theta+varx_add
      mean_or_ad[[k]]$adjusted=as.matrix(new_du[,rownames(out_result)])%*%out_result$Estimate
      mean_or_ad[[k]]$se=sqrt(diag(Vadjust))
      mean_or_ad[[k]]$ad.lb=mean_or_ad[[k]]$adjusted-mean_or_ad[[k]]$se*qnorm(1-0.05/2)
      mean_or_ad[[k]]$ad.ub=mean_or_ad[[k]]$adjusted+mean_or_ad[[k]]$se*qnorm(1-0.05/2)
      mean_or_ad[[k]]=rbind(data.frame(orde=1,TEMP=0,PRES=0,DEWP=0,blh=0,HUMI=0,WSPM=0,
                                       uwind=0,vwind=0,uIws=0,vIws=0,Iwu=0,Iwv=0,IIwu=0,IIwv=0,
                                       epfmINws=0,epfpn=0,epfNen=0,epfpen=0,pmf35=0,
                                       epfmINws48=0,epfpn48=0,epfpen48=0,epfNen48=0,
                                       pollutant=0,SE=0,year=years[k],adjusted=0,ad.lb=0,ad.ub=0,se=0),mean_or_ad[[k]])
      
      adjust_ba=rbind(adjust_ba,mean_or_ad[[k]])
      
      {
        #difference between two years at hour 2:7
        comparetest=data.frame(orde=2:eplen,Years=years[k],Seasons=seasons[i],Sites=rnames[u],
                               Estimates=as.matrix(new_du[,rownames(out_result)])%*%(out_result$Estimate-theta0),
                               SE=sqrt(diag(AVar_theta+AVar0+varx_add_compare)))
        compared=rbind(compared,comparetest)
      }
    }
    rownames(ranks)=years; xtable(t(ranks),caption=paste0("Ranks of all covariates in models for ",seasons[i]))
    colnames(test_pv)=as.character(years)
    print(xtable(test_pv,
                 caption=paste0("P-values of Wooldridge's test and Breusch-Pagan test for models of panel data in ",seasons[i]," of ",rnames[u]),
                 digits=c(0,rep(3,length(years)))),
          include.rownames =T)
    #year变这里会改
    # screenreg(list("2013"=modef[[1]],"2014"=modef[[2]],"2015"=modef[[3]],"2016"=modef[[4]],
    #                "2017"=modef[[5]],"2018"=modef[[6]],"2019"=modef[[7]]))
    screenreg(list("2013"=modef1[[1]],"2014"=modef1[[2]],"2015"=modef1[[3]],
                   "2016"=modef1[[4]],"2017"=modef1[[5]],"2018"=modef1[[6]]))
    # screenreg(list("2013"=modeli1[[1]],"2014"=modeli1[[2]],"2015"=modeli1[[3]],
    #                "2016"=modeli1[[4]],"2017"=modeli1[[5]],"2018"=modeli1[[6]]))
    # screenreg(list("2013"=modeplm[[1]],"2014"=modeplm[[2]],"2015"=modeplm[[3]],
    #                "2016"=modeplm[[4]],"2017"=modeplm[[5]],"2018"=modeplm[[6]]))
    # screenreg(list("2013"=modeo[[1]],"2014"=modeo[[2]],"2015"=modeo[[3]],
    #                "2016"=modeo[[4]],"2017"=modeo[[5]],"2018"=modeo[[6]]))
    # 
    # texreg(list("2013"=modeo[[1]],"2014"=modeo[[2]],"2015"=modeo[[3]],
    #             "2016"=modeo[[4]],"2017"=modeo[[5]],"2018"=modeo[[6]]),
    #        caption=paste0("Results of linear models for data in ",seasons[i]," of ",rnames[u]))
    
    texreg(list("2013"=modef1[[1]],"2014"=modef1[[2]],"2015"=modef1[[3]],
                "2016"=modef1[[4]],"2017"=modef1[[5]],"2018"=modef1[[6]]),
           caption=paste0("Results of linear models for data in ",seasons[i]," of ",rnames[u]))
    all_results=left_join(left_join(left_join(left_join(left_join(out_print[[1]],out_print[[2]]),out_print[[3]]),out_print[[4]]),out_print[[5]]),out_print[[6]])
    print(xtable(all_results,
                 caption=paste0("Results of panel data models for ",seasons[i]," of ",rnames[u])),
          include.rownames =F)
    
    ggplot(p_residuals[p_residuals$models=='final lm model',],aes(x = factor(years), y =residuals, fill =factor(orde)))+ 
      geom_boxplot()+labs(x = "Year", y = 'Residuals',fill = "Hours")+scale_fill_npg()+
      #theme(legend.position = c(0.7, 0.2),legend.direction = "horizontal")+
      mytheme1 
    # +stat_summary(fun.y="mean", geom="point", size=5,
    #              position=position_dodge(width=0.75), shape = 8, size = 1, color="white")
    ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'residuals.png'),units="in",width=5, height=4, dpi=300)
    
    ggplot(adjust_ba,aes(x=orde, y=adjusted,group=factor(year)))+
      geom_point(size=0.5,color="blue")+geom_line(color="blue")+
      geom_ribbon(aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
      geom_line(aes(x=orde, y=pollutant),size=0.4,alpha=1)+
      facet_wrap(.~ year,ncol=6, scales = "fixed") + 
      labs(x = "Hour", y = paste0('Difference of ',pollutant_sp))+mytheme_null
    ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'adjustedfacet.png'),units="in",width=8, height=2, dpi=300)
    
    ggplot(adjust_ba,aes(x=orde, y=adjusted,group=factor(year),color=factor(year)))+
      geom_point(size=0.5)+geom_line()+
      geom_ribbon(aes(x =orde, ymin =ad.lb, ymax =ad.ub,fill=factor(year)),linetype=2,alpha = 0.2) +
      labs(x = "Hour", y = paste0('Adjusted Difference of ',pollutant_sp))+mytheme+
      scale_fill_brewer(palette = "RdBu")+scale_color_brewer(palette = "RdBu")
    ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'adjusted.png'),units="in",width=5, height=4, dpi=300)
    
    metero=data.frame()
    for(uuu in 1:length(elemen_final))
    {
      metero_ob=adjust_ba[,c('orde','year',elemen_final[uuu])]
      names(metero_ob)[3]='values'
      metero_ob$type=elemen_final[uuu]
      metero=rbind(metero,metero_ob)
    }
    ggplot(metero[metero$orde>1,],aes(x=orde, y=values,color=factor(year)))+
      geom_point(size=0.5)+geom_line()+
      facet_wrap(.~ type,ncol=4, scales = "free") + 
      labs(x = "Hour", y = paste0('The Average Change of Meteorological Variables'))+mytheme
    ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'meteorological.png'),units="in",width=5, height=4, dpi=300)
    
    adjust_ba$season=seasons[i]
    adjust_bas=rbind(adjust_bas,adjust_ba)
  }
  ggplot(adjust_bas,aes(x=orde, y=adjusted))+
    geom_point(size=0.5,color="blue")+geom_line(color="blue")+
    geom_ribbon(aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
    geom_line(aes(x=orde, y=pollutant),size=0.4,alpha=1)+
    facet_grid(season~year,scales = "fixed")+
    labs(x = "Hour", y = paste0('Difference of ',pollutant_sp))+mytheme
  ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],'adjustedfacet.png'),units="in",width=8, height=8, dpi=300)
  
  coeffis$Name=apply(as.matrix(coeffis$Variables),1,convert_variables)
  #-----------------------------------
  coeffis$Name=factor(coeffis$Name,levels=c("D(DEWP)","D(LogBLH)","D(LogHUMI)","D(PRES)","D(TEMP)","D(WS)",
                                            "D(WSU)","D(WSV)","D(IWU)","D(IWV)","MCNWS","SNWS",
                                            "Hour3-Hour2","Hour4-Hour2","Hour5-Hour2","Hour6-Hour2","Hour7-Hour2"))
  
  coeffis$shape=rep(NA,length(coeffis$Signifance))
  coeffis$shape[!is.na(coeffis$Signifance)&coeffis$Signifance==' ']='non-significant'
  coeffis$shape[!is.na(coeffis$Signifance)&!coeffis$Signifance==' ']='significant'
  
  ggplot(coeffis[!is.na(coeffis$Estimates)&coeffis$Variables!='(Intercept)',],aes(x=Years, y=Estimates,group=Seasons,color=factor(Seasons)))+
    geom_point(aes(shape=factor(shape)),size=1.2)+geom_line(size=0.6)+
    #geom_text(aes(x=Years,y=Estimates,label=Signifance),color='black',vjust=0)+
    geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
    facet_wrap(~Name,ncol=6,scales = "free_y") + 
    labs(x = "Year", y ='Estimates of Coefficients',color='Seasons')+mytheme_year+
    theme(legend.position="top")
  ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],'coefficients.png'),units="in",width=10, height=5, dpi=300)
  #----------------------------------------
  select_num=data.frame(Variables=c('TEMP','PRES','DEWP','blh','HUMI','WSPM',
                                    'uwind','vwind','Iwu','Iwv',
                                    'epfmINws','epfNen',paste0('orde_',3:eplen)))
  for(yyy in 1:length(seasons))
  {select_num=left_join(select_num,data.frame(Variables=names(elemen_season[[yyy]]),count=as.vector(elemen_season[[yyy]])))
  names(select_num)[ncol(select_num)]=seasons[yyy]}
  print(xtable(select_num,
               caption=paste0("The number of variables selected in six models for different seasons"," in ",rnames[u])),
        include.rownames =F)
  
  adjust_bas$season=factor(adjust_bas$season,levels=seasons)
  adjust_bas$site=rnames[u]
  ggplot(adjust_bas[adjust_bas$orde==eplen,],aes(x=factor(year),y=adjusted/(eplen-1),fill=season))+
    geom_bar(stat='identity', position='dodge') +
    geom_errorbar(aes(x =factor(year), ymin=ad.lb/(eplen-1), ymax=ad.ub/(eplen-1)), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
    mytheme_year +theme(legend.position="top")+
    scale_fill_brewer(palette = "RdBu")+
    scale_color_brewer(palette = "RdBu")+
    labs(x = 'Year',y =paste0('Growth Rate of ',pollutant_sp,' in Six Hours'), title = paste0('Growth rate of ',pollutant_sp,' in six hours for ',rnames[u]))
  ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],'adjusted.png'),units="in",width=5, height=4, dpi=300)
  
  origin_adjust=rbind(origin_adjust,adjust_bas)
}
#write.csv(as.data.frame(allls),'/Users/zyr/Documents/R2episodelength23.csv')

origin_adjust$sitename=apply(as.matrix(origin_adjust$site),1,convert_sites)
origin_adjust$season=factor(origin_adjust$season,levels = c('spring','summer','autumn','winter'))
origin_adjust$sitename=factor(origin_adjust$sitename,levels = c("Southeast of Beijing",'Northwest of Beijing',"Tangshan","Baoding"))
ggplot(origin_adjust[origin_adjust$orde==eplen,],aes(x=factor(year),y=adjusted/(eplen-1),fill=season))+
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(x =factor(year), ymin=ad.lb/(eplen-1), ymax=ad.ub/(eplen-1)), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
  mytheme_year +
  theme(legend.position="none")+
  scale_fill_brewer(palette = "RdBu")+
  scale_color_brewer(palette = "RdBu")+
  labs(x = 'Year',y =paste0('Growth Rate of ',pollutant_sp,' in Six Hours'), title = paste0('Growth rate of ',pollutant_sp,' in six hours'))
ggsave(paste0(pathsavegp,pollutant_sp,'adjusted.png'),units="in",width=10, height=4.5, dpi=300)

p.compare=compared[compared$orde==eplen,]
p.compare=arrange(p.compare,Sites,Years,Seasons)
p.compare$Year_name=paste0(p.compare$Years,'-2013')

p.compare$sitename=apply(as.matrix(p.compare$Sites),1,convert_sites)
p.compare$sitename=factor(p.compare$sitename,levels = c("Southeast of Beijing",'Northwest of Beijing',"Tangshan","Baoding"))
p.compare$Seasons=factor(p.compare$Seasons,levels = c('spring','summer','autumn','winter'))
ggplot(p.compare,aes(x=Seasons,y=Estimates/(eplen-1),group=factor(Years),color=Seasons))+
  geom_point()+geom_line() +
  geom_errorbar(aes(x =Seasons, ymin=(Estimates-qnorm(1-0.05/2)*SE)/(eplen-1),
                    ymax=(Estimates+qnorm(1-0.05/2)*SE)/(eplen-1)), alpha = 1, width = 0.3)+
  geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
  facet_grid(sitename~Year_name,scales = "free_y")+scale_color_futurama()+
  mytheme_year +theme(legend.position="top")+
  labs(x = 'Year',y =paste0('Difference of Growth Rate of ',pollutant_sp,' in Six Hours'), title = paste0('Growth rate of ',pollutant_sp,' in six hours'))
ggsave(paste0(pathsavegp,pollutant_sp,'differences.png'),units="in",width=8, height=8, dpi=300)

colnames(allls)=c('Number','R2','Adj.R2')
allls=as.data.frame(allls)
allls$Sites=rep(rnames,each=length(seasons)*length(years))
allls$Seasons=rep(rep(seasons,each=length(years)),length(rnames))
allls$Years=rep(years,length(rnames)*length(seasons))

allls$sitename=apply(as.matrix(allls$Sites),1,convert_sites)
allls$sitename=factor(allls$sitename,levels = c("Southeast of Beijing",'Northwest of Beijing',"Tangshan","Baoding"))
allls$Seasons=factor(allls$Seasons,levels = c('spring','summer','autumn','winter'))
ggplot(allls,aes(x=Years,y=R2,group=factor(Seasons)))+
  geom_point()+geom_line() +
  facet_grid(sitename~Seasons,scales = "fixed")+
  mytheme_year +theme(legend.position="top")+
  labs(x = 'Year',y ='R squared', title = paste0('R squared of ',pollutant_sp))
ggsave(paste0(pathsavegp,pollutant_sp,'R2.png'),units="in",width=8, height=8, dpi=300)

hour_try=3
ggplot(origin_adjust[origin_adjust$orde==hour_try+1,],aes(x=factor(year),y=adjusted/hour_try,fill=season))+
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(x =factor(year), ymin=ad.lb/hour_try, ymax=ad.ub/hour_try), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
  mytheme_year +
  theme(legend.position="none")+
  scale_fill_brewer(palette = "RdBu")+
  scale_color_brewer(palette = "RdBu")+
  labs(x = 'Year',y =paste0('Growth Rate of ',pollutant_sp,' in Three Hours'), title = paste0('Growth rate of ',pollutant_sp,' in Three hours'))
ggsave(paste0(pathsavegp,pollutant_sp,'adjustedthree.png'),units="in",width=10, height=4.5, dpi=300)

p.compare_try=compared[compared$orde==hour_try+1,]
p.compare_try=arrange(p.compare_try,Sites,Years,Seasons)
p.compare_try$Year_name=paste0(p.compare_try$Years,'-2013')

p.compare_try$sitename=apply(as.matrix(p.compare_try$Sites),1,convert_sites)
p.compare_try$sitename=factor(p.compare_try$sitename,levels = c("Southeast of Beijing",'Northwest of Beijing',"Tangshan","Baoding"))
p.compare_try$Seasons=factor(p.compare_try$Seasons,levels = c('spring','summer','autumn','winter'))
ggplot(p.compare_try,aes(x=Seasons,y=Estimates/hour_try,group=factor(Years),color=Seasons))+
  geom_point()+geom_line() +
  geom_errorbar(aes(x =Seasons, ymin=(Estimates-qnorm(1-0.05/2)*SE)/hour_try,
                    ymax=(Estimates+qnorm(1-0.05/2)*SE)/hour_try), alpha = 1, width = 0.3)+
  geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
  facet_grid(sitename~Year_name,scales = "free_y")+scale_color_futurama()+
  mytheme_year +theme(legend.position="top")+
  labs(x = 'Year',y =paste0('Difference of Growth Rate of ',pollutant_sp,' in Three Hours'), title = paste0('Growth rate of ',pollutant_sp,' in Three hours'))
ggsave(paste0(pathsavegp,pollutant_sp,'differencesthree.png'),units="in",width=8, height=8, dpi=300)

hour_try=4
ggplot(origin_adjust[origin_adjust$orde==hour_try+1,],aes(x=factor(year),y=adjusted/hour_try,fill=season))+
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(x =factor(year), ymin=ad.lb/hour_try, ymax=ad.ub/hour_try), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
  mytheme_year +
  theme(legend.position="none")+
  scale_fill_brewer(palette = "RdBu")+
  scale_color_brewer(palette = "RdBu")+
  labs(x = 'Year',y =paste0('Growth Rate of ',pollutant_sp,' in Four Hours'), title = paste0('Growth rate of ',pollutant_sp,' in Four hours'))
ggsave(paste0(pathsavegp,pollutant_sp,'adjustedfour.png'),units="in",width=10, height=4.5, dpi=300)

p.compare_try=compared[compared$orde==hour_try+1,]
p.compare_try=arrange(p.compare_try,Sites,Years,Seasons)
p.compare_try$Year_name=paste0(p.compare_try$Years,'-2013')

p.compare_try$sitename=apply(as.matrix(p.compare_try$Sites),1,convert_sites)
p.compare_try$sitename=factor(p.compare_try$sitename,levels = c("Southeast of Beijing",'Northwest of Beijing',"Tangshan","Baoding"))
p.compare_try$Seasons=factor(p.compare_try$Seasons,levels = c('spring','summer','autumn','winter'))
ggplot(p.compare_try,aes(x=Seasons,y=Estimates/hour_try,group=factor(Years),color=Seasons))+
  geom_point()+geom_line() +
  geom_errorbar(aes(x =Seasons, ymin=(Estimates-qnorm(1-0.05/2)*SE)/hour_try,
                    ymax=(Estimates+qnorm(1-0.05/2)*SE)/hour_try), alpha = 1, width = 0.3)+
  geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
  facet_grid(sitename~Year_name,scales = "free_y")+scale_color_futurama()+
  mytheme_year +theme(legend.position="top")+
  labs(x = 'Year',y =paste0('Difference of Growth Rate of ',pollutant_sp,' in Four Hours'), title = paste0('Growth rate of ',pollutant_sp,' in Four hours'))
ggsave(paste0(pathsavegp,pollutant_sp,'differencesfour.png'),units="in",width=8, height=8, dpi=300)

hour_try=5
ggplot(origin_adjust[origin_adjust$orde==hour_try+1,],aes(x=factor(year),y=adjusted/hour_try,fill=season))+
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(x =factor(year), ymin=ad.lb/hour_try, ymax=ad.ub/hour_try), color = "grey40",
                alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
  facet_wrap(.~ sitename,ncol=4, scales = "fixed") +
  mytheme_year +
  theme(legend.position="none")+
  scale_fill_brewer(palette = "RdBu")+
  scale_color_brewer(palette = "RdBu")+
  labs(x = 'Year',y =paste0('Growth Rate of ',pollutant_sp,' in Five Hours'), title = paste0('Growth rate of ',pollutant_sp,' in Five hours'))
ggsave(paste0(pathsavegp,pollutant_sp,'adjustedfive.png'),units="in",width=10, height=4.5, dpi=300)

p.compare_try=compared[compared$orde==hour_try+1,]
p.compare_try=arrange(p.compare_try,Sites,Years,Seasons)
p.compare_try$Year_name=paste0(p.compare_try$Years,'-2013')

p.compare_try$sitename=apply(as.matrix(p.compare_try$Sites),1,convert_sites)
p.compare_try$sitename=factor(p.compare_try$sitename,levels = c("Southeast of Beijing",'Northwest of Beijing',"Tangshan","Baoding"))
p.compare_try$Seasons=factor(p.compare_try$Seasons,levels = c('spring','summer','autumn','winter'))
ggplot(p.compare_try,aes(x=Seasons,y=Estimates/hour_try,group=factor(Years),color=Seasons))+
  geom_point()+geom_line() +
  geom_errorbar(aes(x =Seasons, ymin=(Estimates-qnorm(1-0.05/2)*SE)/hour_try,
                    ymax=(Estimates+qnorm(1-0.05/2)*SE)/hour_try), alpha = 1, width = 0.3)+
  geom_hline(yintercept = 0,color='black',size=0.5,alpha=1,linetype=3)+
  facet_grid(sitename~Year_name,scales = "free_y")+scale_color_futurama()+
  mytheme_year +theme(legend.position="top")+
  labs(x = 'Year',y =paste0('Difference of Growth Rate of ',pollutant_sp,' in Five Hours'), title = paste0('Growth rate of ',pollutant_sp,' in Five hours'))
ggsave(paste0(pathsavegp,pollutant_sp,'differencesfive.png'),units="in",width=8, height=8, dpi=300)
