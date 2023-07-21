install.packages("remotes")
remotes::install_github("badbye/baidumap")
library(baidumap)
library(ggmap)
options(baidumap.key = 'X8acHNtnlKfIcHS9QagCkAuOGBGVlASv')
ggmap(getBaiduMap(location = c(lon=116.354431, lat=39.942333),zoom = 9))
mapbg <- get_map(location = center, 
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

eplen=7
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

path_min <- function(x)
{
  return(min(which(x^2>0)))
}

mytheme=theme_bw()+theme(axis.title = element_text(size = 10), 
                         text = element_text(face = "bold"),
                         strip.text = element_text(size = 10,face = 'bold'),
                         #axis.title.x = element_blank(),
                         #axis.title.y = element_blank(),
                         axis.text.x = element_text(size=10,hjust = 1, face = 'bold'),
                         axis.text.y = element_text(size=10, face = 'bold'),
                         plot.title = element_text(size=15, face = 'bold', hjust = 0.5, vjust = 0.5),
                         #legend.title = element_blank(),
                         legend.text = element_text(size=10, face = 'bold'),
                         legend.key.width  = unit(.3,"inches"),
                         legend.key.height = unit(.3,"inches")) 

set.seed(0)
pathsavegp="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/glsplot_log/"
berange=6:18#range of the beginning of episodes 
years=2013:2018
len_elem=6
pollutant_sp='PM2.5'

if(pollutant_sp=='PM2.5') 
  lower=28
if(pollutant_sp=='PM10') 
  lower=260
if(pollutant_sp=='SO2') 
  lower=28
if(pollutant_sp=='CO') 
  lower=28
if(pollutant_sp=='NO2') 
  lower=28
if(pollutant_sp=='O3') 
  lower=28

ysepn <-paste0(rep(years,length(seasons)),rep(seasons,each=length(years)))
allls=c()

origin_adjust=c()
for(u in 1:length(rnames))
{
  adjust_bas=c()
  for(i in 1:length(seasons))
  {
    #datas=vector("list",length(years))
    datas1=vector("list",length(years))
    
    #modef=vector("list",length(years))
    modef1=vector("list",length(years))
    modeAR1=vector("list",length(years))
    
    new=matrix(rep(0,(eplen-1)*24),nrow=eplen-1)#value for ajustment
    
    mean_or_ad=vector("list",length(years))
    adjust_ba=c()
    #elemen1=c()
    elemen2=c()
    elements=vector("list",length(years))
    ranks=c()
    #pdf(file= paste0(pathsavegp,'gls23',rnames[u],'_',seasons[i],'.pdf'),height = 12,width=12,family = 'GB1')
    for(j in 1:length(years))
    {
      d<- read.csv(paste0(pathmdatasave,rnames[u],ysepn[(i-1)*length(years)+j],'_eptreat.csv'),
                   stringsAsFactors = FALSE)
      d$pollutant=d[,pollutant_sp]
      idna=unique(d$ep_id[is.na(d$pollutant)])
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
      #  
      # step(lm(pollutant~TEMP+PRES+DEWP+blh+HUMI+WSPM+uwind+vwind+uIws+vIws+Iwu+Iwv+IIwu+IIwv+
      #           epfmINws+epfpn+epfNen+epfpen+pmf35+epfmINws48+epfpn48+epfpen48+epfNen48+
      #           factor(orde),dataaa))
      # 
      # dataaa_dummy=dummy_cols(dataaa,select_columns = c("orde"),remove_first_dummy = TRUE)
      # # dataaa_feature=dataaa_dummy[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM',
      # #                 'uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv',
      # #                 'epfmINws','epfpn','epfNen','epfpen','pmf35','epfmINws48','epfpn48',
      # #                 'epfpen48','epfNen48',paste0('orde_',3:eplen))]
      # dataaa_feature=dataaa_dummy[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM',
      #                                'uwind','vwind','Iwu','Iwv',
      #                                'epfmINws','epfNen','pmf35',
      #                                paste0('orde_',3:eplen))]
      # dataaa_group=c(1:(ncol(dataaa_feature)-eplen+2),rep(ncol(dataaa_feature)-eplen+3,eplen-2))
      # dataaa_cv <- cv.gglasso(x=as.matrix(dataaa_feature),y=dataaa_dummy$pollutant,
      #                         group=dataaa_group,loss="ls",
      #                         pred.loss="L2", nfolds=5)
      # 
      # # the coefficients at lambda = lambda.1se
      # coeff1=coef(dataaa_cv$gglasso.fit,s=dataaa_cv$lambda.min)
      # elemen1=c(elemen1,names(coeff1[setdiff(which(coeff1!=0),c(1,(length(coeff1)-eplen+3):length(coeff1))),]))
      # #fit group lasso
      # dataaa_try <- gglasso(x=as.matrix(dataaa_feature),y=dataaa_dummy$pollutant,group=dataaa_group,loss="ls")
      # print(dataaa_try)
      
      dataaa1=dataaa
      (id_delete=unique(dataaa1$ep_id[dataaa1$pollutant<=-lower]))
      if(length(id_delete)>0)
        print(ggplot(dataaa1[which(dataaa1$ep_id%in%id_delete),],aes(x=orde, y=pollutant,group=factor(ep_id)))+
                geom_point(size=0.5)+geom_line()+stat_smooth(method="lm",se = F)+
                facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + theme_bw()+
                labs(x = "Time", y = pollutant_sp,title='Episodes deleted'))
      
      dataaa1=dataaa1[which(!dataaa1$ep_id%in%id_delete),]
      allls=c(allls,length(unique(dataaa1$ep_id)))
      # step(lm(log(lower+pollutant)~TEMP+PRES+DEWP+blh+HUMI+WSPM+uwind+vwind+uIws+vIws+Iwu+Iwv+IIwu+IIwv+
      #           epfmINws+epfpn+epfNen+epfpen+pmf35+epfmINws48+epfpn48+epfpen48+epfNen48+
      #           factor(orde),dataaa1))
      
      dataaa1_dummy=dummy_cols(dataaa1,select_columns = c("orde"),remove_first_dummy = TRUE)
      # dataaa1_feature=dataaa1_dummy[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM',
      #                 'uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv',
      #                 'epfmINws','epfpn','epfNen','epfpen','pmf35','epfmINws48','epfpn48',
      #                 'epfpen48','epfNen48',paste0('orde_',3:eplen))]
      dataaa1_feature=cbind(as.data.frame(scale(dataaa1_dummy[,c('TEMP','PRES','DEWP','blh','HUMI','WSPM',
                                                                 'uwind','vwind','Iwu','Iwv',
                                                                 'epfmINws','epfNen','pmf35')],center = TRUE, scale = TRUE)),
                            dataaa1_dummy[,paste0('orde_',3:eplen)])
      dataaa1_group=c(1:(ncol(dataaa1_feature)-eplen+2),rep(ncol(dataaa1_feature)-eplen+3,eplen-2))
      dataaa1_cv <- cv.gglasso(x=as.matrix(dataaa1_feature),y=log(lower+dataaa1_dummy$pollutant),
                               group=dataaa1_group,loss="ls",
                               pred.loss="L2", nfolds=5)
      
      dataaa1_try <- gglasso(x=as.matrix(dataaa1_feature),y=log(lower+dataaa1_dummy$pollutant),group=dataaa1_group,loss="ls")
      ranks=rbind(ranks,rank(apply(dataaa1_try$beta,1,path_min)))
      # the coefficients at lambda = lambda.1se
      coeff2=coef(dataaa1_cv$gglasso.fit,s=dataaa1_cv$lambda.min)
      elemen2=c(elemen2,names(coeff2[setdiff(which(coeff2!=0),c(1,(length(coeff2)-eplen+3):length(coeff2))),]))
      elements[[j]]=names(coeff2[setdiff(which(coeff2!=0),1),])
      # #Functions rstandard and rstudent give the standardized and Studentized residuals respectively.
      # dataaa$residual=rstandard(lm(pollutant ~ TEMP + DEWP + blh + HUMI + WSPM + vwind + 
      #                                vIws + Iwu + IIwu + IIwv + epfpn + epfpen + factor(orde), 
      #                              data = dataaa))
      # dataaa1$residual=rstandard(lm(log(lower + pollutant) ~ PRES + WSPM + vwind + vIws + 
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
                                       summarise(SE=sd(pollutant)/sqrt(eplen-1))))
      mm$year=rep(years[j],eplen-1)
      mean_or_ad[[j]]=mm
      
      new=new+as.matrix(mm[,c('orde','TEMP','PRES','DEWP','blh','HUMI','WSPM',
                              'uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv',
                              'epfmINws','epfpn','epfNen','epfpen','pmf35','epfmINws48','epfpn48',
                              'epfpen48','epfNen48')])
    }
    #sort(table(elemen1),decreasing = T)
    sort(table(elemen2),decreasing = T)
    new=new/length(years)
    name_rank=names(sort(apply(ranks[,-c((ncol(ranks)-eplen+3):ncol(ranks))],2,sum)))
    elemen_final=intersect(unique(elemen2),name_rank[1:len_elem])
    for(k in 1:length(years))
    {
      # M0=lm(pollutant ~ 1,data=datas[[k]])
      # com <- update(M0, paste("~ .+ factor(orde)+", paste(unique(elemen1),collapse='+')), evaluate = FALSE)
      # com=eval.parent(com)
      # modef[[k]]=com
      
      M1=lm(log(lower+pollutant) ~ 1,data=datas1[[k]])
      com1 <- update(M1, paste("~ .+ factor(orde)+", paste(elemen_final,collapse='+')), evaluate = FALSE)
      com1=eval.parent(com1)
      #linear model
      modef1[[k]]=com1
      
      # #A joint LM test for random effects and serial correlation under normality and homoskedasticity of the idiosyncratic errors
      # pbsytest(log(lower + pollutant) ~ factor(orde) + TEMP + PRES +
      #            DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"),
      #          test = "j")
      # pbsytest(log(lower + pollutant) ~ factor(orde) + TEMP + PRES +
      #                  DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"),
      #                test = "re")
      # pbsytest(log(lower + pollutant) ~ factor(orde) + TEMP + PRES +
      #                  DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"),
      #                test = "ar")
      # #Wooldridge’s test for serial correlation in “short” FE panels
      # pwartest(log(lower + pollutant) ~ factor(orde) + TEMP + PRES +
      #            DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"))
      # #Modified BNF–Durbin–Watson Test for AR(1) disturbances in panel models.
      # pbnftest(log(lower + pollutant) ~ factor(orde) + TEMP + PRES +
      #            DEWP + blh + Iwu + Iwv + epfNen + WSPM, data = datas1[[k]],index=c("ep_id","orde"),model="pooling")
      # #equivalent to lm
      # Model1=panelAR(log(lower + pollutant) ~ factor(orde),data = datas1[[k]],
      #                 autoCorr = "none", panelCorrMethod = "none", panelVar="ep_id", timeVar="orde")
      Model1=panelAR(log(lower + pollutant) ~ factor(orde),data = datas1[[k]],
                     panel.weight = "t-1", rhotype ="breg",autoCorr = "ar1",
                     panelCorrMethod = "none", panelVar="ep_id", timeVar="orde")
      out=eval.parent(update(Model1, paste("~ .+", paste(elemen_final,collapse='+')), evaluate = FALSE))
      
      # out=panelAR(log(lower + pollutant) ~ factor(orde) + TEMP + uwind + 
      #               Iwu + Iwv + blh + HUMI,data = datas1[[k]],
      #             panel.weight = "t-1", rhotype ="breg",autoCorr = "ar1",
      #             panelCorrMethod = "none", panelVar="ep_id", timeVar="orde")
      modeAR1[[k]]=out
      fit=out$fitted.values
      datas1[[k]]$fitted=fit[order(as.numeric(names(fit)))]
      residual=out$residuals
      datas1[[k]]$residuals=residual[order(as.numeric(names(residual)))]
      #all(datas1[[k]]$residuals==log(lower + datas1[[k]]$pollutant)-datas1[[k]]$fitted)
      datas1[[k]]$fitted=exp(datas1[[k]]$fitted)-lower
      datas1[[k]]$residuals=datas1[[k]]$pollutant-datas1[[k]]$fitted
      
      # summary(out)$rho 
      # summary(out)$Sigma # panel covariances
      
      # #fitted
      # ggplot(datas1[[k]],aes(x=orde, y=pollutant,group=factor(ep_id)))+
      #   geom_point(size=0.5)+geom_line()+
      #   geom_line(aes(x=orde, y=fitted,group=factor(ep_id)),color = "red",size=0.4,alpha=1)+
      #   facet_wrap(.~ ep_id,ncol=8, scales = "fixed") + theme_bw()+
      #   labs(x = "Time", y = 'Fitted values')
      adjusted=predict(out,newdata=as.data.frame(new),se.fit=TRUE, conf.interval=TRUE)$fit
      mean_or_ad[[k]]$adjusted=exp(adjusted$fit)-lower
      mean_or_ad[[k]]$ad.lb=exp(adjusted$lb)-lower
      mean_or_ad[[k]]$ad.ub=exp(adjusted$ub)-lower
      mean_or_ad[[k]]$se=exp(adjusted$fit)*adjusted$se
      mean_or_ad[[k]]=rbind(data.frame(orde=1,TEMP=0,PRES=0,DEWP=0,blh=0,HUMI=0,WSPM=0,
                                       uwind=0,vwind=0,uIws=0,vIws=0,Iwu=0,Iwv=0,IIwu=0,IIwv=0,
                                       epfmINws=0,epfpn=0,epfNen=0,epfpen=0,pmf35=0,
                                       epfmINws48=0,epfpn48=0,epfpen48=0,epfNen48=0,
                                       pollutant=0,SE=0,year=years[k],adjusted=0,ad.lb=0,ad.ub=0,se=0),mean_or_ad[[k]])
      
      adjust_ba=rbind(adjust_ba,mean_or_ad[[k]])
    }
    
    #year变这里会改
    # screenreg(list("2013"=modef[[1]],"2014"=modef[[2]],"2015"=modef[[3]],"2016"=modef[[4]],
    #                "2017"=modef[[5]],"2018"=modef[[6]],"2019"=modef[[7]]))
    screenreg(list("2013"=modef1[[1]],"2014"=modef1[[2]],"2015"=modef1[[3]],
                   "2016"=modef1[[4]],"2017"=modef1[[5]],"2018"=modef1[[6]]))
    # screenreg(list("2013"=modeAR1[[1]],"2014"=modeAR1[[2]],"2015"=modeAR1[[3]],
    #                "2016"=modeAR1[[4]],"2017"=modeAR1[[5]],"2018"=modeAR1[[6]]))
    # stargazer(modef1[[1]],modef1[[2]],modef1[[3]],modef1[[4]],
    #           modef1[[5]],modef1[[6]],title=paste0("Results of linear AR(1) panel data model for ",seasons[u]," of ",rnames[i]),align=T)
    
    ggplot(adjust_ba,aes(x=orde, y=adjusted,group=factor(year)))+
      geom_point(size=0.5,color="blue")+geom_line(color="blue")+
      geom_ribbon(aes(x =orde, ymin =ad.lb, ymax =ad.ub),linetype=2,fill='blue',alpha = 0.2) +
      geom_line(aes(x=orde, y=pollutant),size=0.4,alpha=1)+
      facet_wrap(.~ year,ncol=3, scales = "fixed") + 
      labs(x = "Time", y = paste0('Difference of ',pollutant_sp))+mytheme
    ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'adjustedfacet.png'),units="in",width=5, height=4, dpi=300)
    
    ggplot(adjust_ba,aes(x=orde, y=adjusted,group=factor(year),color=factor(year)))+
      geom_point(size=0.5)+geom_line()+
      geom_ribbon(aes(x =orde, ymin =ad.lb, ymax =ad.ub,fill=factor(year)),linetype=2,alpha = 0.2) +
      labs(x = "Time", y = paste0('Adjusted difference of ',pollutant_sp))+mytheme+
      scale_fill_brewer(palette = "RdBu")+scale_color_brewer(palette = "RdBu")
    ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],seasons[i],'adjusted.png'),units="in",width=5, height=4, dpi=300)
    
    adjust_ba$season=seasons[i]
    adjust_bas=rbind(adjust_bas,adjust_ba)
  }
  adjust_bas$season=factor(adjust_bas$season,levels=seasons)
  adjust_bas$site=rnames[u]
  ggplot(adjust_bas[adjust_bas$orde==eplen,],aes(x=factor(year),y=adjusted/(eplen-1),fill=season))+
    geom_bar(stat='identity', position='dodge') +
    geom_errorbar(aes(x =factor(year), ymin=ad.lb/(eplen-1), ymax=ad.ub/(eplen-1)), color = "grey40",
                  alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
    mytheme +
    scale_fill_brewer(palette = "RdBu")+
    scale_color_brewer(palette = "RdBu")+
    labs(x = 'Year',y = 'Growth rate of ',pollutant_sp,' in six hours', title = paste0('Growth rate of ',pollutant_sp,' in six hours for ',rnames[u]))
  ggsave(paste0(pathsavegp,pollutant_sp,rnames[u],'adjusted.png'),units="in",width=5, height=4, dpi=300)
  
  origin_adjust=rbind(origin_adjust,adjust_bas)
}
write.csv(as.data.frame(allls),'/Users/zyr/Documents/episodelength23.csv')

