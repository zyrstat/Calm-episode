#------------- Correlation of wind and PM2.5 -------------------

library(RColorBrewer)
library(corrplot)
library(corrgram)
library(cowplot)
prefix=c('Beijing_','Beijing_','Tangshan_','Baoding_')
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin",
          "Shierzhong","Wuziju","Leidazhan",
          "Huadianerqu","Youyongguan","Jiancezhan")
csnames=matrix(csnames,ncol=3,byrow = T)
winddirection=c("ENE","NE","NNE","N","NNW","NW","WNW","W","WSW","SW","SSW","S","SSE","SE","ESE","E")

# for(j in 1:length(prefix))
# {
#   sites_combined=str_c(csnames[j,], collapse = "")
#   da_cs=read.csv(paste0(path8,prefix[j],sites_combined,'_cleaned.csv'),stringsAsFactors = F)
#   da_cs$season=factor(da_cs$season, levels = 1:4,labels = seasons)
#   for(i in 1:5)
#   {
#     da_cs_wd=da_cs[da_cs$cbwd==winddirection[i],]
#     
#     ggplot(da_cs_wd,aes(x=Iws, y=av_PM2.5,color=season))+
#       geom_point(color='black',size=0.15)+stat_smooth(size=0.7,method="loess",se = F)+
#       facet_wrap(.~ cbwd,ncol=1)+
#       mytheme+theme(legend.position="none")+
#       labs(x = "CWS", y = 'PM2.5')
#     
#     ggsave(paste0(path3,sites_combined,winddirection[i],'.png'),units="in",width=2.5, height=2.5, dpi=300)
#   }
# }
da_css=c()
for(k in 1:length(prefix))
{
  sites_combined=csnames[k,]
  da_s1=read.csv(paste0(path8,prefix[k],sites_combined[1],'_cleaned.csv'),stringsAsFactors = F)
  da_s1$CWDws=apply(da_s1[,added],1,sum)
  da_s2=read.csv(paste0(path8,prefix[k],sites_combined[2],'_cleaned.csv'),stringsAsFactors = F)
  da_s2$CWDws=apply(da_s2[,added],1,sum)
  da_s3=read.csv(paste0(path8,prefix[k],sites_combined[3],'_cleaned.csv'),stringsAsFactors = F)
  da_s3$CWDws=apply(da_s3[,added],1,sum)
  
  for(j in 2013:2018)
  {
    da_cs=c()
    da_cs=rbind(da_cs,da_s1[da_s1$X %in%min(da_s1$X[da_s1$year==j&da_s1$month>=3]):max(da_s1$X[da_s1$year==j+1&da_s1$month<3]),])
    da_cs=rbind(da_cs,da_s2[da_s2$X %in%min(da_s2$X[da_s2$year==j&da_s2$month>=3]):max(da_s2$X[da_s2$year==j+1&da_s2$month<3]),])
    da_cs=rbind(da_cs,da_s3[da_s3$X %in%min(da_s3$X[da_s3$year==j&da_s3$month>=3]):max(da_s3$X[da_s3$year==j+1&da_s3$month<3]),])
    da_cs$season_year=j
    da_cs$sites=str_c(csnames[k,], collapse = "")
    da_css=rbind(da_css,da_cs)
  }
}

for(k in 1:length(prefix))
{
  for(j in 2013:2018)
  {
    da_cs=da_css[da_css$sites==str_c(csnames[k,], collapse = "")&da_css$season_year==j,]
    da_cs$season=factor(da_cs$season, levels = 1:4,labels = seasons)
    # for(i in 1:length(winddirection))
    # {
    #   if(na.omit(any(da_cs$wd==winddirection[i])))
    #   {
    #     da_cs_wd=da_cs[da_cs$wd==winddirection[i],]
    # 
    #     ggplot(da_cs_wd,aes(x=CWDws, y=av_PM2.5,color=season))+
    #       geom_point(color='black',size=0.15)+stat_smooth(size=0.7,method="loess",se = F)+
    #       facet_wrap(.~ wd,ncol=1)+
    #       mytheme+theme(legend.position="none")+
    #       xlim(0,max(da_css$CWDws[da_css$season_year==j]))+
    #       ylim(0,max(da_css$av_PM2.5[da_css$season_year==j]))+
    #       labs(x = "CWS", y = 'PM2.5')
    # 
    #     ggsave(paste0(path3,str_c(csnames[k,], collapse = ""),j,winddirection[i],'.png'),units="in",width=2.5, height=2.5, dpi=300)
    #   }
    # }
    
    da_cs$cbwd=factor(da_cs$cbwd,levels=c("NW","NE","SW","SE","CV"))
    
    ggplot(da_cs,aes(x=Iws, y=av_PM2.5,color=season))+
      geom_point(color='black',size=0.15)+stat_smooth(size=0.7,method="loess",se = F)+
      facet_wrap(.~ cbwd,ncol=length(unique(da_cs$cbwd)))+
      mytheme+theme(legend.position="none")+
      # xlim(0,max(c(da_s1$Iws,da_s2$Iws,da_s3$Iws)))+
      # ylim(0,max(c(da_s1$PM2.5,da_s2$PM2.5,da_s3$PM2.5)))+
      xlim(0,max(da_css$Iws[da_css$season_year==j]))+
      ylim(0,max(da_css$av_PM2.5[da_css$season_year==j]))+
      labs(x = "CWS", y = 'PM2.5')
    
    ggsave(paste0(path3,str_c(csnames[k,], collapse = ""),j,'cbwd.png'),units="in",width=7.3, height=1.5, dpi=300)
    
    da_cs$wd=factor(da_cs$wd,levels=winddirection)
    ggplot(da_cs[!is.na(da_cs$wd)&da_cs$wd!='CV',],aes(x=CWDws, y=av_PM2.5,color=season))+
      geom_point(color='black',size=0.15)+stat_smooth(size=0.7,method="loess",se = F)+
      facet_wrap(.~ wd,nrow=length(unique(da_cs$wd)))+
      xlim(0,max(da_css$CWDws[da_css$season_year==j]))+
      ylim(0,max(da_css$av_PM2.5[da_css$season_year==j]))+
      mytheme+theme(legend.position="none")+
      labs(x = "CWS", y = 'PM2.5')
    
    ggsave(paste0(path3,str_c(csnames[k,], collapse = ""),j,'wd.png'),units="in",width=3.5, height=25, dpi=300)
    
  }
}  
