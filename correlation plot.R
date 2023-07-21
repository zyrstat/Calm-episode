#------------- Correlation of wind and PM2.5 -------------------

library(RColorBrewer)
library(corrplot)
library(corrgram)
library(cowplot)
library(ggcorrplot)
#correlationplot--> correlationplot_new
prefix=c('Beijing_','Beijing_','Tangshan_','Baoding_')
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin",
          "Shierzhong","Wuziju","Leidazhan",
          "Huadianerqu","Youyongguan","Jiancezhan")
csnames=matrix(csnames,ncol=3,byrow = T)
season=c(1,2,3,4)
seasons=c('spring','summer','autumn','winter')

for(k in 1:length(prefix))
{
  sites_combined=csnames[k,]
  da_s1=read.csv(paste0(path8,prefix[k],sites_combined[1],'_cleaned.csv'),stringsAsFactors = F)
  da_s2=read.csv(paste0(path8,prefix[k],sites_combined[2],'_cleaned.csv'),stringsAsFactors = F)
  da_s3=read.csv(paste0(path8,prefix[k],sites_combined[3],'_cleaned.csv'),stringsAsFactors = F)

  for(j in 2013:2018)
  {
    da_cs=c()
    da_cs=rbind(da_cs,da_s1[da_s1$X %in%min(da_s1$X[da_s1$year==j&da_s1$month>=3]):max(da_s1$X[da_s1$year==j+1&da_s1$month<3]),])
    da_cs=rbind(da_cs,da_s2[da_s2$X %in%min(da_s2$X[da_s2$year==j&da_s2$month>=3]):max(da_s2$X[da_s2$year==j+1&da_s2$month<3]),])
    da_cs=rbind(da_cs,da_s3[da_s3$X %in%min(da_s3$X[da_s3$year==j&da_s3$month>=3]):max(da_s3$X[da_s3$year==j+1&da_s3$month<3]),])

    for(i in season)
    {
      da_cs_sea=da_cs[da_cs$season==i,]
      da_part=da_cs_sea[,c('av_PM2.5','av_SO2','av_NO2','INws','ISws')]
      colnames(da_part)=c('PM2.5','SO2','NO2','CNWS','CSWS')
      corr=cor(da_part[,c('PM2.5','SO2','NO2')],da_part[,c('CNWS','CSWS')],use="complete.obs",method="spearman")
      #p-values of cor test
      p_cors=c()
      for(poll in c('PM2.5','SO2','NO2'))
      {
        p_cor=c()
        for(CWS_type in c('CNWS','CSWS')) 
        {
          p_cor=c(p_cor,cor.test(da_part[,poll],da_part[,CWS_type],method="spearman")$p.value)
          #print(paste(poll,CWS_type))
        }
        p_cors=rbind(p_cors,p_cor)
      }
      
      correlations=data.frame(pollutants=rep(rownames(corr),ncol(corr)),
                              CWS=rep(colnames(corr),each=nrow(corr)),correlation=as.vector(corr),
                              sig=sapply(as.vector(p_cors),significance))
      
      ggplot(correlations,aes(CWS,pollutants))+geom_tile(aes(fill=correlation),colour = "white") + 
        scale_fill_gradient2('Value',low ="#6D9EC1", high = "#E46726", mid = 'white',limits=c(-1,1))+
        geom_text(aes(label = paste0(round(correlation, 2),sig))) +labs(title = paste0(str_to_title(seasons[i])))+
        theme_classic()+
        theme(axis.ticks = element_blank(),axis.line = element_blank(),
              axis.title = element_text(size = 6), 
              text = element_text(face = "bold"),
              strip.text = element_text(size = 6,face = 'bold'),
              strip.background = element_rect(color="black", fill="white", linetype="solid"),
              axis.text.x = element_text(size=8,hjust = 0.5, face = 'bold'),
              axis.text.y = element_text(size=8, face = 'bold'),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              #plot.title = element_blank(),
              plot.title = element_text(hjust=0.5),
              legend.title = element_blank(),
              legend.text = element_text(size=6, face = 'bold'),
              legend.key.width  = unit(.3,"inches"),
              legend.key.height = unit(.3,"inches"),
              panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 
      ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/correlationplot/',str_c(csnames[k,], collapse = ""),j,seasons[i],'correlation.png'),units="in",width=2.8, height=2, dpi=300)

    }
  }
}  

