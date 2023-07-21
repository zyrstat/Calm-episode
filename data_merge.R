prefix='Beijing_'                              #'Tangshan_'                           #'Baoding_'
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin")  #c("Shierzhong","Wuziju","Leidazhan")  #c("Huadianerqu","Youyongguan","Jiancezhan")
csnames=matrix(csnames,ncol=3,byrow = T)
comb <- function(x,path5)
{
  da1=read.csv(paste0(path5,prefix,x[1],'_cleaned.csv'))
  da2=read.csv(paste0(path5,prefix,x[2],'_cleaned.csv'))
  da3=read.csv(paste0(path5,prefix,x[3],'_cleaned.csv'))
  da123=rbind(da1,da2,da3)
  write.csv(da123,file=paste0(path5,prefix,x[1],x[2],x[3],
                              '_cleaned.csv') )
}
apply(csnames,1,comb,path5)
#--------------------------------------------------------
path6="/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/data_y_s_s/"
years=year1:year2
season=c(1,2,3,4)
seasons=c('spring','summer','autumn','winter')
########按季节整理数据

yss <- function(x,years,season,seasons,path5,path6)
{
  da_agw=read.csv(paste0(path5,prefix,x[1],x[2],x[3],'_cleaned.csv'))
  
  for (i in 1:length(years)) 
  {
    for(j in 1:(length(season)-1))
    {
      agw2017chun=filter(da_agw,year==years[i]&season==j)
      write.csv(agw2017chun,file=paste0(path6,x[1],x[2],x[3],years[i],seasons[j],'_cleaned.csv'))
    }
    j=j+1
    a2017dong1=filter(da_agw,year==years[i]&month==12&loca==paste0(prefix,x[1]))
    a2017dong2=filter(da_agw,year==years[i]+1&month==1&loca==paste0(prefix,x[1]))
    a2017dong3=filter(da_agw,year==years[i]+1&month==2&loca==paste0(prefix,x[1]))
    a2017dong=rbind(a2017dong1,a2017dong2,a2017dong3)
    
    g2017dong1=filter(da_agw,year==years[i]&month==12&loca==paste0(prefix,x[2]))
    g2017dong2=filter(da_agw,year==years[i]+1&month==1&loca==paste0(prefix,x[2]))
    g2017dong3=filter(da_agw,year==years[i]+1&month==2&loca==paste0(prefix,x[2]))
    g2017dong=rbind(g2017dong1,g2017dong2,g2017dong3)
    
    w2017dong1=filter(da_agw,year==years[i]&month==12&loca==paste0(prefix,x[3]))
    w2017dong2=filter(da_agw,year==years[i]+1&month==1&loca==paste0(prefix,x[3]))
    w2017dong3=filter(da_agw,year==years[i]+1&month==2&loca==paste0(prefix,x[3]))
    w2017dong=rbind(w2017dong1,w2017dong2,w2017dong3)
    
    agw2017dong=rbind(a2017dong,g2017dong,w2017dong)
    write.csv(agw2017dong,file=paste0(path6,x[1],x[2],x[3],years[i],seasons[j],'_cleaned.csv'))
    
  }
  
}
apply(csnames,1,yss,years,season,seasons,path5,path6)

#------------- Correlation of wind and PM2.5 -------------------

# library(RColorBrewer)
# library(corrplot)
# library(corrgram)
# library(cowplot)
# library(ggcorrplot)
# correlation_plot<- function(sites_combined,prefix)
# {
#   da_cs=read.csv(paste0(path5,prefix,str_c(sites_combined, collapse = ""),'_cleaned.csv'),stringsAsFactors = F)
#   wind_sort=c()
#   wd_polss=c()
#   # pdf(file=paste0(path3,prefix,str_c(sites_combined, collapse = ""),'_correlation.pdf'),height = 12, width = 10,family = 'GB1')
#   # plots=c()
#   for(i in season)
#   {
#     da_cs_sea=da_cs[da_cs$season==i,]
#     # print(corrgram(da_cs_sea[,c('PM2.5','avan')],
#     #                order=T,lower.panel=panel.shade,upper.panel=panel.pts,text.panel=panel.txt,diag.panel=panel.density,
#     #                cor.method="spearman",main=paste0('Correlations of ',str_c(sites_combined, collapse = ""),' in ',seasons[i])))
#     # # ggsave(paste0(path3,prefix,str_c(sites_combined, collapse = ""),'_',seasons[i],'_corrgram.pdf'),units="in",width=8, height=8, dpi=300)
#     
#     # corr=cor(da_cs_sea[,c('PM2.5','uwind','vwind','uIws','vIws','Iwu','Iwv','IIwu','IIwv','INws','ISws','massN','massS','massE','massW')],method="spearman")
#     da_part=da_cs_sea[,c('PM2.5','uwind','vwind','INws','ISws')]
#     colnames(da_part)=c('PM2.5','WSU','WSV','CNWS','CSWS')
#     corr=cor(da_part,method="spearman")
#     # corrplot(corr)
#     # print(corrplot(corr = corr,add=T,type="lower",method="number",col='black',diag=FALSE,tl.pos="n",cl.pos="n",
#     #                main=paste0('Correlations of ',str_c(sites_combined, collapse = ""),' in ',seasons[i])))
#     ggcorrplot(corr, hc.order = F, type = "lower",show.diag=F,lab = TRUE,
#                outline.col = "white",
#                ggtheme = mytheme,
#                colors = c("#6D9EC1", "white", "#E46726"))+labs(title = paste0(str_to_title(seasons[i])))
#     ggsave(paste0(path3,prefix,str_c(sites_combined, collapse = ""),seasons[i],'corrgram.png'),units="in",width=4, height=4, dpi=300)
#     
#     wd_gas=as.data.frame(da_cs_sea%>%
#                            group_by(cbwd) %>%
#                            summarise(PM25.m = mean(PM2.5,na.rm=T),PM10.m=mean(PM10,na.rm=T),SO2.m=mean(SO2,na.rm=T),
#                                      CO.m=mean(CO,na.rm=T),NO2.m = mean(NO2,na.rm=T),O3.m = mean(O3,na.rm=T),
#                                      PM25.sd = sd(PM2.5,na.rm=T),PM10.sd=sd(PM10,na.rm=T),SO2.sd=sd(SO2,na.rm=T),
#                                      CO.sd=sd(CO,na.rm=T),NO2.sd = sd(NO2,na.rm=T),O3.sd = sd(O3,na.rm=T)))
#     wd_gas_count=c()
#     for(k in wd_gas$cbwd)
#     {
#       wd_gas_count=rbind(wd_gas_count,apply(!is.na(da_cs_sea[da_cs_sea$wd==k,c('PM2.5','PM10','SO2','CO','NO2','O3')]),2,sum))
#     }
#     wd_gas_count=as.data.frame(wd_gas_count)
#     wd_pol=data.frame(cbwd=rep(wd_gas$cbwd,6),
#                       pollution=rep(c('PM2.5','PM10','SO2','CO','NO2','O3'),each=nrow(wd_gas)),
#                       mean=c(wd_gas$PM25.m,wd_gas$PM10.m,wd_gas$SO2.m,wd_gas$CO.m,wd_gas$NO2.m,wd_gas$O3.m),
#                       sd=c(wd_gas$PM25.sd,wd_gas$PM10.sd,wd_gas$SO2.sd,wd_gas$CO.sd,wd_gas$NO2.sd,wd_gas$O3.sd),
#                       count=c(wd_gas_count$PM2.5,wd_gas_count$PM10,wd_gas_count$SO2,wd_gas_count$CO,wd_gas_count$NO2,wd_gas_count$O3))
#     wd_wspm=as.data.frame(da_cs_sea%>%
#                             group_by(cbwd) %>%
#                             summarise(WSPM.m = mean(WSPM),WSPM.sd = sd(WSPM),counts=n()))
#     if(any(is.na(wd_wspm))) print(i)
#     wd_pol$cbwd=as.character(wd_pol$cbwd); wd_wspm$cbwd=as.character(wd_wspm$cbwd)
#     wd_pol=left_join(wd_pol,wd_wspm)
#     #wd_pol$pollution=factor(wd_pol$pollution,levels=c('PM10','PM2.5','O3','NO2','SO2','CO'))
#     p_text=paste0(wd_pol$cbwd,' (',round(wd_pol$WSPM.m,2),' m/s)')
#     #wd_pol$wd.text=factor(p_text,levels=p_text[order(wd_pol$mean[wd_pol$pollution=='PM2.5'])])
#     wd_pol$wd.text=p_text
#     wd_pol$season=seasons[i]
#     wind_sort=rbind(wind_sort,c(seasons[i],'PM2.5',wd_pol$cbwd[order(wd_pol$mean[wd_pol$pollution=='PM2.5'])]))
#     wind_sort=rbind(wind_sort,c(seasons[i],'CO',wd_pol$cbwd[order(wd_pol$mean[wd_pol$pollution=='CO'])]))
#     
#     #plots[[i]]= 
#     ggplot(wd_pol,aes(x =pollution, y=mean, fill =wd.text)) +
#       geom_bar(stat="identity", width=0.9, position = "dodge")+
#       geom_errorbar(aes(x =pollution, ymin=mean-sd/sqrt(count), ymax=mean+sd/sqrt(count)), color = "grey40",alpha = 1, stat = "identity",width = 0.9,position = "dodge")+
#       theme_bw()+
#       theme(axis.text.x = element_text(angle=45,hjust = 1))+
#       scale_fill_brewer(palette = "RdBu")+
#       scale_color_brewer(palette = "RdBu")+
#       labs(x="Pollutants",y='Mean',title=paste0('The concentration of pollutants in ',seasons[i]))
#     
#     wd_polss=rbind(wd_polss,wd_pol)
#     write.csv(wd_pol,file=paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/windpollutant/",str_c(sites_combined, collapse = ""),'_',seasons[i],'.csv'))
#   }
#   wd_polss$cbwd=factor(wd_polss$cbwd,levels=c('NW','NE','SW','SE','CV'))
#   wd_polss$season=factor(wd_polss$season,levels=seasons)
#   wd_polss$site=str_c(sites_combined, collapse = "")
#   ggplot(wd_polss[wd_polss$pollution=='PM2.5',],aes(x=cbwd, y=mean, group=season, colour=season)) + 
#     geom_point(size=1) +geom_line() + 
#     #geom_text(aes(label=round(WSPM.m,2)),hjust = 0,check_overlap = TRUE)+
#     xlab("Wind direction") + 
#     ylab("PM2.5") +mytheme+theme(legend.position="none")
#   ggsave(paste0(path3,prefix,str_c(sites_combined, collapse = ""),'winddirection.png'),units="in",width=2.5, height=2.5, dpi=300)
#   
#   ggplot(wd_polss[wd_polss$pollution=='PM2.5',],aes(x=cbwd, y=WSPM.m, group=season, colour=season)) + 
#     geom_point(size=1) +
#     geom_line() + 
#     #geom_text(aes(label=round(WSPM.m,2)),hjust = 0,check_overlap = TRUE)+
#     xlab("Wind direction") + 
#     ylab("Wind speed")+mytheme+theme(legend.position="none")
#   ggsave(paste0(path3,prefix,str_c(sites_combined, collapse = ""),'windspeed.png'),units="in",width=2.5, height=2.5, dpi=300)
#   
#   # print(plot_grid(plotlist = plots,ncol=2))
#   # dev.off()
#   write.csv(wind_sort,file=paste0("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/windpollutant/",str_c(sites_combined, collapse = ""),'_windsort.csv'))
#   # print('PM2.5')
#   # print(sort(table(as.vector(wind_sort[wind_sort[,2]=="PM2.5",c(3:11)])),decreasing = T))
#   # print('CO')
#   # print(sort(table(as.vector(wind_sort[wind_sort[,2]=="CO",c(3:11)])),decreasing = T))
#   return(wd_polss)
# }
# wd_pollus=c()
# wd_pollus=rbind(wd_pollus,apply(matrix(csnames[1,],nrow=1,byrow=T),1,correlation_plot,prefix)[[1]])
# 
# wd_pollution=wd_pollus
# wd_pollution$sitename=apply(as.matrix(wd_pollution$site),1,convert_sites)
# wd_pollution$sitename=factor(wd_pollution$sitename,levels = c('DNT, Beijing','AGW, Beijing','LSW, Tangshan','HJY, Baoding'))
# ggplot(wd_pollution[wd_pollution$pollution=='PM2.5',],aes(x=cbwd, y=mean, group=season, colour=season))+
#   geom_point(size=1) +geom_line() +
#   geom_hline(yintercept = 35,color='black',size=0.5,alpha=1,linetype=3)+
#   facet_wrap(.~ sitename,ncol=4, scales = "fixed")+
#   scale_y_continuous(breaks = sort(c(35,50,100,150,200)))+
#   xlab("Wind Direction") + 
#   ylab("PM2.5")+mytheme+theme(legend.position="none")
# ggsave(paste0(path3,'PM25.png'),units="in",width=7, height=2.8, dpi=300)
# 
# ggplot(wd_pollution[wd_pollution$pollution=='PM2.5',],aes(x=cbwd, y=WSPM.m, group=season, colour=season))+
#   geom_point(size=1) +geom_line() +
#   facet_wrap(.~ sitename,ncol=4, scales = "fixed")+
#   xlab("Wind Direction") + 
#   ylab("Wind Speed")+mytheme+theme(legend.position="none")
# ggsave(paste0(path3,'windspeed.png'),units="in",width=7, height=2.8, dpi=300)

