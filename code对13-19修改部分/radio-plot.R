# this code is to plot the radio plot

# package
library(ggplot2)
library(ggthemes)
library(ggExtra)
library(grid)
library(gridExtra)
library(lattice)
library(latex2exp)
# my defined color
pptblue<-rgb(r=16,g=77,b=96,maxColorValue = 255)
pptgrey<-rgb(r=107,g=107,b=107,maxColorValue = 255)
pptred<-rgb(r=233,g=77,b=96,maxColorValue = 255)
ppttea<-rgb(r=198,g=96,b=52,maxColorValue = 255)
pptdark<-rgb(r=69,g=87,b=101,maxColorValue = 255)
pptbg<-rgb(r=242,g=242,b=242,maxColorValue = 255)

# select the color for each wind
mycolor <- c(rgb(red = 0, green = 191, blue = 255, maxColorValue = 255), # blue for NE
             rgb(red = 255, green = 99, blue = 71, maxColorValue = 255),# red for CV
             rgb(red = 238, green = 180, blue = 34, maxColorValue = 255), # yellow for SE
             rgb(red = 186, green = 85, blue = 211, maxColorValue = 255),# purple for SW
             rgb(red = 0, green = 205, blue = 0, maxColorValue = 255))#green for NW

mytheme_radio=theme_bw()+
  theme(legend.position = 'none', # legend of position
        #text=element_text(family='A'), # type of charachter
        panel.background=element_rect(pptbg), # background color of the canvas
        plot.background=element_rect(pptbg), # background color of the plot
        plot.title = element_text(hjust=0.5,size=16,face = 'bold',vjust=0.5), #location of titl
        plot.caption=element_text(size = 14, hjust=0.5),# addjust the position of caption
        plot.margin = margin(t = 0.5, b  = 0.5, r = 0.5, l = 0.5, unit = "cm"),
        panel.border=element_blank(),# border of the plot
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"), #grid line
        panel.grid.minor=element_blank(), # grid of second level
        legend.title=element_blank(), #title of legend
        legend.text=element_text(size=10,colour='black',face = 'bold'), # text of legend
        legend.background =element_rect(pptbg),#background color of legnet
        axis.text.x = element_text(size=10,colour="black",face = "bold"), # text of the axis
        axis.text.y = element_text(size=12,colour="black",face = "bold"),
        axis.title.x = element_blank(), # no title for each axis
        axis.title.y = element_text(size = 14), # no title for each axis
        strip.text=element_text(size=12,colour="black",face = 'bold'),# text of strip/facet
        strip.background=element_blank(),# color of strip/facet
        strip.placement = "outside", # the place of the strip/facet
        axis.line = element_line(size=0.5, colour = 'black'), #setting for axis, color and size
        panel.spacing=unit(5,'mm')) #size of plot

# wind direction 
winddir <- c("NE","CV","SE","SW","NW")
# city, station list
csnames=c("Dongsi","Tiantan","Nongzhanguan",   
          "Guanyuan","Wanliu","Aotizhongxin",
          "Huadianerqu","Youyongguan","Jiancezhan",
          "Shierzhong","Wuziju","Leidazhan")
csnames=matrix(csnames,ncol=3,byrow = T)
years=2013:2019
seasons=c('spring','summer','autumn','winter')
kinds=c('before','middle','after')
kis=c(-1,0,1)
mark=c('earlier','equal','later')

j=ss=kk=yy=h=1
datplots=c()
for(j in 1:nrow(csnames))
{
  sites=csnames[j,]
  for(ss in 1:length(seasons))
  {
    season=seasons[ss]
    for(kk in 1:length(kinds))
    {
      kind=kinds[kk]
      dat=c()
      for(yy in 1:length(years))
      {
        year=years[yy]
        # read the data 
        dat1 <- read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_',kind,'/',
                                sites[1],year,season,'_ep_',kind,'.csv'),stringsAsFactors = F)
        dat1$ep_num=dat1$ep_id
        dat2 <- read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_',kind,'/',
                                sites[2],year,season,'_ep_',kind,'.csv'),stringsAsFactors = F)
        dat2$ep_num=dat2$ep_id+max(dat1$ep_num)
        dat3 <- read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episodes_',kind,'/',
                                sites[3],year,season,'_ep_',kind,'.csv'),stringsAsFactors = F)
        dat3$ep_num=dat3$ep_id+max(dat2$ep_num)
        dats<- rbind(dat1,dat2,dat3)
        dat= rbind(dat,dats)
        print(paste0(j,': ',ss,': ',kk,': ',yy))
      }
      write.csv(dat,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episode_form_bma/',str_c(sites,collapse=""),season,kind,'.csv'))
      
      if(kind=='before')
      {
        prop=as.data.frame(dat%>%group_by(kind)%>%summarise(count=n()))
        prop$count=round(prop$count/sum(prop$count)*100,1)
        write.csv(prop,file=paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episode_form_bma/',str_c(sites,collapse=""),season,'.csv'))
      }
                
      for(h in 1:length(kis))
        {
          ki=kis[h]
          if(any(dat$kind==ki))
          {
            td=dat[dat$kind==ki,]
            datplot=list()
            # wind
            for(w in 1:length(winddir)) {
              if(any(td[,"cbwd"] == winddir[w]))
              {
                tdwind <- td[which(td[,"cbwd"] == winddir[w]),]
                
                # calculate the percentage
                p <- round(nrow(tdwind)/nrow(td), 3)
                # average wind speed
                sp <- round(mean(tdwind[,"WSPM"],na.rm = TRUE),1)
              }
              else
              {p <- 0
              # average wind speed
              sp <- 0}
              # store
              # wind direction
              datplot[["winddir"]] <- c(datplot[["winddir"]], winddir[w])
              # wind occupying 
              datplot[["occupy"]] <- c(datplot[["occupy"]], p)
              # wind average speed
              datplot[["avespeed"]] <- c(datplot[["avespeed"]], sp)
            }
            # dataframe
            datplot <- data.frame(datplot)
            # reorder the wind direction
            datplot[,"winddir"] <- factor(datplot[,"winddir"], levels = winddir)
            # add label
            # datplot[,"label"] <- paste0(datplot[,"occupy"]*100,"%","\n",datplot[,"avespeed"]," m/s")
            datplot[,"label"] <- paste0(datplot[,"occupy"]*100,"%")
            
            datplot[,"sites"]=str_c(csnames[j,], collapse = "")
            datplot[,"season"]=seasons[ss]
            #datplot[,"year"]=years[yy]
            datplot[,"kind"]=kinds[kk]
            datplot[,"type"]=ki
            
            # add x position
            pos <- 0
            posl <- cumsum(datplot[,"occupy"])
            
            for(w in 1:length(winddir)) {
              pos <- posl[w] - 0.5 * datplot[which(datplot[,"winddir"] == winddir[w]),"occupy"] 
              datplot[which(datplot[,"winddir"] == winddir[w]),"xpos"] <- pos
            }
            
            datplots=rbind(datplots,datplot)
            
            if(kk==1&ki<=0) thecaption=TeX('Four hours before $t_s$')
            else if(kk==1&ki>0) thecaption=TeX('Four hours before $t_{\\omega}$')
            else if(kk==2&ki<0) thecaption=TeX('The period between $t_s$ and $t_{\\omega}$')
            else if(kk==2&ki>0) thecaption=TeX('The period between $t_{\\omega}$ and $t_s$')
            else if(kk==3&ki<0) thecaption=TeX('Four hours after $t_{\\omega}$')
            else if(kk==3&ki>=0) thecaption=TeX('Four hours after $t_s$')
            
            # labels for each wind and paste with wind occupying and average wind speed
            mylabel<-paste0(datplot$winddir,'\n',datplot$avespeed,'m/s') 
            
            ggplot()+
              geom_bar(data = datplot,
                       aes(x = xpos, y = avespeed,
                           color = winddir, fill = winddir, width = occupy), 
                       stat = "identity",
                       position = "identity")+#, width = td$occupy
              # add  x label and break
              scale_color_manual(values = mycolor)+
              scale_fill_manual(values = mycolor)+
              scale_x_continuous(breaks = datplot[,"xpos"], labels = rep('',nrow(datplot)))+
              #scale_x_continuous(breaks = datplot[,"xpos"], labels = mylabel)+
              coord_polar(start=0)+ylim(0,6)+
              geom_label(data = datplot,
                         #aes(x = xpos, y = min(max(avespeed)+0.2,5.5), fill = winddir, label = label),
                         aes(x = xpos, y = avespeed+1.5, fill = winddir, label = label),
                         position = "identity",
                         color = "white",
                         vjust = 1,
                         fontface = "bold",
                         size = 3.5)+
              geom_text(data = datplot,
                        #aes(x = xpos, y = min(max(avespeed)+2,6), label = paste0(winddir,'\n',avespeed," m/s")),
                        aes(x = xpos, y = min(max(avespeed)+5.6,6), label = paste0(winddir,'\n',avespeed," m/s")),
                        position = "identity",
                         #check_overlap = T,
                         vjust = 1,
                         fontface = "bold",
                         size = 4)+
              labs(caption= thecaption, y = "Average Wind Speed (m/s)")+
              labs(title = thecaption, y = "Average Wind Speed (m/s)")+
              mytheme_radio
              
            ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/episode_plot_bma/',str_c(datplot[1,c("sites","season","kind")], collapse = ""),mark[h],'.png'),units="in",width=6.5, height=6.5, dpi=300)
            
          }
        }

    }
  }
}
