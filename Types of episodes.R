mytheme_allnull=theme_bw()+theme(axis.title = element_text(size = 10), 
                              text = element_text(face = "bold"),
                              strip.text = element_text(size = 10,face = 'bold'),
                              strip.background = element_rect(color="black", fill="white", linetype="solid"),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_blank(),
                              legend.title = element_blank(),
                              legend.text = element_text(size=10, face = 'bold'),
                              legend.key.width  = unit(.3,"inches"),
                              legend.key.height = unit(.3,"inches"),
                              panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

# #Dongsi2018autumn_episode
# ep[j,1:5]
# begin    en  type   epl   epr   hang[js[j]] t
# 395   407     1    13    29    392
j=4
data_type1=sys[(ep[j,1]-4):(ep[j,2]+1),-(1:4)]
data_type1$date=as.POSIXct(paste0(data_type1$year,'-',data_type1$month,'-',data_type1$day,' ',data_type1$hour,':00'))
ggplot(data_type1,aes(x=date,y=PM2.5))+
  geom_line() +scale_x_datetime(breaks = date_breaks("4 hours"),date_labels ="%Y-%m-%d\n%H:%M")+
  geom_hline(yintercept=be_PM2.5,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_vline(xintercept=data_type1$date[data_type1$id==392],color='blue',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type1$date[data_type1$id==395],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type1$date[data_type1$id==396],color='#00CC33',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type1$date[data_type1$id==407],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  # annotate(geom = "curve", x =data_type1$date[data_type1$id==392], y = 1, xend =data_type1$date[data_type1$id==393], yend =1,
  #          curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "segment", x =data_type1$date[data_type1$id==392], y = 1, xend =data_type1$date[data_type1$id==393], yend =1,
           arrow = arrow(length = unit(2, "mm")),color='blue') +
  annotate(geom = "text", x = data_type1$date[data_type1$id==393], y =1, label = "the possible starting point", hjust = "left")+
  annotate(geom = "segment", x =data_type1$date[data_type1$id==395], y = 30, xend =data_type1$date[data_type1$id==396], yend =30,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type1$date[data_type1$id==396], y =30, label = "the starting point of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type1$date[data_type1$id==407], y = 33, xend =data_type1$date[data_type1$id==406], yend =33,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type1$date[data_type1$id==401], y =33, label = "the end of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type1$date[data_type1$id==396], y = 25, xend =data_type1$date[data_type1$id==397], yend =25,
           arrow = arrow(length = unit(2, "mm")),color='#00CC33') +
  annotate(geom = "text", x = data_type1$date[data_type1$id==397], y =25, label = "the end of the strong cleaning", hjust = "left")+
  scale_y_continuous(breaks = sort(c(seq(floor(min(data_type1$PM2.5)),ceiling(max(data_type1$PM2.5)),length.out=5),be_PM2.5)))+
  labs(x='Date',y='PM2.5')+
  mytheme
ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/PM25type1.png'),units="in",width=7, height=4, dpi=300)

datawind_type1=rbind(data_type1[,c("X","year","month","day","hour","season","PM2.5","PM10","SO2","CO","NO2","O3",
                             "wd","cbwd","WSPM","Iws","id" ,"date")],
                     data_type1[,c("X","year","month","day","hour","season","PM2.5","PM10","SO2","CO","NO2","O3",
                                   "wd","cbwd","WSPM","Iws","id" ,"date")])
datawind_type1$CNSWS=c(data_type1$INws,data_type1$ISws)
datawind_type1$kinds=rep(c('CNWS','CSWS'),each=nrow(data_type1))
datawind_type1$kinds=factor(datawind_type1$kinds,levels = c('CSWS','CNWS'))
ggplot(datawind_type1,aes(x=date,y=CNSWS,group=kinds,color=kinds))+
  geom_line() +scale_x_datetime(breaks = date_breaks("4 hours"),date_labels ="%Y-%m-%d\n%H:%M")+
  geom_hline(yintercept=be_INws,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_hline(yintercept=be_ISws,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_vline(xintercept=data_type1$date[data_type1$id==392],color='blue',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type1$date[data_type1$id==395],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type1$date[data_type1$id==396],color='#00CC33',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type1$date[data_type1$id==407],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  # annotate(geom = "curve", x =data_type1$date[data_type1$id==392], y = 1, xend =data_type1$date[data_type1$id==393], yend =1,
  #          curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "segment", x =data_type1$date[data_type1$id==392], y = 4, xend =data_type1$date[data_type1$id==393], yend =4,
           arrow = arrow(length = unit(2, "mm")),color='blue') +
  annotate(geom = "text", x = data_type1$date[data_type1$id==393], y =4, label = "the possible starting point", hjust = "left")+
  annotate(geom = "segment", x =data_type1$date[data_type1$id==395], y = 13, xend =data_type1$date[data_type1$id==396], yend =13,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type1$date[data_type1$id==396], y =13, label = "the starting point of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type1$date[data_type1$id==407], y = 15, xend =data_type1$date[data_type1$id==406], yend =15,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type1$date[data_type1$id==401], y =15, label = "the end of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type1$date[data_type1$id==396], y = 9, xend =data_type1$date[data_type1$id==397], yend =9,
           arrow = arrow(length = unit(2, "mm")),color='#00CC33') +
  annotate(geom = "text", x = data_type1$date[data_type1$id==397], y =9, label = "the end of the strong cleaning", hjust = "left")+
  scale_y_continuous(breaks = sort(c(seq(floor(min(datawind_type1$CNSWS)),ceiling(max(datawind_type1$CNSWS)),length.out=5),be_INws,be_ISws)))+
  scale_color_npg()+labs(x='Date',y='CNWS (CSWS)')+
  #scale_y_reverse()+
  mytheme+theme(legend.position="top")
ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/CNSWStype1.png'),units="in",width=7, height=4, dpi=300)

j=30
# begin    en  type   epl   epr t
# 1928  1936     2     9    42  1923
data_type2=sys[(ep[j,1]-6):(ep[j,2]+1),-(1:4)]
data_type2$date=as.POSIXct(paste0(data_type2$year,'-',data_type2$month,'-',data_type2$day,' ',data_type2$hour,':00'))
ggplot(data_type2,aes(x=date,y=PM2.5))+
  geom_line() +scale_x_datetime(breaks = date_breaks("4 hours"),date_labels ="%Y-%m-%d\n%H:%M")+
  geom_hline(yintercept=be_PM2.5,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_vline(xintercept=data_type2$date[data_type2$id==1923],color='blue',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type2$date[data_type2$id==1928],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type2$date[data_type2$id==1927],color='#00CC33',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type2$date[data_type2$id==1936],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  annotate(geom = "segment", x =data_type2$date[data_type2$id==1923], y = 1, xend =data_type2$date[data_type2$id==1924], yend =1,
           arrow = arrow(length = unit(2, "mm")),color='blue') +
  annotate(geom = "text", x = data_type2$date[data_type2$id==1924], y =1, label = "the possible starting point", hjust = "left")+
  annotate(geom = "segment", x =data_type2$date[data_type2$id==1928], y = 13, xend =data_type2$date[data_type2$id==1929], yend =13,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type2$date[data_type2$id==1929], y =13, label = "the starting point of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type2$date[data_type2$id==1936], y = 21, xend =data_type2$date[data_type2$id==1935], yend =21,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type2$date[data_type2$id==1931], y =21, label = "the end of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type2$date[data_type2$id==1927], y = 7, xend =data_type2$date[data_type2$id==1928], yend =7,
           arrow = arrow(length = unit(2, "mm")),color='#00CC33') +
  annotate(geom = "text", x = data_type2$date[data_type2$id==1928], y =7, label = "the end of the strong cleaning", hjust = "left")+
  scale_y_continuous(breaks = sort(c(seq(floor(min(data_type2$PM2.5)),ceiling(max(data_type2$PM2.5)),length.out=5),be_PM2.5)))+
  labs(x='Date',y='PM2.5')+
  mytheme
ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/PM25type2.png'),units="in",width=7, height=4, dpi=300)

datawind_type2=rbind(data_type2[,c("X","year","month","day","hour","season","PM2.5","PM10","SO2","CO","NO2","O3",
                                   "wd","cbwd","WSPM","Iws","id" ,"date")],
                     data_type2[,c("X","year","month","day","hour","season","PM2.5","PM10","SO2","CO","NO2","O3",
                                   "wd","cbwd","WSPM","Iws","id" ,"date")])
datawind_type2$CNSWS=c(data_type2$INws,data_type2$ISws)
datawind_type2$kinds=rep(c('CNWS','CSWS'),each=nrow(data_type2))
datawind_type2$kinds=factor(datawind_type2$kinds,levels = c('CSWS','CNWS'))
ggplot(datawind_type2,aes(x=date,y=CNSWS,group=kinds,color=kinds))+
  geom_line() +scale_x_datetime(breaks = date_breaks("4 hours"),date_labels ="%Y-%m-%d\n%H:%M")+
  geom_hline(yintercept=be_INws,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_hline(yintercept=be_ISws,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_vline(xintercept=data_type2$date[data_type2$id==1923],color='blue',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type2$date[data_type2$id==1928],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type2$date[data_type2$id==1927],color='#00CC33',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type2$date[data_type2$id==1936],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  annotate(geom = "segment", x =data_type2$date[data_type2$id==1923], y = 7.5, xend =data_type2$date[data_type2$id==1924], yend =7.5,
           arrow = arrow(length = unit(2, "mm")),color='blue') +
  annotate(geom = "text", x = data_type2$date[data_type2$id==1924], y =7.5, label = "the possible starting point", hjust = "left")+
  annotate(geom = "segment", x =data_type2$date[data_type2$id==1928], y = 15, xend =data_type2$date[data_type2$id==1929], yend =15,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type2$date[data_type2$id==1929], y =15, label = "the starting point of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type2$date[data_type2$id==1936], y = 17, xend =data_type2$date[data_type2$id==1935], yend =17,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type2$date[data_type2$id==1931], y =17, label = "the end of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type2$date[data_type2$id==1927], y = 13, xend =data_type2$date[data_type2$id==1928], yend =13,
           arrow = arrow(length = unit(2, "mm")),color='#00CC33') +
  annotate(geom = "text", x = data_type2$date[data_type2$id==1928], y =13, label = "the end of the strong cleaning", hjust = "left")+
  scale_y_continuous(breaks = sort(c(seq(floor(min(datawind_type2$CNSWS)),ceiling(max(datawind_type2$CNSWS)),length.out=4),be_INws,be_ISws)))+
  scale_color_npg()+labs(x='Date',y='CNWS (CSWS)')+
  #scale_y_reverse()+
  mytheme+theme(legend.position="top")
ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/CNSWStype2.png'),units="in",width=7, height=4, dpi=300)

j=23
# begin    en  type   epl   epr  t
# 1568  1582     3    15    56 1566
data_type3=sys[(ep[j,1]-21):(ep[j,2]+1),-(1:4)]
data_type3$date=as.POSIXct(paste0(data_type3$year,'-',data_type3$month,'-',data_type3$day,' ',data_type3$hour,':00'))
ggplot(data_type3,aes(x=date,y=PM2.5))+
  geom_line() +scale_x_datetime(breaks = date_breaks("8 hours"),date_labels ="%Y-%m-%d\n%H:%M")+
  geom_hline(yintercept=be_PM2.5,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_vline(xintercept=data_type3$date[data_type3$id==1566],color='blue',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type3$date[data_type3$id==1568],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type3$date[data_type3$id==1548],color='#00CC33',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type3$date[data_type3$id==1582],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  annotate(geom = "segment", x =data_type3$date[data_type3$id==1566], y = 1, xend =data_type3$date[data_type3$id==1567], yend =1,
           arrow = arrow(length = unit(2, "mm")),color='blue') +
  annotate(geom = "text", x = data_type3$date[data_type3$id==1567], y =1, label = "the possible starting point", hjust = "left")+
  annotate(geom = "segment", x =data_type3$date[data_type3$id==1568], y = 13, xend =data_type3$date[data_type3$id==1569], yend =13,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type3$date[data_type3$id==1569], y =13, label = "the starting point of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type3$date[data_type3$id==1582], y = 21, xend =data_type3$date[data_type3$id==1581], yend =21,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type3$date[data_type3$id==1571], y =21, label = "the end of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type3$date[data_type3$id==1548], y = 5, xend =data_type3$date[data_type3$id==1549], yend =5,
           arrow = arrow(length = unit(2, "mm")),color='#00CC33') +
  annotate(geom = "text", x = data_type3$date[data_type3$id==1549], y =5, label = "the end of the strong cleaning", hjust = "left")+
  scale_y_continuous(breaks = sort(c(seq(floor(min(data_type3$PM2.5)),ceiling(max(data_type3$PM2.5)),length.out=5),be_PM2.5)))+
  labs(x='Date',y='PM2.5')+
  mytheme
ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/PM25type3.png'),units="in",width=7, height=4, dpi=300)

datawind_type3=rbind(data_type3[,c("X","year","month","day","hour","season","PM2.5","PM10","SO2","CO","NO2","O3",
                                   "wd","cbwd","WSPM","Iws","id" ,"date")],
                     data_type3[,c("X","year","month","day","hour","season","PM2.5","PM10","SO2","CO","NO2","O3",
                                   "wd","cbwd","WSPM","Iws","id" ,"date")])
datawind_type3$CNSWS=c(data_type3$INws,data_type3$ISws)
datawind_type3$kinds=rep(c('CNWS','CSWS'),each=nrow(data_type3))
datawind_type3$kinds=factor(datawind_type3$kinds,levels = c('CSWS','CNWS'))
ggplot(datawind_type3,aes(x=date,y=CNSWS,group=kinds,color=kinds))+
  geom_line() +scale_x_datetime(breaks = date_breaks("8 hours"),date_labels ="%Y-%m-%d\n%H:%M")+
  geom_hline(yintercept=be_INws,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_hline(yintercept=be_ISws,color='#999999',size=0.5,alpha=1,linetype=3)+
  geom_vline(xintercept=data_type3$date[data_type3$id==1566],color='blue',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type3$date[data_type3$id==1568],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type3$date[data_type3$id==1548],color='#00CC33',size=0.5,alpha=1,linetype=2)+
  geom_vline(xintercept=data_type3$date[data_type3$id==1582],color='#FFCC00',size=0.5,alpha=1,linetype=2)+
  annotate(geom = "segment", x =data_type3$date[data_type3$id==1566], y = 9.5, xend =data_type3$date[data_type3$id==1567], yend =9.5,
           arrow = arrow(length = unit(2, "mm")),color='blue') +
  annotate(geom = "text", x = data_type3$date[data_type3$id==1567], y =9.5, label = "the possible starting point", hjust = "left")+
  annotate(geom = "segment", x =data_type3$date[data_type3$id==1568], y = 16, xend =data_type3$date[data_type3$id==1569], yend =16,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type3$date[data_type3$id==1569], y =16, label = "the starting point of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type3$date[data_type3$id==1582], y = 20, xend =data_type3$date[data_type3$id==1581], yend =20,
           arrow = arrow(length = unit(2, "mm")),color='#FFCC00') +
  annotate(geom = "text", x = data_type3$date[data_type3$id==1571], y =20, label = "the end of the episode", hjust = "left")+
  annotate(geom = "segment", x =data_type3$date[data_type3$id==1548], y = 7, xend =data_type3$date[data_type3$id==1549], yend =7,
           arrow = arrow(length = unit(2, "mm")),color='#00CC33') +
  annotate(geom = "text", x = data_type3$date[data_type3$id==1549], y =7, label = "the end of the strong cleaning", hjust = "left")+
  scale_y_continuous(breaks = sort(c(0.0,5.5,16.5,22.0,be_INws,be_ISws)))+
  scale_color_npg()+labs(x='Date',y='CNWS (CSWS)')+
  #scale_y_reverse()+
  mytheme+theme(legend.position="top")
ggsave(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/plots/CNSWStype3.png'),units="in",width=7, height=4, dpi=300)

