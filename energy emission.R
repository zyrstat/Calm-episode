mytheme_emission=theme_bw()+theme(axis.title = element_text(size = 14), 
                                  text = element_text(face = "bold"),
                                  strip.text = element_text(size = 10,face = 'bold'),
                                  strip.background = element_rect(color="black", fill="white", linetype="solid"),
                                  #axis.title.x = element_blank(),
                                  #axis.title.y = element_blank(),
                                  axis.text.x = element_text(size=14,hjust = 0.5, face = 'bold'),
                                  axis.text.y = element_text(size=14, face = 'bold'),
                                  plot.title = element_text(size=15, face = 'bold', hjust = 0.5, vjust = 0.5),
                                  legend.title = element_blank(),
                                  legend.text = element_text(size=10, face = 'bold'),
                                  legend.key.width  = unit(.15,"inches"),
                                  legend.key.height = unit(.15,"inches"),
                                  panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

mytheme_emission1=theme_bw()+theme(axis.title = element_text(size = 14), 
                                  text = element_text(face = "bold"),
                                  strip.text = element_text(size = 10,face = 'bold'),
                                  strip.background = element_rect(color="black", fill="white", linetype="solid"),
                                  #axis.title.x = element_blank(),
                                  #axis.title.y = element_blank(),
                                  axis.text.x = element_text(size=14,hjust = 0.5, face = 'bold'),
                                  axis.text.y = element_text(size=14, face = 'bold'),
                                  plot.title = element_blank(),
                                  legend.title = element_blank(),
                                  legend.text = element_text(size=10, face = 'bold'),
                                  legend.key.width  = unit(.15,"inches"),
                                  legend.key.height = unit(.15,"inches"),
                                  panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

df<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bj3chan.csv", stringsAsFactors = FALSE)
df$group=factor(df$group,levels=c('Primary industry','Secondary industry','Tertiary industry',
                                  'Living consumption'))
p1=ggplot(data=df,aes(x=factor(year),y=Consumption,fill=group))+
  geom_bar(position="dodge",stat= 'identity',width = 0.9)+
  labs(x = 'Year',y ='Energy Consumption (million tons of SCE)', title = '(a)Total Energy consumption')+
  mytheme_emission+theme(legend.position = 'bottom')
#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bj3chan.png',units="in",width=6.6,height = 5.2,dpi=300)

####能源构成
df<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bjenergy.csv", stringsAsFactors = FALSE) 
df$lev=0
df$lev[df$Energy.composition=='Other energy']=1
df$lev[df$Energy.composition=='Natural gas']=2
df$lev[df$Energy.composition=='Coal']=3
df$lev[df$Energy.composition=='Electricity']=4
df$lev[df$Energy.composition=='Petroleum']=5
df=arrange(df,year,lev)
library(plyr)
df=ddply(df,"year",transform, label_ypos=cumsum(ratio))
df$Energy.composition=factor(df$Energy.composition,levels=c('Petroleum','Electricity','Coal','Natural gas','Other energy'))
p2=ggplot(data=df,aes(x=factor(year),y=ratio,fill=factor(Energy.composition)))+
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=ratio), vjust=1.3, color="black", size=4)+scale_fill_simpsons()+
  labs(x = 'Year',y ='Percentage (%)',title = '(b) Composition of energy consumption')+mytheme_emission+
  theme(legend.position = 'bottom')
#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bjenergy.png',units="in",width=6.5,height = 5.2,dpi=300)

#####污染物排放
d<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bjNO.csv", stringsAsFactors = FALSE) 

p3=ggplot(d,aes(x=year,y=Emissions,color=type))+geom_line()+geom_point(size=1)+
  labs(x = 'Year',y ='Emission (ton)', title = expression(bold('(c)'~NO[x]~'emissions')))+mytheme_emission+
  theme(legend.position = 'bottom')
#theme(legend.position = c(0.8,0.9))
#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bjNOx.png',units="in",width=6.5,height = 5.2,dpi=300)

d<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bjso2.csv", stringsAsFactors = FALSE)  
p4=ggplot(d,aes(x=year,y=Emissions,color=type))+geom_line() +geom_point()+
  labs(x = 'Year',y ='Emission (ton)', title = expression(bold('(d)'~SO[2]~'emissions')))+mytheme_emission+
  theme(legend.position = 'bottom')
#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bjso2.png',units="in",width=6.5,height = 5.2,dpi=300)
ggarrange(p1,p2,p3,p4,nrow=2,ncol=2)
ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/BJ.png',units="in",width=11,height = 10.5,dpi=300)

df<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bdheavy.csv", stringsAsFactors = FALSE)
p1=ggplot(df,aes(x=year,y=output,fill=factor(year)))+
  geom_bar(position="dodge",stat= 'identity')+
  facet_wrap(.~ Heavy.industrial.products,ncol=5, scales = "fixed") +scale_fill_npg()+
  labs(x = 'Year',y ='Output', title = '(a) Output of main industrial products')+
  mytheme_emission+theme(legend.position = 'bottom')
#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bdheavy.png',units="in",width=12,height = 4.5,dpi=300)

df<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bdcoal.csv", stringsAsFactors = FALSE) 
df$type=factor(df$type,levels=c('Primary industry','Secondary industry','Tertiary industry',
                                  'Domestic use'))
p2=ggplot(data=df,aes(x=factor(year),y=consumption,fill=type))+
  geom_bar(position="dodge",stat= 'identity',width = 0.9)+
  labs(x = 'Year',y ='Consumption (million tons of SCE)',title = '(b) Coal consumption')+
  mytheme_emission+theme(legend.position = 'bottom')
#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bdcoal.png',units="in",width=6.5,height = 5,dpi=300)
ggarrange(p1,p2,nrow=2,ncol=1)
ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/BD.png',units="in",width=12,height = 11,dpi=300)

# d<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bjyan.csv", stringsAsFactors = FALSE)  
# ggplot(d,aes(x=year,y=Emissions,color=type))+geom_line()+geom_point()+
#   labs(x = 'Year',y ='Emission (ton)',title = 'Smoke and dust emissions in Beijing')+mytheme_emission+
#   theme(legend.position = 'bottom')
# 
# ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/bjyan.png',units="in",width=6.5,height = 5.2,dpi=300)

#####重工业产品
df<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/tsheavy.csv", stringsAsFactors = FALSE)
df$Heavy.industrial.products=factor(df$Heavy.industrial.products,
                                    levels=c("Crude oil","Coke","Cement","Crude steel","Pig iron","Rolled steel"))
p1=ggplot(df,aes(x=factor(year),y=output,fill=Heavy.industrial.products))+scale_fill_npg()+
  labs(x = 'Year',y ='Output (million tons)', title = '(a) Output of main industrial products in Tangshan')+
  geom_bar(position="dodge",stat= 'identity',width = 0.9)+mytheme_emission+
  theme(legend.position = 'bottom')+guides(fill = guide_legend(nrow = 1))
#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/tsheavy.png',units="in",width=6.5,height = 5.2,dpi=300)

d<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/tsso2.csv", stringsAsFactors = FALSE) 
p2=ggplot(d,aes(x=year,y=Emissions,color=type))+geom_line()+geom_point()+
  labs(x = 'Year',y ='Emission (ton)', title = expression(bold('(b)'~SO[2]~'emissions')))+mytheme_emission+
  theme(legend.position = 'bottom')

#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/tsso2.png',units="in",width=6.5,height = 5.2,dpi=300)

d<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/tsNO.csv", stringsAsFactors = FALSE)  ###读取数据
p3=ggplot(d,aes(x=year,y=Emissions,color=type))+geom_line()+geom_point()+
  labs(x = 'Year',y ='Emission (ton)', title = expression(bold('(c)'~NO[x]~'emissions')))+mytheme_emission+
  theme(legend.position = 'bottom')
#ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/tsNO.png',units="in",width=6.5,height = 5.2,dpi=300)
ggarrange(p1,ggarrange(p2,p3,nrow=1,ncol=2),nrow=2,ncol=1)
ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/TS.png',units="in",width=7.5,height =7.5,dpi=300)

###机动车保有量
d<- read.csv("/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/vehicle.csv", stringsAsFactors = FALSE)  
ggplot(d,aes(x=year,y= Civil.vehicle.ownership,color=city))+
  geom_line()+geom_point()+labs(x = 'Year',y ='Number of civil motor vehicles (million units)',
                                title = 'The number of Civil motor vehicles in three cities')+mytheme_emission1+
  theme(legend.position = 'bottom')
ggsave('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/Emissions/vehicle.png',units="in",width=5.4,height = 5.2,dpi=300)

