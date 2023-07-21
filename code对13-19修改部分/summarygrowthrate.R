convert_sites <- function(x)
{
  if(x=="DongsiTiantanNongzhanguan")
    return('Beijing SE')
  else if(x=="GuanyuanWanliuAotizhongxin")
    return('Beijing NW')
  else if(x=="ShierzhongWuzijuLeidazhan")
    return('Tangshan')
  else if(x=="HuadianerquYouyongguanJiancezhan")
    return('Baoding')
}
pps=c('PM2.5','NO2','SO2')

tb_summary=c()
tbcompare_summary=c()
for(pp in 1:length(pps))
{
  tb=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/',pps[pp],'adjusted.csv'),stringsAsFactors = F)
  tb_compare=read.csv(paste0('/Users/zyr/Documents/PM2.5/datas/data_BJ_TS_BD/form_adjusted_new/',pps[pp],'adjustedcompared.csv'),stringsAsFactors = F)
  tb_compare$rate.se=0
  tb_compare$rate.se=tb_compare$SE/(tb_compare$orde-1)
  for(i in 1:length(seasons))
  {
    for(u in 1:length(rnames))
    {
      tb_part=tb[tb$site==rnames[u]&tb$season==seasons[i]&tb$orde==eplen,c('adjusted','se')]/(eplen-1)
      tb_part2=tb[tb$site==rnames[u]&tb$season==seasons[i]&tb$orde==hour_try,c('adjusted','se')]/(hour_try-1)
      tb_summary=rbind(tb_summary, c(pps[pp],seasons[i],rnames[u],paste0(round(tb_part2$adjusted,1),'(',round(tb_part2$se,1),'), ',round((tb_part2$adjusted/tb_part2$adjusted[1]-1)*100,1)),
                                     paste0(round(tb_part$adjusted,1),'(',round(tb_part$se,1),'), ',round((tb_part$adjusted/tb_part$adjusted[1]-1)*100,1))))
      #---------------------------------------
      tbcompare_part=tb_compare[tb_compare$Sites==rnames[u]&tb_compare$Seasons==seasons[i]&tb_compare$orde==eplen,]
      tbcompare_part2=tb_compare[tb_compare$Sites==rnames[u]&tb_compare$Seasons==seasons[i]&tb_compare$orde==hour_try,]
      #-----------------
      aver=paste0(-round(mean(tbcompare_part$rate),1),'(',-round(mean(tbcompare_part$rate/abs(tb_part$adjusted[1]))*100,1),'%)')
      aver2=paste0(-round(mean(tbcompare_part2$rate),1),'(',-round(mean(tbcompare_part2$rate/abs(tb_part2$adjusted[1]))*100,1),'%)')
      # #-------------
      # # The change point that is the year when the significant reduction in the growth rate compared to that in 2013 happened 
      # # and after which significant reduction was maintained in subsequent year.
      # nonsig=c(0,which(tbcompare_part$significance==F|tbcompare_part$rate>=0))
      # 
      # if(max(nonsig)+1<length(years)) stay=c(tbcompare_part$Years[max(nonsig)+1],
      #                                        paste0(round(-tbcompare_part$rate[max(nonsig)+1],1),
      #                                               '(',round(tbcompare_part$rate.se[max(nonsig)+1],1),')'))
      # else stay=c('---','---')
      # nonsig2=c(0,which(tbcompare_part2$significance==F|tbcompare_part2$rate>=0))
      # 
      # if(max(nonsig2)+1<length(years)) stay2=c(tbcompare_part2$Years[max(nonsig2)+1],
      #                                          paste0(round(-tbcompare_part2$rate[max(nonsig2)+1],1),
      #                                                 '(',round(tbcompare_part2$rate.se[max(nonsig2)+1],1),')'))
      # else stay2=c('---','---')
      
      #------------
      # The change point that is the year when the significant reduction in the growth rate compared to that in 2013 happened 
      # and after which significant increase in the growth rate did not happen in subsequent year.
      sig_increase=max(c(0,which(tbcompare_part$significance==T&tbcompare_part$rate>0)))
      # # The change point that is the year when the significant reduction in the growth rate compared to that in 2013 happened 
      # # and after which the reduction was maintained in subsequent years
      # sig_increase=max(c(0,which(tbcompare_part$rate>=0)))
      if(sig_increase<length(years)-1)
      {
        tbcompare_potent=tbcompare_part[(sig_increase+1):(length(years)-1),]
        if(any(tbcompare_potent$significance==T&tbcompare_potent$rate<0)) 
        {
          point=min(which(tbcompare_potent$significance==T&tbcompare_potent$rate<0))
          stay=c(tbcompare_potent$Years[point],paste0(round(-tbcompare_potent$rate[point],1),
                                                      '(',round(tbcompare_potent$rate.se[point],1),')'))
          
        }
        else stay=c('---','---')        
      }
      else stay=c('---','---')
      
      sig_increase2=max(c(0,which(tbcompare_part2$significance==T&tbcompare_part2$rate>0)))
      
      if(sig_increase2<length(years)-1)
      {
        tbcompare_potent2=tbcompare_part2[(sig_increase2+1):(length(years)-1),]
        if(any(tbcompare_potent2$significance==T&tbcompare_potent2$rate<0)) 
        {
          point2=min(which(tbcompare_potent2$significance==T&tbcompare_potent2$rate<0))
          stay2=c(tbcompare_potent2$Years[point2],paste0(round(-tbcompare_potent2$rate[point2],1),
                                                         '(',round(tbcompare_potent2$rate.se[point2],1),')'))
          
        }
        else stay2=c('---','---')        
      }
      else stay2=c('---','---')
      #------------
      small=which.min(tbcompare_part$rate)
      small2=which.min(tbcompare_part2$rate)
      
      if(min(tbcompare_part2$rate)<0)
        minn2=c(tbcompare_part2$Years[small2],paste0(round(-tbcompare_part2$rate[small2],1),'(',round(tbcompare_part2$rate.se[small2],1),')'))
      else minn2=c('---','---')
      if(min(tbcompare_part$rate)<0) 
        minn=c(tbcompare_part$Years[small],paste0(round(-tbcompare_part$rate[small],1),'(',round(tbcompare_part$rate.se[small],1),')'))
      else minn=c('---','---')
      
      tbcompare_summary=rbind(tbcompare_summary,c(pps[pp],seasons[i],rnames[u],stay2,minn2,aver2,stay,minn,aver))
    }
  }
}
tb_summary=as.data.frame(tb_summary)
colnames(tb_summary)=c('Pollutant','Season','Cluster',years,years)
tb_summary$Cluster=apply(as.matrix(tb_summary$Cluster),1,convert_sites)
tb_summary$Season=str_to_title(tb_summary$Season)
#Table S2
tb_summary[,c(1:3,1:length(years)+3+length(years))]
print(xtable(tb_summary[,c(1:3,1:length(years)+3+length(years))],
             caption=paste0("Growth Rate")),
      include.rownames =F)

tbcompare_summary=as.data.frame(tbcompare_summary)
colnames(tbcompare_summary)=c('Pollutant','Season','Cluster','Year','Difference','Year','Difference','Average reduction',
                              'Year','Difference','Year','Difference','Average reduction')
tbcompare_summary$Cluster=apply(as.matrix(tbcompare_summary$Cluster),1,convert_sites)
tbcompare_summary$Season=str_to_title(tbcompare_summary$Season)
#Table S3
print(xtable(tbcompare_summary[,c(1:3,9:13)],
             caption=paste0("The largest reduction in growth rate compared with that in 2013")),
      include.rownames =F)
