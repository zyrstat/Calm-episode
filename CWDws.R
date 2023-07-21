# path4-->path7
# path5-->path8
wd_CWS <- function(stationdata,WD)
{
  IWD=rep(0,nrow(stationdata))
  IWD[!is.na(stationdata$wd)&stationdata$wd==WD]=1

  CWDws=rep(0,nrow(stationdata))#北风连续累积风速
  
  WS=stationdata$WSPM
  WS[is.na(WS)]=0
  if(IWD[1]==1)
    CWDws[1]=WS[1]
  
  for(k in 2:nrow(stationdata)) 
  {
    if(IWD[k]==1)
    {
      CWDws[k]=CWDws[k-1]+WS[k]
    }
  }
  return(CWDws)
}

onames_update=c('Beijing_Dongsi','Beijing_Tiantan','Beijing_Nongzhanguan',
                'Beijing_Guanyuan','Beijing_Wanliu','Beijing_Aotizhongxin',
                'Tangshan_Shierzhong','Tangshan_Wuziju','Tangshan_Leidazhan',
                'Baoding_Huadianerqu','Baoding_Youyongguan','Baoding_Jiancezhan')
added=paste0('C',c("ENE","NE","NNE","N","NNW","NW","WNW","W","WSW","SW","SSW","S","SSE","SE","ESE","E","CV"))
for(i in 1:length(onames_update))
{
  stationdata=read.csv(paste0(path4,onames_update[i],"_NS.csv"),stringsAsFactors = F)
  stationdata[,added]=matrix(rep(0,nrow(stationdata)*17,ncol=17))
  for(j in c("ENE","NE","NNE","N","NNW","NW","WNW","W","WSW","SW","SSW","S","SSE","SE","ESE","E","CV"))
  {
    stationdata[,paste0('C',j)]=wd_CWS(stationdata,j)
  }
  write.csv(stationdata,file=paste0(path4,onames_update[i],"_NS.csv"))
  stationd=read.csv(paste0(path5,onames_update[i],"_cleaned.csv"),stringsAsFactors = F)
  stationd=inner_join(stationd,stationdata[,c('X',added)],by=c('oX'='X'))
  
  write.csv(stationd,file=paste0(path5,onames_update[i],"_cleaned.csv"))
  print(onames_update[i])
}

