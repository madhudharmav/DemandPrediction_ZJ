format_predorders<-function(df_prediction_all,test_dates){
  library(data.table)
  table_1 <-data.table(df_prediction_all[c(4:7),])
  table_1$Date<-gsub("-","/",table_1$Date)
  return(table_1)
}

format_realorders<-function(df_prediction_all,test_dates){
  library(data.table)
  df_prediction_2daysahead <-data.table(df_prediction_all)
  df_prediction_2daysahead<-df_prediction_2daysahead[Date==test_dates[2] | Date==test_dates[3] | Date==test_dates[4]]
  pred_M_shft<-df_prediction_2daysahead[as.POSIXct(paste(Date,From,sep= " "),format="%Y-%m-%d %H:%M") <as.POSIXct(paste(Date,"14:00",sep= " "),format="%Y-%m-%d %H:%M"),lapply(.SD,sum), by=Date,.SDcols=c( "Camden" ,"City" ,"North East" , "Soho" ,"South" , "Southeast" ,"Victoria" ,"West (new)",  "West",   "Zentrallager" ,"Central") ]
  pred_E_shft<-df_prediction_2daysahead[as.POSIXct(paste(Date,From,sep= " "),format="%Y-%m-%d %H:%M") >=as.POSIXct(paste(Date,"14:00",sep= " "),format="%Y-%m-%d %H:%M"), lapply(.SD,sum),by=Date,.SDcols=c( "Camden" ,"City" , "North East" , "Soho" ,"South" ,"Southeast" ,"Victoria" ,"West (new)", "West",    "Zentrallager","Central"  ) ]
  table_1<-rbind(pred_M_shft,pred_E_shft,use.names=TRUE,fill=TRUE)
  table_1<-table_1[c(4,2,5,3),]
  shft_n<-c("ES","MS","ES","MS")
  table_1$Date<-paste(table_1[,Date],shft_n,sep="-")
  table_1$Date<-gsub("-","/",table_1$Date)
  return(table_1)
}
addsmallshift<-function(table_7,table_9,table_10d,cluster){
  clusterss<-paste0(cluster,"_smallshifts")
  sl_lu<-rbind(c(0,1,1,1,1,2,2,2,3,3,3),c(0,0,1,2,3,2,3,4,3,4,5))
  for (i1 in c(1:4)){
    if(cluster=="Zentrallager" & (i1==1 | i1==3)){
      table_10d[i1,c(clusterss) := 0]
      next
    }
    if(unlist(table_9[i1,cluster,with=FALSE])!=0){
      a<-findInterval(table_7[i1,cluster,with=FALSE],unlist(table_9[i1,cluster,with=FALSE])*sl_lu[1,]+180*sl_lu[2,])
      longs_driv<-sl_lu[1,a]
      smalls_driv<-sl_lu[2,a]
      remtime<-table_7[i1,cluster,with=FALSE]-(unlist(table_9[i1,cluster,with=FALSE])*sl_lu[1,a]+180*sl_lu[2,a])
      if(sl_lu[1,a]!=sl_lu[1,a+1]){
        table_10d[i1,c(cluster) := unlist(remtime/unlist(table_9[i1,cluster,with=FALSE])+longs_driv)]
        table_10d[i1,c(clusterss):=smalls_driv]
      }else{
        table_10d[i1,c(cluster) := longs_driv]
        table_10d[i1,c(clusterss) := remtime/180+smalls_driv]
      }
    }
  }
  return(table_10d)
}
format_drivers_asd_utilrate_lon<-function(df_prediction_all,test_dates,df_test,clust_names){
  library(data.table)
 
  table_1<-format_predorders(df_prediction_all,test_dates)
  table_2<-format_realorders(df_test,test_dates)
  table_1<-table_1[,.SD,.SDcols=intersect(names(table_2),names(table_1))]
  table_2d<-table_2[,c(2:ncol(table_2)),with=F]
  table_3d<-table_1[,c(2:ncol(table_1)),with=F]-table_2[,c(2:ncol(table_1)),with=F]
  table_3<-cbind(Date=table_2[,Date],table_3d)
  table_4<-data.frame(t(c(17,15,17,17,21,18,17,18,18.4,15.8,14.5,17)))
  names(table_4)<-c("West (new)","Victoria","South","Soho","North East","Camden","City","Southeast","West","Zentral","Nord Westen","Central")
  table_4["Zentrallager"]<-mean(unlist(c(table_4["Zentral"],table_4["Nord Westen"])))
  table_4<-data.table(table_4[rep(nrow(table_4), each=4),])
  table_4<-table_4[,.SD,.SDcols=(intersect(names(table_3d), names(table_4)))]
  table_5<-cbind(Date=table_2[,Date],table_3d*table_4)
  table_6<-cbind(Date=table_2[,Date],table_2d*table_4)
  m1<-data.matrix(table_6[,c(2:ncol(table_6)),with=F])
  m2<-data.matrix(table_5[,c(2:ncol(table_5)),with=F])
  table_7d<-ifelse(is.na(m1), ifelse(is.na(m2), NA, m2), ifelse(is.na(m2), m1, m1 + m2))
  table_7<-data.table(Date=table_3[,Date],table_7d)
  table_9<-read.csv2("avg_shft_dur.csv",header = TRUE,sep="")
  names(table_9)<-c("Wday","shift","Camden", "Central", "City", "North East", "Soho", "South", "Southeast", "Victoria", "West (new)", "West", "Zentrallager" )
  rno_sel_avg_shft<-which(table_9$Wday==weekdays(test_dates[2]))[2]
  rno_sel_avg_shft1<-seq(rno_sel_avg_shft,rno_sel_avg_shft+3,1)
  rno_sel_avg_shft1[rno_sel_avg_shft1/14>1]<-rno_sel_avg_shft1[rno_sel_avg_shft1/14>1]%%14
  table_9d<-table_9[rno_sel_avg_shft1,clust_names]
  table_9<-data.table(Date=table_2[,Date],table_9d)
  table_10d<-table_7[,c(2:ncol(table_7)),with=F]/table_9[,c(2:ncol(table_9)),with=F]
  table_10d[abs(table_10d)==Inf]<-NA
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Victoria")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Soho")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"City")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Zentrallager")
  table_10<-cbind(Date=table_3[,Date],table_10d)
  drivcolnames<-c("Date","Camden", "City", "City_smallshifts", "North East", "Soho", "Soho_smallshifts", "South", "Southeast", "Victoria", "Victoria_smallshifts", "West (new)", "West", "Zentrallager","Zentrallager_smallshifts","Central")
  table_10<-table_10[,.SD,.SDcols=(intersect(drivcolnames, names(table_10)))]
  table_10d<-table_10[,c(2:ncol(table_10)),with=FALSE]
  table_12d<-utilrate_format_driv_london_smalls(table_10d)
  table_12<-cbind(Date=table_3[,Date],table_12d)
  return(table_12)
  
}

format_drivers_asd_utilrate_ber<-function(df_prediction_all,test_dates,df_test,clust_names){
  library(data.table)
 
  table_1<-format_predorders(df_prediction_all,test_dates)
  table_2<-format_realorders(df_test,test_dates)
  table_1<-table_1[,.SD,.SDcols=intersect(names(table_2),names(table_1))]
  table_2d<-table_2[,c(2:ncol(table_2)),with=F]
  table_3d<-table_1[,c(2:ncol(table_1)),with=F]-table_2[,c(2:ncol(table_1)),with=F]
  table_3<-cbind(Date=table_2[,Date],table_3d)
  table_4<-data.frame(t(c(17,15,17,17,21,18,17,18,18.4,15.8,14.5,17)))
  names(table_4)<-c("West (new)","Victoria","South","Soho","North East","Camden","City","Southeast","West","Zentral","Nord Westen","Central")
  table_4["Zentrallager"]<-mean(unlist(c(table_4["Zentral"],table_4["Nord Westen"])))
  table_4<-data.table(table_4[rep(nrow(table_4), each=4),])
  table_4<-table_4[,.SD,.SDcols=(intersect(names(table_3d), names(table_4)))]
  table_5<-cbind(Date=table_2[,Date],table_3d*table_4)
  table_6<-cbind(Date=table_2[,Date],table_2d*table_4)
  m1<-data.matrix(table_6[,c(2:ncol(table_6)),with=F])
  m2<-data.matrix(table_5[,c(2:ncol(table_5)),with=F])
  table_7d<-ifelse(is.na(m1), ifelse(is.na(m2), NA, m2), ifelse(is.na(m2), m1, m1 + m2))
  table_7<-data.table(Date=table_3[,Date],table_7d)
  table_9<-read.csv2("avg_shft_dur.csv",header = TRUE,sep="")
  names(table_9)<-c("Wday","shift","Camden", "Central", "City", "North East", "Soho", "South", "Southeast", "Victoria", "West (new)", "West", "Zentrallager" )
  rno_sel_avg_shft<-which(table_9$Wday==weekdays(test_dates[2]))[2]
  rno_sel_avg_shft1<-seq(rno_sel_avg_shft,rno_sel_avg_shft+3,1)
  rno_sel_avg_shft1[rno_sel_avg_shft1/14>1]<-rno_sel_avg_shft1[rno_sel_avg_shft1/14>1]%%14
  table_9d<-table_9[rno_sel_avg_shft1,clust_names]
  table_9<-data.table(Date=table_2[,Date],table_9d)
  table_10d<-table_7[,c(2:ncol(table_7)),with=F]/table_9[,c(2:ncol(table_9)),with=F]
  table_10d[abs(table_10d)==Inf]<-NA
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Victoria")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Soho")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"City")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Zentrallager")
  table_10<-cbind(Date=table_3[,Date],table_10d)
  drivcolnames<-c("Date","Camden", "City", "City_smallshifts", "North East", "Soho", "Soho_smallshifts", "South", "Southeast", "Victoria", "Victoria_smallshifts", "West (new)", "West", "Zentrallager","Zentrallager_smallshifts","Central")
  table_10<-table_10[,.SD,.SDcols=(intersect(drivcolnames, names(table_10)))]
  table_10d<-table_10[,c(2:ncol(table_10)),with=FALSE]
  table_12d<-utilrate_format_driv_berlin_smalls(table_10d)
  table_12<-cbind(Date=table_3[,Date],table_12d)
  return(table_12)
  
}


format_drivers_asd_utilrate_par<-function(df_prediction_all,test_dates,df_test,clust_names){
  library(data.table)
  
  table_1<-format_predorders(df_prediction_all,test_dates)
  table_2<-format_realorders(df_test,test_dates)
  table_1<-table_1[,.SD,.SDcols=intersect(names(table_2),names(table_1))]
  table_2d<-table_2[,c(2:ncol(table_2)),with=F]
  table_3d<-table_1[,c(2:ncol(table_1)),with=F]-table_2[,c(2:ncol(table_1)),with=F]
  table_3<-cbind(Date=table_2[,Date],table_3d)
  table_4<-data.frame(t(c(17,15,17,17,21,18,17,18,18.4,15.8,14.5,17)))
  names(table_4)<-c("West (new)","Victoria","South","Soho","North East","Camden","City","Southeast","West","Zentral","Nord Westen","Central")
  table_4["Zentrallager"]<-mean(unlist(c(table_4["Zentral"],table_4["Nord Westen"])))
  table_4<-data.table(table_4[rep(nrow(table_4), each=4),])
  table_4<-table_4[,.SD,.SDcols=(intersect(names(table_3d), names(table_4)))]
  table_5<-cbind(Date=table_2[,Date],table_3d*table_4)
  table_6<-cbind(Date=table_2[,Date],table_2d*table_4)
  m1<-data.matrix(table_6[,c(2:ncol(table_6)),with=F])
  m2<-data.matrix(table_5[,c(2:ncol(table_5)),with=F])
  table_7d<-ifelse(is.na(m1), ifelse(is.na(m2), NA, m2), ifelse(is.na(m2), m1, m1 + m2))
  table_7<-data.table(Date=table_3[,Date],table_7d)
  table_9<-read.csv2("avg_shft_dur.csv",header = TRUE,sep="")
  names(table_9)<-c("Wday","shift","Camden", "Central", "City", "North East", "Soho", "South", "Southeast", "Victoria", "West (new)", "West", "Zentrallager" )
  rno_sel_avg_shft<-which(table_9$Wday==weekdays(test_dates[2]))[2]
  rno_sel_avg_shft1<-seq(rno_sel_avg_shft,rno_sel_avg_shft+3,1)
  rno_sel_avg_shft1[rno_sel_avg_shft1/14>1]<-rno_sel_avg_shft1[rno_sel_avg_shft1/14>1]%%14
  table_9d<-table_9[rno_sel_avg_shft1,clust_names]
  table_9<-data.table(Date=table_2[,Date],table_9d)
  table_10d<-table_7[,c(2:ncol(table_7)),with=F]/table_9[,c(2:ncol(table_9)),with=F]
  table_10d[abs(table_10d)==Inf]<-NA
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Victoria")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Soho")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"City")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Zentrallager")
  table_10<-cbind(Date=table_3[,Date],table_10d)
  drivcolnames<-c("Date","Camden", "City", "City_smallshifts", "North East", "Soho", "Soho_smallshifts", "South", "Southeast", "Victoria", "Victoria_smallshifts", "West (new)", "West", "Zentrallager","Zentrallager_smallshifts","Central")
  table_10<-table_10[,.SD,.SDcols=(intersect(drivcolnames, names(table_10)))]
  table_10d<-table_10[,c(2:ncol(table_10)),with=FALSE]
  table_12d<-utilrate_format_driv_paris_smalls(table_10d)
  table_12<-cbind(Date=table_3[,Date],table_12d)
  table_12$East<-1
  table_12$East[which(table_9$Central==0)]<-NA
  return(table_12)
  
}


format_asd_values<-function(df_prediction_all,test_dates,df_test,clust_names){
  library(data.table)
  
  table_1<-format_predorders(df_prediction_all,test_dates)
  table_2<-format_realorders(df_test,test_dates)
  table_1<-table_1[,.SD,.SDcols=intersect(names(table_2),names(table_1))]
  table_2d<-table_2[,c(2:ncol(table_2)),with=F]
  table_3d<-table_1[,c(2:ncol(table_1)),with=F]-table_2[,c(2:ncol(table_1)),with=F]
  table_3<-cbind(Date=table_2[,Date],table_3d)
  table_4<-data.frame(t(c(17,15,17,17,21,18,17,18,18.4,15.8,14.5,17)))
  names(table_4)<-c("West (new)","Victoria","South","Soho","North East","Camden","City","Southeast","West","Zentral","Nord Westen","Central")
  table_4["Zentrallager"]<-mean(unlist(c(table_4["Zentral"],table_4["Nord Westen"])))
  table_4<-data.table(table_4[rep(nrow(table_4), each=4),])
  table_4<-table_4[,.SD,.SDcols=(intersect(names(table_3d), names(table_4)))]
  table_5<-cbind(Date=table_2[,Date],table_3d*table_4)
  table_6<-cbind(Date=table_2[,Date],table_2d*table_4)
  m1<-data.matrix(table_6[,c(2:ncol(table_6)),with=F])
  m2<-data.matrix(table_5[,c(2:ncol(table_5)),with=F])
  table_7d<-ifelse(is.na(m1), ifelse(is.na(m2), NA, m2), ifelse(is.na(m2), m1, m1 + m2))
  table_7<-data.table(Date=table_3[,Date],table_7d)
  table_9<-read.csv2("avg_shft_dur.csv",header = TRUE,sep="")
  names(table_9)<-c("Wday","shift","Camden", "Central", "City", "North East", "Soho", "South", "Southeast", "Victoria", "West (new)", "West", "Zentrallager" )
  rno_sel_avg_shft<-which(table_9$Wday==weekdays(test_dates[2]))[2]
  rno_sel_avg_shft1<-seq(rno_sel_avg_shft,rno_sel_avg_shft+3,1)
  rno_sel_avg_shft1[rno_sel_avg_shft1/14>1]<-rno_sel_avg_shft1[rno_sel_avg_shft1/14>1]%%14
  table_9d<-table_9[rno_sel_avg_shft1,clust_names]
  table_9<-data.table(Date=table_2[,Date],table_9d)
  table_10d<-table_7[,c(2:ncol(table_7)),with=F]/table_9[,c(2:ncol(table_9)),with=F]
  table_10d[abs(table_10d)==Inf]<-NA
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Victoria")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Soho")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"City")
  table_10d<-addsmallshift(table_7,table_9,table_10d,"Zentrallager")
  table_10<-cbind(Date=table_3[,Date],table_10d)
  drivcolnames<-c("Date","Camden", "City", "City_smallshifts", "North East", "Soho", "Soho_smallshifts", "South", "Southeast", "Victoria", "Victoria_smallshifts", "West (new)", "West", "Zentrallager","Zentrallager_smallshifts","Central")
  table_10<-table_10[,.SD,.SDcols=(intersect(drivcolnames, names(table_10)))]
  table_10d<-table_10[,c(2:ncol(table_10)),with=FALSE]
  return(table_10)
  
}

