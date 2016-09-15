
format_orders1day<-function(df_tasks_for_today,clust_names){
  library(data.table)
  a<-data.table(matrix(NA,nrow=0,ncol=length(clust_names)))
  names(a)<-clust_names
  df_tasks_for_today <-data.table(df_tasks_for_today)
  df_tasks_for_today<-rbind(df_tasks_for_today,a,use.names = TRUE,fill=TRUE)
  pred_M_shft<-df_tasks_for_today[as.POSIXct(paste(Date,From,sep= " "),format="%Y-%m-%d %H:%M") <as.POSIXct(paste(Date,"14:00",sep= " "),format="%Y-%m-%d %H:%M"),lapply(.SD,sum), by=Date,.SDcols=c( "Camden" ,"City" ,"North East" , "Soho" ,"South" , "Southeast" ,"Victoria" ,"West (new)",  "West",   "EBERTY-ZENTRAL" ,"Central") ]
  pred_E_shft<-df_tasks_for_today[as.POSIXct(paste(Date,From,sep= " "),format="%Y-%m-%d %H:%M") >=as.POSIXct(paste(Date,"14:00",sep= " "),format="%Y-%m-%d %H:%M"), lapply(.SD,sum),by=Date,.SDcols=c( "Camden" ,"City" , "North East" , "Soho" ,"South" ,"Southeast" ,"Victoria" ,"West (new)", "West",    "EBERTY-ZENTRAL" ,"Central") ]
  table_1<-rbind(pred_M_shft,pred_E_shft,use.names=TRUE,fill=TRUE)
  shft_n<-c("MS","ES")
  table_1$Date<-paste(table_1[,Date],shft_n,sep="-")
  table_1$Date<-gsub("-","/",table_1$Date)
  return(table_1)
}
tasks_driv_1day<-function(wh_day){
#tasks and drivers for 1 day 
source("Demand_format.R")
source("make_idletimepivot.R")
clust_names<-c( "Camden" ,"City" ,"North East" , "Soho" ,"South" ,   "Southeast" ,"Victoria" , "West (new)", "West", "EBERTY-ZENTRAL","Central" )

dates<-c(wh_day,wh_day+1)
date_s<-strptime(dates[1],format="%Y-%m-%d",tz="UTC")
date_e<-strptime(dates[2],format="%Y-%m-%d",tz="UTC")

#------------------tasks



df_interactions_raw<-gen_data_distr_PUDO(date_s,date_e)
df_interactions_raw$timeslotfromasstringhours_localtime<-format(as.POSIXct(df_interactions_raw$timeslotfromasstringhours_localtime,format="%H:%M")+hours(1),format="%H:%M")
df_interactions_raw$timeslottoasstringhours_localtime<-format(as.POSIXct(df_interactions_raw$timeslottoasstringhours_localtime,format="%H:%M")+hours(1),format="%H:%M")
df_interactions_raw[df_interactions_raw$cluster=="Nord Westen","cluster"]<-"EBERTY-ZENTRAL"
df_interactions_raw[df_interactions_raw$cluster=="Zentral","cluster"]<-"EBERTY-ZENTRAL"
df_test <- format_interaction_2(df_interactions_raw, dates, daily = FALSE)
df_test <- one2two_hour_bins(df_test)
orders_1day<-format_orders1day(df_test,clust_names)



#---------drivers


idle_time_raw<-gen_data_idletime(date_s,date_e)

idle_time_raw[idle_time_raw$fleet__name=="Nord Westen","fleet__name"]<-"EBERTY-ZENTRAL"
idle_time_raw[idle_time_raw$fleet__name=="Zentral","fleet__name"]<-"EBERTY-ZENTRAL"
driver_nos_data<-data.table(idle_time_raw[,c("courier__name","fleet__name","Days.in.forecast__startAt__date","destination_from_hours")])

#morning shift
driver_nos_MS<-driver_nos_data[strptime(destination_from_hours,format="%H:%M")< strptime("14:00",format="%H:%M"),uniqueN(courier__name),by=.(fleet__name,Days.in.forecast__startAt__date)]
class(driver_nos_MS$V1)<-"numeric"
driver_nos_MS<-data.table(t(dcast(driver_nos_MS, fleet__name ~ Days.in.forecast__startAt__date, value.var = "V1")),keep.rownames = TRUE)
names(driver_nos_MS)<-c("Date",unlist(driver_nos_MS[1,])[-1])
driver_nos_MS<-driver_nos_MS[-1,]
driver_nos_MS$Date<-paste(driver_nos_MS$Date,"MS",sep="/")
cols<-setdiff(names(driver_nos_MS),"Date")
driver_nos_MS<-data.table(driver_nos_MS)
driver_nos_MS[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
#Evening shift
driver_nos_ES<-driver_nos_data[strptime(destination_from_hours,format="%H:%M")>= strptime("14:00",format="%H:%M"),uniqueN(courier__name),by=.(fleet__name,Days.in.forecast__startAt__date)]
class(driver_nos_ES$V1)<-"numeric"
driver_nos_ES<-data.table(t(dcast(driver_nos_ES, fleet__name ~ Days.in.forecast__startAt__date, value.var = "V1")),keep.rownames = TRUE)
names(driver_nos_ES)<-c("Date",unlist(driver_nos_ES[1,])[-1])
driver_nos_ES<-driver_nos_ES[-1,]
driver_nos_ES$Date<-paste(driver_nos_ES$Date,"ES",sep="/")
cols<-setdiff(names(driver_nos_ES),"Date")
driver_nos_ES<-data.table(driver_nos_ES)
driver_nos_ES[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
#both shifts
driver_nos<-data.table(matrix(NA,nrow=0,ncol=length(clust_names)+1))
names(driver_nos)<-c("Date",clust_names)
driver_nos<-rbind(driver_nos,driver_nos_MS,use.names = TRUE,fill=TRUE)
driver_nos<-rbind(driver_nos,driver_nos_ES,use.names = TRUE,fill=TRUE)
driver_nos$Date<-orders_1day$Date

#------------------travellingtime
ntt_int_time<-matrix(rep(c(18,17,21,17,17,18,15,17,18.4,15.15,17),each=2),nrow=2,byrow=F)
avgtrvl_time_tslots<-cbind(Date=orders_1day$Date,data.table(ntt_int_time * data.matrix(orders_1day[,c(2:ncol(orders_1day)),with=FALSE])))
table_9<-read.csv2("avg_shft_dur.csv",header = TRUE,sep="")
clust_names_table_9<-c("Wday","shift","Camden","Central","City","North East" ,"Soho", "South" ,"Southeast","Victoria","West (new)" ,"West","EBERTY-ZENTRAL")
names(table_9)<-clust_names_table_9
rno_sel_avg_shft1<-which(table_9$Wday==weekdays(wh_day))
table_9d<-table_9[rno_sel_avg_shft1,clust_names]
table_9<-data.table(Date=avgtrvl_time_tslots$Date,table_9d)
real_asd<-cbind(Date=avgtrvl_time_tslots$Date,avgtrvl_time_tslots[,c(2:ncol(avgtrvl_time_tslots)),with=F]/table_9[,c(2:ncol(table_9)),with=F])

return(list(orders_1day,driver_nos,real_asd))
}