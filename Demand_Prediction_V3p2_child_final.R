
# ----------------------------------------------------------------------- #
# 1. Parameter settings -----
# -------------- #
# where are located the scripts ?
setwd("/home/madhu/workspace/Demandprediction/workingversion")

# -------------- #
# DATA ...

#last_date<-"2016-08-17"
#today<-as.Date(last_date)
today<-Sys.Date()
last_date<-strftime(today)


nb_weeks <- 10 # total number of weeks 
testing_days <- 7 



# ----------------------------------------------------------------------- #
# 2. Loading the packages, functions -----
require(plyr)
require(dplyr)
require(dummies)
require(ggplot2)
require(viridis)
require(forecast)
require(caret)
require(xgboost)
require(rpart.plot)
require(vars)
require(lubridate)
require(magrittr )

set.seed(1)

# load functions
source("Demand_format.R")
source("Demand_models_hourly.R")
source("Demand_conv2drivers.R")
source("make_idletimepivot.R")
source("predictions_cluster_V3p2_diff_noit.R")
source("getcreateddatebyref.R")



# ----------------------------------------------------------------------- #
# 3. Loading the  data, formatting and cleaning -----

# training days 
dates <- rep(as.Date(last_date, format = "%Y-%m-%d"), 2)
dates[1] <- dates[2] - 7*nb_weeks+1
date_s<-strptime(dates[1],format="%Y-%m-%d",tz="UTC")
date_e<-strptime(dates[2]+1,format="%Y-%m-%d",tz="UTC")

# ... interactions
df_interactions_raw<-gen_data_distr_PUDO(date_s,date_e)
df_interactions_raw$timeslotfromasstringhours_localtime<-format(as.POSIXct(df_interactions_raw$timeslotfromasstringhours_localtime,format="%H:%M")+hours(1),format="%H:%M")
df_interactions_raw$timeslottoasstringhours_localtime<-format(as.POSIXct(df_interactions_raw$timeslottoasstringhours_localtime,format="%H:%M")+hours(1),format="%H:%M")
df_interactions_raw[df_interactions_raw$cluster=="Nord Westen","cluster"]<-"EBERTY-ZENTRAL"
df_interactions_raw[df_interactions_raw$cluster=="Zentral","cluster"]<-"EBERTY-ZENTRAL"
hourly_interactions <- format_interaction_2(df_interactions_raw, dates, daily = FALSE)
a<-data.table(hourly_interactions)
a1<-a[strptime(From,format="%H:%M")<strptime("14:00",format="%H:%M")]
daily_interactions_MS<-setkey(a1,Date)[,lapply(.SD,sum,na.rm=T) ,.SDcols=setdiff(names(a1),c("Date","From")),by="Date"][CJ(unique(a$Date)),allow.cartesian=TRUE]
a1<-a[strptime(From,format="%H:%M")>=strptime("14:00",format="%H:%M")]
daily_interactions_ES<-setkey(a1,Date)[,lapply(.SD,sum,na.rm=T) ,.SDcols=setdiff(names(a1),c("Date","From")),by="Date"][CJ(unique(a$Date)),allow.cartesian=TRUE]
#eberty corrections
e_ms<-daily_interactions_MS[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`]
e_es<-daily_interactions_ES[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`]
daily_interactions_MS[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`:=rowSums(data.frame(e_ms,e_es),na.rm = TRUE)]
daily_interactions_ES[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`:=NA]

#regressors for training
df_interactions_48hrsahead<-gen_48hrsahead_PUDO(date_s,date_e)
df_interactions_48hrsahead<-df_interactions_48hrsahead[names(df_interactions_raw)]
df_interactions_48hrsahead$timeslotfromasstringhours_localtime<-format(as.POSIXct(df_interactions_48hrsahead$timeslotfromasstringhours_localtime,format="%H:%M")+hours(1),format="%H:%M")
df_interactions_48hrsahead$timeslottoasstringhours_localtime<-format(as.POSIXct(df_interactions_48hrsahead$timeslottoasstringhours_localtime,format="%H:%M")+hours(1),format="%H:%M")
df_interactions_48hrsahead[df_interactions_48hrsahead$cluster=="Nord Westen","cluster"]<-"EBERTY-ZENTRAL"
df_interactions_48hrsahead[df_interactions_48hrsahead$cluster=="Zentral","cluster"]<-"EBERTY-ZENTRAL"
hourly_interactions_48hrsahead <- format_interaction_2(df_interactions_48hrsahead, dates, daily = FALSE)
a<-data.table(hourly_interactions_48hrsahead)
a1<-a[strptime(From,format="%H:%M")<strptime("14:00",format="%H:%M")]
daily_interactions_48hrsahead_MS<-setkey(a1,Date)[,lapply(.SD,sum,na.rm=T) ,.SDcols=setdiff(names(a1),c("Date","From")),by="Date"][CJ(unique(a$Date)),allow.cartesian=TRUE]
a1<-a[strptime(From,format="%H:%M")>=strptime("14:00",format="%H:%M")]
daily_interactions_48hrsahead_ES<-setkey(a1,Date)[,lapply(.SD,sum,na.rm=T) ,.SDcols=setdiff(names(a1),c("Date","From")),by="Date"][CJ(unique(a$Date)),allow.cartesian=TRUE]
e_ms<-daily_interactions_48hrsahead_MS[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`]
e_es<-daily_interactions_48hrsahead_ES[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`]
daily_interactions_48hrsahead_MS[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`:=rowSums(data.frame(e_ms,e_es),na.rm = TRUE)]
daily_interactions_48hrsahead_ES[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`:=NA]


#forecasting days
test_dates <- (dates[2] + seq(1,testing_days , 1))
date_s_test<-strptime(test_dates[1],format="%Y-%m-%d",tz="UTC")
date_e_test<-strptime(test_dates[testing_days]+1,format="%Y-%m-%d",tz="UTC")

#regressors for forecasting
df_interactions_raw_test<-gen_48hrsahead_PUDO(date_s_test,date_e_test)
df_interactions_raw_test<-df_interactions_raw_test[names(df_interactions_raw)]
df_interactions_raw_test$timeslotfromasstringhours_localtime<-format(as.POSIXct(df_interactions_raw_test$timeslotfromasstringhours_localtime,format="%H:%M")+hours(1),format="%H:%M")
df_interactions_raw_test$timeslottoasstringhours_localtime<-format(as.POSIXct(df_interactions_raw_test$timeslottoasstringhours_localtime,format="%H:%M")+hours(1),format="%H:%M")
df_interactions_raw_test[df_interactions_raw_test$cluster=="Nord Westen","cluster"]<-"EBERTY-ZENTRAL"
df_interactions_raw_test[df_interactions_raw_test$cluster=="Zentral","cluster"]<-"EBERTY-ZENTRAL"
hourly_interactions_raw_test <- format_interaction_2(df_interactions_raw_test, c(head(test_dates, 1),tail(test_dates, 1)), daily = FALSE)
a<-data.table(hourly_interactions_raw_test)
a1<-a[strptime(From,format="%H:%M")<strptime("14:00",format="%H:%M")]
daily_interactions_test_48hrsahead_MS<-setkey(a1,Date)[,lapply(.SD,sum,na.rm=T) ,.SDcols=setdiff(names(a1),c("Date","From")),by="Date"][CJ(unique(a$Date)),allow.cartesian=TRUE]
a1<-a[strptime(From,format="%H:%M")>=strptime("14:00",format="%H:%M")]
daily_interactions_test_48hrsahead_ES<-setkey(a1,Date)[,lapply(.SD,sum,na.rm=T) ,.SDcols=setdiff(names(a1),c("Date","From")),by="Date"][CJ(unique(a$Date)),allow.cartesian=TRUE]
e_ms<-daily_interactions_test_48hrsahead_MS[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`]
e_es<-daily_interactions_test_48hrsahead_ES[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`]
daily_interactions_test_48hrsahead_MS[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`:=rowSums(data.frame(e_ms,e_es),na.rm = TRUE)]
daily_interactions_test_48hrsahead_ES[weekdays(Date)=="Saturday",`EBERTY-ZENTRAL`:=NA]

#current tasks for forecast days
df_test <- format_interaction_2(df_interactions_raw_test, c(head(test_dates, 1),tail(test_dates, 1)), daily = FALSE)
df_test <- one2two_hour_bins(df_test)


#------------------------------------------------Forecasting orders and drivers for each cluster--------------------------------#
# MODELS ...
# daily model 
# available: "stl", "HoltWinters", "ets", "stlm", "Arima",  "Arimax", "tbats"

clusters_all<-c("Victoria" , "Camden" ,"City" , "North East" , "Soho" ,"South" ,"Southeast" , "West",  "West (new)",  "EBERTY-ZENTRAL","Central" )  
#df_drivers_all<-NULL
#df_prediction_all<-NULL
df_prediction_all<-data.frame(row.names=1:14)

for (district in clusters_all){
  print(district)
  if(district %in% c("EBERTY-ZENTRAL" ,  "West")){
    city<-"Berlin"
  } else if(district %in% c("Central")){
    city<-"Paris"
  }else{
    city<-"London"
  }
  
  #-----predicting tasks model--------
  df_prediction_MS<-predictions_cluster_V3p2_diff_noit(district,testing_days,daily_interactions_MS,dates,daily_interactions_48hrsahead_MS,daily_interactions_test_48hrsahead_MS)
  df_prediction_ES<-predictions_cluster_V3p2_diff_noit(district,testing_days,daily_interactions_ES,dates,daily_interactions_48hrsahead_ES,daily_interactions_test_48hrsahead_ES)
  
  df_prediction_all<-cbind(df_prediction_all,rbind(df_prediction_MS,df_prediction_ES)[c(matrix(1:14,nrow=2,byrow=TRUE))])
}

df_prediction_all<-data.frame(Date=paste0(rep(test_dates,each=2),rep(c("-MS", "-ES"),nrow(df_prediction_MS))),df_prediction_all,stringsAsFactors=FALSE)
names(df_prediction_all)<-c("Date",clusters_all)


#------------------------------Calculating drivers allotted for forecast days ------------------------------------
idle_time_raw_test<-gen_data_idletime(date_s_test,date_e_test)
idle_time_raw_test[idle_time_raw_test$fleet__name=="Nord Westen","fleet__name"]<-"EBERTY-ZENTRAL"
idle_time_raw_test[idle_time_raw_test$fleet__name=="Zentral","fleet__name"]<-"EBERTY-ZENTRAL"
driver_nos_data<-data.table(idle_time_raw_test[,c("courier__name","fleet__name","Days.in.forecast__startAt__date","destination_from_hours")])
driver_nos_data<-driver_nos_data[as.POSIXct(paste(Days.in.forecast__startAt__date,destination_from_hours,sep=" "), format="%m/%d/%y %H:%M")<as.POSIXct(paste(today+4,"14:00"))]
driver_nos_data<-driver_nos_data[as.POSIXct(paste(Days.in.forecast__startAt__date,destination_from_hours,sep=" "), format="%m/%d/%y %H:%M")>=as.POSIXct(paste(today+2,"14:00"))]

#morning shift
driver_nos_MS<-driver_nos_data[strptime(destination_from_hours,format="%H:%M")< strptime("14:00",format="%H:%M"),uniqueN(courier__name),by=.(fleet__name,Days.in.forecast__startAt__date)]
class(driver_nos_MS$V1)<-"numeric"
driver_nos_MS<-data.table(t(dcast(driver_nos_MS, fleet__name ~ Days.in.forecast__startAt__date, value.var = "V1")),keep.rownames = TRUE)
names(driver_nos_MS)<-c("Date",unlist(driver_nos_MS[1,])[-1])
driver_nos_MS<-driver_nos_MS[-1,]
driver_nos_MS$Date<-paste(as.Date(driver_nos_MS$Date,format="%m/%d/%y"),"-MS",sep="")
cols<-setdiff(names(driver_nos_MS),"Date")
driver_nos_MS<-data.table(left_join(data.frame("Date"=paste(rep(seq(today+2,today+4,1)),c("MS","MS","MS"),sep="-")),driver_nos_MS))
driver_nos_MS[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
#Evening shift
driver_nos_ES<-driver_nos_data[strptime(destination_from_hours,format="%H:%M")>= strptime("14:00",format="%H:%M"),uniqueN(courier__name),by=.(fleet__name,Days.in.forecast__startAt__date)]
class(driver_nos_ES$V1)<-"numeric"
driver_nos_ES<-data.table(t(dcast(driver_nos_ES, fleet__name ~ Days.in.forecast__startAt__date, value.var = "V1")),keep.rownames = TRUE)
names(driver_nos_ES)<-c("Date",unlist(driver_nos_ES[1,])[-1])
driver_nos_ES<-driver_nos_ES[-1,]
driver_nos_ES$Date<-paste(as.Date(driver_nos_ES$Date,format="%m/%d/%y"),"-ES",sep="")
cols<-setdiff(names(driver_nos_ES),"Date")
driver_nos_ES<-data.table(left_join(data.frame("Date"=paste(rep(seq(today+2,today+4,1)),c("ES","ES","ES"),sep="-")),driver_nos_ES))
driver_nos_ES[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
#both shifts
driver_nos<-data.table(matrix(NA,nrow=0,ncol=length(clusters_all)))
names(driver_nos)<-clusters_all
driver_nos<-rbind(driver_nos,driver_nos_MS,use.names = TRUE,fill=TRUE)
driver_nos<-rbind(driver_nos,driver_nos_ES,use.names = TRUE,fill=TRUE)
driver_nos<-driver_nos[c(4,2,5,3),]

#------save output
save(driver_nos,df_prediction_all,test_dates,df_test,file=paste("df_prediction_",today,".dat",sep=""))
