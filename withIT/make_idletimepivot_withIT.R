

library(rmongodb)
library(plyr)
library(lubridate)
library(data.table)
library(reshape2)

flatlist <- function(mylist){
  lapply(rapply(mylist, enquote, how="unlist"), eval)
}
flattenList <- function(x){
  x<-rbind(lapply(x,function(y){data.table(t(y))}),fill=TRUE)
}



gen_data_demandpred<-function(start_date,last_date){
#loads fields_needed
fields_needed<-list( "start.assignedTo" =1L, 
                     "start.type"=1L,
                     "courier.name"=1L,
                     "fleet.name"=1L,
                     "start.externalId"=1L,
                     "destination.externalId"=1L,
                     "forecast.startAt.date"=1L,
                     "forecast.startAt.tz"=1L,
                     "forecast.completedAt.date"=1L,
                     "forecast.completedAt.tz"=1L,
                     "destination.serviceSlot.from.date"=1L,
                     "destination.serviceSlot.from.tz"=1L,
                     "destination.serviceSlot.to.date"=1L,
                     "destination.serviceSlot.to.tz"=1L,
                     "forecast.travelingDistance"=1L,
                     "forecast.travelingTime"=1L)

#mongo.destroy(mongo)
mongo <- mongo.create(host="172.31.51.215:27017", db="alyx-live")
mongo.is.connected(mongo)
cursor<-mongo.find(mongo, "alyx-live.bi_task_predictions",query=list("start.serviceSlot.from.date" = list("$gt" = start_date,"$lte" = last_date)),  sort = mongo.bson.empty(),fields = fields_needed, options = 4L )
doc3<-mongo.cursor.to.list(cursor)
mongo.destroy(mongo)

#flatten each list
doc3<-lapply(doc3,flatlist)
#remove mongoid fields
doc3<-lapply(doc3,function(x)(x=x[which(sapply(x,class)!="mongo.oid")]))

#combine lists by column names and save as dataframe
doc3<-rbindlist(doc3,use.names=TRUE,fill=TRUE)
doc3<-doc3[!duplicated(doc3)]
#remove all rows of duplicate startid
#doc3<-doc3[!(duplicated(doc3$start.externalId) | duplicated(doc3$start.externalId, fromLast = TRUE)), ]
#doc3<-doc3[!(duplicated(doc3$destination.externalId) | duplicated(doc3$destination.externalId, fromLast = TRUE)), ]
#doc3<-doc3[ start.type == "DROPOFF" | start.type=="PICKUP"]
doc3<-inner_join(doc3,doc3[,.(start.externalId,forecast.startAt.date,forecast.startAt.tz)],by=c("destination.externalId"="start.externalId"))

doc3<-data.table(doc3)
doc3<-doc3[!duplicated(doc3)]
doc3$final_idletime<-round(difftime(doc3$forecast.startAt.date.y,doc3$forecast.completedAt.date,units="mins"))

a<-doc3[,.I[forecast.startAt.tz.x=="Europe/Berlin"]]
doc3$forecast.startAt.date.x[a]<-as.character(with_tz(doc3[a,forecast.startAt.date.x],tzone="Europe/Berlin"))
doc3$destination.serviceSlot.to.date[a]<-as.character(with_tz(doc3[a,destination.serviceSlot.to.date],tzone="Europe/Berlin"))
doc3$destination.serviceSlot.from.date[a]<-as.character(with_tz(doc3[a,destination.serviceSlot.from.date],tzone="Europe/Berlin"))
doc3$forecast.startAt.date.y[a]<-as.character(with_tz(doc3[a,forecast.startAt.date.y],tzone="Europe/Berlin"))
doc3$forecast.completedAt.date[a]<-as.character(with_tz(doc3[a,forecast.completedAt.date],tzone="Europe/Berlin"))


a<-doc3[,.I[forecast.startAt.tz.x== "Europe/London"]]
doc3$forecast.startAt.date.x[a]<-as.character(with_tz(doc3[a,forecast.startAt.date.x],tzone= "Europe/London"))
doc3$destination.serviceSlot.to.date[a]<-as.character(with_tz(doc3[a,destination.serviceSlot.to.date],tzone= "Europe/London"))
doc3$destination.serviceSlot.from.date[a]<-as.character(with_tz(doc3[a,destination.serviceSlot.from.date],tzone= "Europe/London"))
doc3$forecast.startAt.date.y[a]<-as.character(with_tz(doc3[a,forecast.startAt.date.y],tzone="Europe/London"))
doc3$forecast.completedAt.date[a]<-as.character(with_tz(doc3[a,forecast.completedAt.date],tzone="Europe/London"))


a<-doc3[,.I[forecast.startAt.tz.x== "Europe/Paris" ]]
doc3$forecast.startAt.date.x[a]<-as.character(with_tz(doc3[a,forecast.startAt.date.x],tzone= "Europe/Paris" ))
doc3$destination.serviceSlot.to.date[a]<-as.character(with_tz(doc3[a,destination.serviceSlot.to.date],tzone= "Europe/Paris" ))
doc3$destination.serviceSlot.from.date[a]<-as.character(with_tz(doc3[a,destination.serviceSlot.from.date],tzone= "Europe/Paris" ))
doc3$forecast.startAt.date.y[a]<-as.character(with_tz(doc3[a,forecast.startAt.date.y],tzone="Europe/Paris"))
doc3$forecast.completedAt.date[a]<-as.character(with_tz(doc3[a,forecast.completedAt.date],tzone="Europe/Paris"))
return(doc3)
}

gen_data_idletime<-function(start_date,last_date){
  
doc3<-gen_data_demandpred(start_date,last_date)
nb_driver_raw<-transmute(doc3,heads_id=start.assignedTo,
                         courier__name=courier.name,
                         fleet__name=fleet.name,
                         start__externalId=start.externalId,
                         destination__externalId=destination.externalId,
                         Days.in.forecast__startAt__date=strftime(as.Date(forecast.startAt.date.x),format="%m/%d/%y"),
                         startat_hours=strftime(forecast.startAt.date.x,format="%H:%M"),
                         Days.in.forecast__completedAt__date=strftime(as.Date(forecast.completedAt.date),format="%m/%d/%y"),
                         completedat_hours=strftime(forecast.completedAt.date,format="%H:%M"),
                         Days.in.forecast_next_task_start=strftime(as.Date(forecast.startAt.date.y),format="%m/%d/%y"),
                         nexttask_hours=strftime(forecast.startAt.date.y,format="%H:%M"),
                         final_idletime=final_idletime,
                         destination_from_hours=strftime(destination.serviceSlot.from.date,format="%H:%M"),
                         destination_to_hours=strftime(destination.serviceSlot.to.date,format="%H:%M"),
                         forecast__travelingDistance=forecast.travelingDistance,
                         forecast__travelingTime=forecast.travelingTime
                         
)
nb_driver_raw<-data.frame(nb_driver_raw)
return(nb_driver_raw)

}

gen_data_distr_PUDO<-function(start_date,last_date){
doc3<-gen_data_demandpred(start_date,last_date)
doc3<-doc3[start.type=="DROPOFF" | start.type=="PICKUP"]

df_interactions_raw_pre<-transmute(doc3,pudo=start.type,
                             Days.in.timeslot__from=strftime(as.Date(destination.serviceSlot.from.date),format="%m/%d/%y"),
                             timeslotfromasstringhours_localtime=strftime(destination.serviceSlot.from.date,format="%H:%M"),
                             timeslottoasstringhours_localtime=strftime(destination.serviceSlot.to.date,format="%H:%M"),
                             city=destination.serviceSlot.to.tz,
                             cluster=fleet.name
 )
df_interactions_raw_pre<-data.table(df_interactions_raw_pre)

df_interactions_raw_pre<-df_interactions_raw_pre[pudo=="PICKUP", c("pickups") := .N, by=names(df_interactions_raw_pre[,-1,with=FALSE])]
df_interactions_raw_pre<-df_interactions_raw_pre[pudo=="DROPOFF", c("dropoffs") := .N, by=names(df_interactions_raw_pre[,-1,with=FALSE])]
df_interactions_raw<-df_interactions_raw_pre[!duplicated(df_interactions_raw_pre)]
df_interactions_raw$city<-colsplit(df_interactions_raw$city, pattern ="/", names = c('a', 'b'))[,2]
df_interactions_raw<-data.frame(df_interactions_raw)
df_interactions_raw<-df_interactions_raw[,-1]
return(df_interactions_raw)
}

gen_48hrsahead_PUDO<-function(start_date,last_date){
  doc3<-gen_data_demandpred(start_date,last_date)
  doc3<-doc3[start.type=="DROPOFF" | start.type=="PICKUP"]
  
  df_interactions_48hrsahead<-transmute(doc3,pudo=start.type,
                                     start__externalId=start.externalId,
                                     timeslot__task=destination.serviceSlot.from.date,
                                     timeslottoasstringhours_localtime=strftime(destination.serviceSlot.to.date,format="%H:%M"),
                                     city=destination.serviceSlot.to.tz,
                                     cluster=fleet.name
  )
  df_interactions_48hrsahead<-data.table(df_interactions_48hrsahead)
  createddates<-get_createddatebyref(df_interactions_48hrsahead$start__externalId)
  df_interactions_48hrsahead<-cbind.data.frame(df_interactions_48hrsahead,createddates)
  df_interactions_48hrsahead<-data.table(df_interactions_48hrsahead)
  
  a<-df_interactions_48hrsahead[,.I[substr(start__externalId,1,2)=="DE" ]]
  df_interactions_48hrsahead$createddates[a]<-as.character(with_tz(df_interactions_48hrsahead[a,createddates],tzone= "Europe/Berlin" ))
  a<-df_interactions_48hrsahead[,.I[substr(start__externalId,1,2)=="GB" ]]
  df_interactions_48hrsahead$createddates[a]<-as.character(with_tz(df_interactions_48hrsahead[a,createddates],tzone= "Europe/London" ))
  a<-df_interactions_48hrsahead[,.I[substr(start__externalId,1,2)=="FR" ]]
  df_interactions_48hrsahead$createddates[a]<-as.character(with_tz(df_interactions_48hrsahead[a,createddates],tzone= "Europe/Paris" ))
  
  
  
  df_interactions_48hrsahead<-cbind.data.frame(df_interactions_48hrsahead,createddates)
  df_interactions_48hrsahead<-data.table(df_interactions_48hrsahead[difftime(df_interactions_48hrsahead[,timeslot__task],df_interactions_48hrsahead[,createddates],units="hours")>48,])
  df_interactions_48hrsahead$createddates<-NULL
  df_interactions_48hrsahead$start__externalId<-NULL
  df_interactions_48hrsahead<-df_interactions_48hrsahead[pudo=="PICKUP", c("pickups") := .N, by=names(df_interactions_48hrsahead[,-1,with=FALSE])]
  df_interactions_48hrsahead<-df_interactions_48hrsahead[pudo=="DROPOFF", c("dropoffs") := .N, by=names(df_interactions_48hrsahead[,-1,with=FALSE])]
  df_interactions_48hrsahead<-df_interactions_48hrsahead[!duplicated(df_interactions_48hrsahead)]
  df_interactions_48hrsahead$city<-colsplit(df_interactions_48hrsahead$city, pattern ="/", names = c('a', 'b'))[,2]
  df_interactions_48hrsahead<-data.frame(df_interactions_48hrsahead)
  df_interactions_48hrsahead<-df_interactions_48hrsahead[,-1]
  df_interactions_48hrsahead$Days.in.timeslot__from<-strftime(as.Date(df_interactions_48hrsahead$timeslot__task),format="%m/%d/%y")
  df_interactions_48hrsahead$timeslotfromasstringhours_localtime<-strftime(df_interactions_48hrsahead$timeslot__task,format="%H:%M")
  df_interactions_48hrsahead$timeslot__task<-NULL
  return(df_interactions_48hrsahead)
}



