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

get_createddatebyref<-function(referenceids){
  fields_needed<-list( "createdAt" =1L)

  mongo <- mongo.create(host="172.31.51.215:27017", db="uk_live")
  mongo.is.connected(mongo)
  c_dates<-seq( as.POSIXct("2011-07-01"),by=0, len=length(referenceids))
 
  for(i1 in 1:length(referenceids)){
    
    ref_id<-referenceids[i1]
    if(substr(ref_id,nchar(ref_id)-1,nchar(ref_id))=="PU"){
    cursor<-mongo.find(mongo, "uk_live.intwash_driver_pickups",query=list("reference"= ref_id),  sort = mongo.bson.empty(),fields =fields_needed, options = 4L )
    doc<-mongo.cursor.to.list(cursor)
    p<-flatlist(doc)$createdAt
    l<-flatlist(doc)$forecast.completedAt.timezone
    }else if(substr(ref_id,nchar(ref_id)-1,nchar(ref_id))=="DO"){
    cursor<-mongo.find(mongo, "uk_live.intwash_driver_deliveries",query=list("reference"= ref_id),  sort = mongo.bson.empty(),fields =fields_needed, options = 4L )
    doc<-mongo.cursor.to.list(cursor)
    p<-flatlist(doc)$createdAt 
    l<-flatlist(doc)$forecast.completedAt.timezone
    }else{
      p<-NA
      l<-NA}
    c_dates[i1]<-p
   

  }
  mongo.destroy(mongo)
  
  #c_dates<-cbind.data.frame(referenceids,c_dates)
 return(c_dates) 
}