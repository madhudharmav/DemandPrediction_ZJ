
utilrate_format_driv_london<-function(table_10d){
table_10d_l<-table_10d[,c(1:8),with=FALSE]
table_10d_l_ceil<-ceiling(table_10d_l)
table_10d_l_ceil$`North East`[table_10d_l_ceil$`North East`<2]<-2
table_10d_l_ceil$`West (new)`[table_10d_l_ceil$`West (new)`<2]<-2
table_10d_l_ceil$South[table_10d_l_ceil$South<2]<-2

util_perc<-100*(rowMeans(table_10d_l,na.rm=TRUE)/rowMeans(table_10d_l_ceil,na.rm=TRUE))
table_10d_l_ceil_old<-table_10d_l_ceil

utilrate_range<-c(70,80)
for(i1 in c(1:nrow(table_10d))){


while (util_perc[i1]<utilrate_range[1] & !is.na(util_perc[i1])){
  clusno<-which.min(table_10d_l[i1,]-floor(table_10d_l[i1,]))
  if( !(names(table_10d_l_ceil)[clusno] %in% c("North East","South","West (new)"))){
    if(table_10d_l_ceil[i1,clusno,with=FALSE]>1){
      table_10d_l_ceil[i1,clusno:=.SD-1,.SDcols=(clusno),with=FALSE]
     }
  }else if(table_10d_l_ceil[i1,clusno,with=FALSE]>2){
    table_10d_l_ceil[i1,clusno:=.SD-1,.SDcols=(clusno),with=FALSE]
  }
  if(identical(table_10d_l_ceil_old[i1,],table_10d_l_ceil[i1,])){break}
  table_10d_l_ceil_old[i1,]<-table_10d_l_ceil[i1,]
  util_perc[i1]<-100*(rowMeans(table_10d_l[i1,],na.rm=TRUE)/rowMeans(table_10d_l_ceil[i1,],na.rm=TRUE))
} 
  
 
while(util_perc[i1]>utilrate_range[2] & !is.na(util_perc[i1])){
  clusno<-which.max(table_10d_l[i1,]-floor(table_10d_l[i1,]))
  table_10d_l_ceil[i1,clusno:=.SD+1,.SDcols=(clusno),with=FALSE]
  util_perc[i1]<-100*(rowMeans(table_10d_l[i1,],na.rm=TRUE)/rowMeans(table_10d_l_ceil[i1,],na.rm=TRUE))
}
}
return(table_10d_l_ceil)
}

