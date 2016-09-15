
utilrate_format_driv_berlin_smalls<-function(table_10d){
table_10d_l<-table_10d[,c(12:14),with=FALSE]
table_10d_l_ceil<-ceiling(table_10d_l)
util_perc<-100*(rowMeans(table_10d_l,na.rm=TRUE)/rowMeans(table_10d_l_ceil,na.rm=TRUE))
table_10d_l_ceil_old<-table_10d_l_ceil

utilrate_range<-c(70,80)
for(i1 in c(1:nrow(table_10d))){


while (util_perc[i1]<utilrate_range[1] & !is.na(util_perc[i1])){
  clusno<-which.min(table_10d_l[i1,]-floor(table_10d_l[i1,]))
  if(table_10d_l_ceil[i1,clusno,with=FALSE]>1){
    table_10d_l_ceil[i1,clusno:=.SD-1,.SDcols=(clusno),with=FALSE]
  } 
  util_perc[i1]<-100*(rowMeans(table_10d_l[i1,],na.rm=TRUE)/rowMeans(table_10d_l_ceil[i1,],na.rm=TRUE))
  if(identical(table_10d_l_ceil_old[i1,],table_10d_l_ceil[i1,])){break}
  table_10d_l_ceil_old[i1,]<-table_10d_l_ceil[i1,]
} 
 
while(util_perc[i1]>utilrate_range[2] & !is.na(util_perc[i1])){
  clusno<-which.max(table_10d_l[i1,]-floor(table_10d_l[i1,]))
  table_10d_l_ceil[i1,clusno:=.SD+1,.SDcols=(clusno),with=FALSE]
  util_perc[i1]<-100*(rowMeans(table_10d_l[i1,],na.rm=TRUE)/rowMeans(table_10d_l_ceil[i1,],na.rm=TRUE))
}
}
return(table_10d_l_ceil)
}

