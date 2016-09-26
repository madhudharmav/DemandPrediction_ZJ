predictions_cluster_V3p2_diff_noit<-function( district,testing_days,daily_interactions,dates,daily_interactions_48hrsahead,daily_interactions_test_48hrsahead){

#daily_model <- "Arimax"

# Forecasting the number of interactions on a daily basis ------

# formatting as a time serie and analysis

daily_interactions_diff<-daily_interactions[, district,with=FALSE]-daily_interactions_48hrsahead[, district,with=FALSE]
ts_training <- ts(data = daily_interactions_diff, end = 0, frequency = 7)
ts_training[is.na(ts_training)]<-0
lamd<-BoxCox.lambda(ts_training, method=c("guerrero"), lower=-1, upper=2)
#print(lamd)
# p<-try(daily_model <- Arima(ts_training,
#                             order = c(2,1,0),
#                             seasonal = list(order = c(2,1,0), period = 7),lambda = lamd),silent = TRUE
# )
# if(class(p)=="try-error"){
#   print("init value problem")
#   daily_model <- Arima(ts_training,
#                        order = c(2,1,0),
#                        seasonal = list(order = c(2,1,0), period = 7,lambda = lamd),method="CSS")}
daily_model <-auto.arima(ts_training,lambda = lamd,seasonal = TRUE)
# forecast

CI<-80
daily_forecasts <- forecast(daily_model , h = testing_days,  level = CI)
df_prediction<-as.numeric(daily_forecasts$mean)+daily_interactions_test_48hrsahead[, district,with=FALSE]

return(df_prediction)
}