# ----------------------------------------------------------------------- #
# DEMAND PREDICTION PIPELINE - Hourly models
# Antoine - Version 2.0, 11.07.2016
# ----------------------------------------------------------------------- # 
# Description:
#   - FIRST, the data is transformed to a bi-hourly timeframe and 
#           NORMALIZE the data according to the daily level
#   - THEN, it TRAINS the hourly models and perform FORECASTING for each individual weekday
# ----------------------------------------------------------------------- # 
require(dplyr)
require(dummies)
require(forecast)

intra_day_learning <- function(inter_hourly, dates, district, inter_daily) {
  
  # ----------------------------------------------------------------------- # 
  # a. format the time frame (and data) on bi-hourly basis -----
  # combine the 30min timeslots in 1h
  #inter_hourly <- inter_hourly[, which(!colnames(inter_daily) %in% c("To"))]
  inter_hourly <- inter_hourly[, which(!colnames(inter_hourly) %in% "Cooperations" )]
  inter_hourly$From <- gsub(":30", ":00", inter_hourly$From)
  inter_hourly <- aggregate(formula = . ~ Date + From, data = inter_hourly, FUN = sum)
  
  # Add zeros for times when closed to have consistent frequency !!
  inter_hourly <- complete_hours(inter_hourly)
  
  # tidy the days' profiles
  inter_hourly_norm <- normalize_profiles(inter_hourly, inter_daily, method = "MinMax")
  inter_hourly_norm[is.na(inter_hourly_norm)] <- 0
  
  # aggregate by 2 hours
  inter_hourly_norm <- aggregate_odd_hours(inter_hourly_norm, method = sum)
  
  # sort by increasing date and hour
  inter_hourly_norm <- arrange(inter_hourly_norm, Date, From)
  

  
  # # ----------------------------------------------------------------------- # 
  # # b. train a model for each weekday ------
   inter_hourly_norm <- mutate(inter_hourly_norm, 
                              Weekdays = weekdays(Date))
   all_weekdays <- unique(inter_hourly_norm$Weekdays)
   all_froms <- unique(inter_hourly_norm$From)
   it<-data.table(inter_hourly_norm)
   sdcols<-names(inter_hourly_norm)
   #sdcols<-sdcols[sdcols!="Date" & sdcols!="From" & sdcols!="Weekdays"]
   sdcols<-district
   day_profiles<-it[,lapply(.SD,mean),by=.(Weekdays,From),.SDcols=sdcols]
   
  # 
  # # initilaize list to store the results
  # ts_hourly_train <- list()
  # ts_hourly_test <- list()
  # day_profiles <- list()
  # 
  # for (weekday in all_weekdays) {
  #   print(weekday)
  #   idx_rows <- which(inter_hourly_norm$Weekdays %in% weekday)
  #   
  #   # ------------------------------------ # 
  #   # transform in time series
  #   ts_inter_hourly_norm <- ts(data = inter_hourly_norm[idx_rows, district], 
  #                              end = 0, 
  #                              frequency = length(all_froms))
  #   ts_hourly_train[[weekday]] <- window(ts_inter_hourly_norm, 
  #                                    start = time(ts_inter_hourly_norm)[1], end = 0)
  #   # ts_hourly_train[[weekday]] <- window(ts_inter_hourly_norm, 
  #   #                                      start = time(ts_inter_hourly_norm)[1], end = -1)
  #   # ts_hourly_test[[weekday]] <- window(ts_inter_hourly_norm, 
  #   #                                     start = -0.99, end = 0)
  #   
  #   # ------------------------------------ #
  #   # train the model
  # 
  #   forecast_model <- auto.arima(ts_hourly_train[[weekday]], seasonal = TRUE)
  #   # ------------------------------------ #
  #   # forecast and store the profiles
  #   day_profiles[[weekday]] <- forecast(forecast_model, h = length(all_froms), level=c(80,95))
  #   # print(Box.test(residuals(forecast_model), lag = length(all_froms), fitdf = 3, type="Ljung-Box"))
  #   
  #   # graphs
  #   # plot_prediction(day_profiles[[weekday]], ts_hourly_train[[weekday]], ts_hourly_test[[weekday]], string_title = weekday)
  # }
  # 
  # return(list(day_profiles = day_profiles, 
  #             hours = all_froms, 
  #             ts_hourly_test = ts_hourly_test,
  #             inter_hourly = inter_hourly)
  # )
  return(list(day_profiles = day_profiles, 
              hours = all_froms, 
             
              inter_hourly = inter_hourly)
  )
}

