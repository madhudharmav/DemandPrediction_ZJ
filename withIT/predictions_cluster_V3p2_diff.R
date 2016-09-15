predictions_cluster_V3p2_diff<-function( idle_time,district,testing_days,daily_interactions,dates,hourly_interactions,daily_interactions_48hrsahead,daily_interactions_test_48hrsahead){


daily_model <- "Arimax"
###########################################


# 4. Forecasting the number of interactions on a daily basis ------

# formatting as a time serie and analysis

daily_interactions_diff<-daily_interactions[, district]-daily_interactions_48hrsahead[, district]
ts_training <- ts(data = daily_interactions_diff, end = 0, frequency = 7)
#############
# prepare the predictors (idle_time AND holiday)
# ... holidays

#holidays_raw <- read.csv(paste(path_data, "Holidays_Dummy.csv", sep="/"), sep=";", stringsAsFactors = FALSE)
#holidays <- format_holidays(holidays_raw, dates)
# parse them as ts
#ts_holidays <- ts(data = holidays[, city], end = 0, frequency = 7)
#ts_holidays_train <- window(ts_holidays, start = time(ts_interaction)[1], end = 0)
#ts_holidays_test <- window(ts_holidays, start = -(testing_days-1)/7,end = 0)


# ... idle_time


ts_idle_time_train<-ts(idle_time[, district], end = 0, frequency = 7)
idletime_model<-auto.arima(ts_idle_time_train)
ts_idle_time_test <-forecast(idletime_model , h = testing_days)
ts_idle_time_test<-round(ts_idle_time_test$mean)
# train
xreg = matrix(data = c(ts_idle_time_train),ncol = 1)
daily_model <- Arima(ts_training,
                     xreg=xreg,
                     order = c(2,1,0),
                     seasonal = list(order = c(2,1,0), period = 7)
                     )

# forecast
# xreg = matrix(data = c(ts_idle_time_test,ts_holidays_train),ncol = 2)
# xreg<-seasonaldummy(ts(ts_testing,f=7))
xreg = matrix(data = c(ts_idle_time_test),ncol = 1)
CI<-80
daily_forecasts <- forecast(daily_model , h = testing_days,  xreg =xreg, level = CI)
daily_forecasts$mean<-daily_forecasts$mean+daily_interactions_test_48hrsahead[, district]
daily_forecasts$upper[, 1]<-daily_forecasts$upper[, 1]+daily_interactions_test_48hrsahead[, district]
daily_forecasts$lower[, 1]<-daily_forecasts$lower[, 1]+daily_interactions_test_48hrsahead[, district]

# ----------------------------------------------------------------------- #
# 5. Forecasting the bi-hourly percentages of interactions ----

# train the hourly and perform the forecasts
intra_day_pattern <- intra_day_learning(hourly_interactions, 
                                        dates, 
                                        district, 
                                        daily_interactions)




# ----------------------------------------------------------------------- #
# 6. Multiply both daily and hourly forecasts -----
test_dates <- (dates[2] + seq(1,testing_days , 1))
# df_prediction <- expand.grid(Date = test_dates, 
#                              From = format(as.POSIXct(as.character(seq(0,23,2)), format = "%H"), 
#                                            format = "%H:%M"))
df_prediction <- expand.grid(Date = test_dates, 
                             From = format(as.POSIXct(as.character(seq(0,22,2)), format = "%H"), 
                                           format = "%H:%M"))
df_prediction <- arrange(df_prediction, Date, From)
df_prediction$Weekday <- weekdays(df_prediction$Date)

# add the inter-day
df_prediction <- inner_join(x = df_prediction, 
                            y = data.frame(Date = test_dates, 
                                           Base = as.numeric(daily_forecasts$mean),
                                           Upper = as.numeric(daily_forecasts$upper[, 1]),
                                           Lower = as.numeric(daily_forecasts$lower[, 1])
                            ), 
                            by = "Date")

# add the intra-day
df_prediction$Density <- 0 
for (weekday in df_prediction$Weekday) {
  idx_rows <- which(df_prediction$Weekday == weekday)
  df_prediction[idx_rows, "Density"] <- intra_day_pattern$day_profiles[Weekdays==weekday,get(district)]
}
df_prediction <- mutate(df_prediction, 
                        Mean = Base * Density,
                        Upper = Upper * Density,
                        Lower = Lower * Density)

return(df_prediction)
}