# ----------------------------------------------------------------------- #
# DEMAND PREDICTION PIPELINE - format the data frames
# Antoine - Version 2.0, 11.07.2016
# ----------------------------------------------------------------------- # 
# Description:
#     this script contains 2 types of functions.
# 
# 1. general functions for formatting that can be called by all other scripts
# 
# 2. specific functions for formatting individual data frames, 
#     regarding interactions, idle time, nb of drivers ...
# ----------------------------------------------------------------------- # 
require(dplyr)
require(dummies)


# ----------------------------------------------------------------------- # 
# 1. General functions -----
# -------------------------------- # 
# transform a factor into columns and associate the values to it
valumy <- function(df, name_col_factor, name_col_value){
    # list all values of the factor
    all_factors <- unique(df[, c(name_col_factor)])
    
    # create dummy indexes df for each value of the factor
    dummy_factors <- dummy.data.frame(data = df, names = name_col_factor,omit.constants = FALSE)
    colnames(dummy_factors) <- gsub(name_col_factor, "", colnames(dummy_factors))
    
    # update the values
    for (i_factor in all_factors) {
        dummy_factors[, c(i_factor)] <- dummy_factors[, c(i_factor)]*dummy_factors[, name_col_value]
    }
    return(dummy_factors)
}

# -------------------------------- # 
# take care of the change of hour fron summer time to winter time !
summer_time_fix <- function(df, col_date = "Date", col_hour = "Hour",
                            start_summer = "2016-03-27", end_summer = "2016-10-30") {
    require(dplyr)
    # format as character the hour
    df[, col_hour] <- as.vector(df[, col_hour])
    # add one hour for all hours between the two dates
    idx_dates <- which(df[, col_date] >= start_summer & df[, col_date] <= end_summer)
    # get the hour from df
    hours <- unlist(
        strsplit(df[idx_dates, col_hour], ":"))[seq(1, 2*length(df[idx_dates, col_hour]), 2)]
    hours <- as.numeric(hours) + 1 # add one hour in the summer season
    # get the minutes
    minutes <- unlist(
        strsplit(df[idx_dates, col_hour], ":"))[seq(2, 2*length(df[idx_dates, col_hour]), 2)]
    # put back in the df
    df[idx_dates, col_hour] <- paste(hours, minutes, sep = ":")
    return(df)
}

# -------------------------------- # 
# change from hourly to bi-hourly format
one2two_hour_bins <- function(df_inter_hourly, method = sum){
    # combine the 30min timeslots in 1h
    df_inter_hourly <- df_inter_hourly[, which(!colnames(df_inter_hourly) %in% "To")]
    df_inter_hourly$From <- gsub(":30", ":00", df_inter_hourly$From)
    df_inter_hourly <- aggregate(formula = . ~ Date + From, data = df_inter_hourly, FUN = sum)
    
    # Add zeros for times when closed to have consistent frequency !!
    df_inter_hourly <- complete_hours(df_inter_hourly)
    
    # aggregate by 2 hours
    df_inter_hourly <- aggregate_odd_hours(df_inter_hourly, method = method)
    
    # sort by increasing date and hour
    df_inter_hourly <- arrange(df_inter_hourly, Date, From)
    return(df_inter_hourly)
}

# -------------------------------- # 
# add zeros for closed hours to have a consistent frequency
complete_hours <- function(df_hourly) {
    # create all_hours
    all_hours <- format(as.POSIXct(as.character(0:23), format = "%H"), format = "%H:%M")
    # create an empty df_hourly with full hours
    new_df_hourly <- expand.grid(Date = sort(unique(df_hourly$Date)), 
                                 From = all_hours, stringsAsFactors = FALSE)
    # left join to fill in all the known hours
    new_df_hourly <- left_join(new_df_hourly, df_hourly)
    
    # replace NAs by 0
    new_df_hourly[is.na(new_df_hourly)] <- 0
    
    return(new_df_hourly)
}

# -------------------------------- # 
# normalize the hourly data by the daily sum
normalize_profiles<- function(df_hourly, df_daily, method = "MinMax"){
    all_districts <- colnames(df_hourly)[which(!colnames(df_hourly) %in% c("Date", "From", "To", "Weekdays"))]
    # loop on days
    for (day in unique(df_hourly$Date)){
        for (district in all_districts) {
            # divide the hourly data for one day, by the sum for that day
            df_hourly[df_hourly$Date == day, 
                      district] <- df_hourly[df_hourly$Date == day, district] / as.numeric(df_daily[df_daily$Date == day, district])
        }
    }
    return(df_hourly)
}


# -------------------------------- # 
# aggregate to get 2hour bins
aggregate_odd_hours <- function(df, method = sum, agg = TRUE){
    # get the hour from the time
    df$From <- format(as.POSIXct(df$From, format = "%H:%M"), format = "%H")
    df$From <- as.numeric(df$From)
    # find the odd hour and remove 1 hour from them
    # df$From <- ifelse(df$From %% 2 , 
    #                   df$From, 
    #                   df$From - 1)
    df$From <- ifelse(df$From %% 2 , 
                      df$From-1, 
                      df$From)
    # transform to time format
    df$From <- format(as.POSIXct(as.character(df$From), format = "%H"), format = "%H:%M")
    # aggregate by date and hours
    if (agg) {
        df <- aggregate(formula = . ~ Date + From, data = df, FUN = method)
    }
    df <- arrange(df, Date, From)
    return(df)
}








# ----------------------------------------------------------------------- # 
# 2. specific functions -----

# -------------------------------- # 
# ... interactions
# !! for CW before 18 (excluded)
format_interaction <- function(df_interactions_raw, dates, daily = FALSE){
    df_interactions <- df_interactions_raw
    # formating dates
    df_interactions$Date <- as.Date(df_interactions$Date, format = "%d.%m.%y")
    # filter on the relevant dates
    df_interactions <- df_interactions[df_interactions$Date >= dates[1] & 
                                           df_interactions$Date <= dates[2],]
    # aggregate the data if daily = TRUE
    if (daily){
        df_interactions <- df_interactions[,-which(colnames(df_interactions) == "Hour")]
        df_interactions <- aggregate(data = df_interactions, . ~ Date, sum)
    }
    return(df_interactions)

}

# !! for CW after 18 (included)
format_interaction_2 <- function(df_interactions_raw, dates, daily = FALSE){
    df_interactions <- df_interactions_raw
    
    # change the name
    colnames(df_interactions) <- c("Date", "From", "To", "City", "Cluster", "PU", "DO")
    
    # replace NAs by 0s
    df_interactions$PU[is.na(df_interactions$PU)] <- 0
    df_interactions$DO[is.na(df_interactions$DO)] <- 0
    
    # calculate the nb of interaction per timeslot
    df_interactions <- mutate(df_interactions, Interaction = PU + DO)
    
    # format
    df_interactions$Date <- as.Date(df_interactions$Date, format = "%m/%d/%y")
    
    # filter on the dates
    df_interactions <- df_interactions[df_interactions$Date >= dates[1] & 
                                           df_interactions$Date <= dates[2],]
    
    # expand by city
    df_new <- valumy(df_interactions, "City", "Interaction")
    
    # expand by cluster
    df_new <- valumy(df_new, "Cluster", "Interaction")
    
    #... aggregate per day and timeslots
    df_new <- aggregate(data = df_new, . ~ Date + From + To, sum)
    df_new <- df_new[, which(!colnames(df_new) %in% c("To"))]
    
    # aggregate the data if daily = TRUE
    if (daily){
        # TODO : use select instead of hard code
        # df_new <- aggregate(data = select(df_new, -one_of(c("From", "To")) ), . ~ Date, sum)
        df_new <- aggregate(data = df_new[, which(!colnames(df_new) %in% c("From", "To"))], 
                            . ~ Date, sum)
    }
    
    # drop the PU, DO and Interaction columns
    df_new <- df_new[, which(!colnames(df_new) %in% c("PU", "DO", "Interaction", "To"))]
    
    return(df_new)
    
}



# -------------------------------- # 
# ... holidays
format_holidays <- function(holidays_raw, dates){
    holidays <- holidays_raw
    # rename the columns
    # colnames(holidays) <- c("Date", "London", "Central", 
    #                         "North.East", "Southwark", "Victoria", 
    #                        "West", "South.West", "Berlin", 
    #                        "Berlin.Ost", "Westen")
    colnames(holidays) <- c("Date", "London", "Berlin")
    # format dates
    holidays$Date <- as.Date(holidays$Date, format = "%d.%m.%Y")
    # filter on relevant dates
    holidays <- holidays[holidays$Date >= dates[1] & 
                             holidays$Date <= dates[2], ]
    return(holidays)
}



# -------------------------------- # 
# ... idle_time
format_idletime <- function(idle_time_raw, dates, daily = FALSE){
    require(dplyr)
    idle_time <- idle_time_raw
    idle_time <- dplyr::select(idle_time,
                               Date = Days.in.forecast__completedAt__date,
                               From = destination_from_hours,
                               To = destination_to_hours,
                               City = destination__externalId,
                               Cluster = fleet__name,
                               Idle_time = final_idletime) %>%
        mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
        dplyr::filter(Date >= dates[1] & 
                   Date <= dates[2])
    idle_time$City<-substr(idle_time$City,1,2)
    idle_time$City[idle_time$City=="DE"]<-"Berlin"
    idle_time$City[idle_time$City=="GB"]<-"London"
    # expand by city
    idle_time <- valumy(idle_time, "City", "Idle_time")
    
    # expand by cluster
    idle_time <- valumy(idle_time, "Cluster", "Idle_time")
    
    #... aggregate per day and timeslots
    idle_time <- aggregate(data = idle_time, . ~ Date + From + To, sum)
    idle_time <- idle_time[, which(!colnames(idle_time) %in% c("To", "Idle_time"))]
    
    # aggregate the data if daily = TRUE
    if (daily){
        idle_time <- aggregate(data = idle_time[, which(!colnames(idle_time) %in% c("From", "To"))], 
                               . ~ Date, 
                               sum)
    }
    return(idle_time)
}

format_idletime_2 <- function(idle_time_raw, dates, daily = FALSE) {
    require(dplyr)
    idle_time <- idle_time_raw %>%
        transmute(Date = as.Date(slot, format = "%Y-%m-%d"),
                  From = localHoursFrom,
                  City = as.factor(city),
                  Cluster = as.factor(fleet),
                  idletime = ifelse(is.na(time),
                                    0,
                                    time)) %>%
        filter(Date >= dates[1] & 
                   Date <= dates[2]) %>% 
        # /!\ take care of non plain hour by just setting "From" to "xx:00" format
        mutate(From = gsub(":..", ":00", From)) 
    # /!\
    
    # expand by city
    idle_time <- valumy(df = idle_time,
                        name_col_factor = "City", 
                        name_col_value = "idletime")
    # expand by cluster
    idle_time <- valumy(df = idle_time,
                        name_col_factor = "Cluster", 
                        name_col_value = "idletime")
    # get rid of the total idle time and aggregate to remove the duplicate
    if(! daily) {
        idle_time <- aggregate(data = dplyr::select(idle_time, -idletime),
                               . ~ Date + From, 
                               sum) %>%
            arrange(Date, From)
    } else {
        idle_time <- aggregate(data = dplyr::select(idle_time, -idletime, -From),
                               . ~ Date, 
                               sum) %>%
            arrange(Date)
    }
    return(idle_time)
}



# -------------------------------- # 
# ... nb of drivers per hour
format_nb_drivers <- function(nb_driver_raw, dates, district) {
    # calculate the nb of drivers bi-hourly
    nb_driver <- transmute(nb_driver_raw, 
                           Date = as.Date(Days.in.forecast__completedAt__date, 
                                          format = "%m/%d/%y"),
                           From = gsub(":30", ":00", destination_from_hours),
                           To = destination_to_hours,
                           Driver = as.character(courier__name),
                           Cluster = as.character(fleet__name)) %>% 
      dplyr::filter(Date > dates[1] & Date <= dates[2] & Cluster == district) %>% 
        dplyr::select(-To, -Cluster) %>% 
        group_by(Date, From) %>%
        dplyr::summarise(nb_driver = n_distinct(Driver)) %>%
        ungroup() 
    
    nb_driver <- as.data.frame(nb_driver) 
    nb_driver <- one2two_hour_bins(nb_driver, method = max) 
    nb_driver <- mutate(nb_driver,
                        Weekday = factor(weekdays(Date), 
                                         levels = c("Monday", "Tuesday", "Wednesday", 
                                                    "Thursday", "Friday", "Saturday", "Sunday"))
    )
    # # prepare the interactions data to be joined
    # inter_hourly <- dplyr::select(inter_hourly, 
    #                               Date, 
    #                               From, 
    #                               Interaction = one_of(district)) %>%
    #     filter(Date > dates[1] & 
    #                Date <= dates[2])
    # inter_hourly <- one2two_hour_bins(inter_hourly, method = sum) 
    # 
    # # join with the nb of interactions 
    # nb_driver <- left_join(x = inter_hourly,
    #                        y = nb_driver)
    return(nb_driver)
}



# -------------------------------- # 
# ... marketing
# format_marketing <- function(df_marketing_raw, dates, features = NULL){
#     df_marketing <- df_marketing_raw
#     # format dates
#     df_marketing$date <- as.Date(df_marketing$date, format = "%m/%d/%y")
#     # filter on the relevant dates
#     df_marketing <- df_marketing[df_marketing$Date >= dates[1] & 
#                                      df_marketing$Date <= dates[2],]
#     # select the relevant features
#     if (is.null(features)){
#         df_marketing <- df_marketing[,which(colnames(df_marketing) %in% 
#                                                 c("sea_clicks","facebook_spend","emails_sent"))] #"date",
#     } else {
#         df_marketing <- df_marketing[,which(colnames(df_marketing) %in% features)] #"date",
#     }
#     return(df_marketing)
# }
# 
# format_marketing_2 <- function(df_marketing_raw, dates, features = NULL){
#     df_marketing <- df_marketing_raw
#     # change the names
#     colnames(df_marketing) <- c("Date", "Channel", "Orders")
#     # format dates
#     df_marketing$date <- as.Date(df_marketing$date, format = "%m/%d/%y")
#     # filter on the relevant dates
#     df_marketing <- df_marketing[df_marketing$Date >= dates[1] & 
#                                      df_marketing$Date <= dates[2],]
#     # expand the Channel factor
#     df_marketing <- valumy(df_marketing, "Channel", "Orders")
#     
#     # select the relevant features
#     if (is.null(features)){
#         df_marketing <- df_marketing[,which(colnames(df_marketing) %in% 
#                                                 c("SEM","SEM","Facebook","Twitter"))] #"date",
#     } else {
#         df_marketing <- df_marketing[,which(colnames(df_marketing) %in% features)] #"date",
#     }
#     return(df_marketing)
# }

# -------------------------------- # 
# ... customers
# format_customers <- function(df_customers_raw, dates){
#     df_customers <- df_customers_raw 
#     # replace NAs by 0s
#     df_customers[is.na(df_customers$New), "New"] <- 0
#     df_customers[is.na(df_customers$Returning), "Returning"] <- 0
#     # rename the columns
#     colnames(df_customers) <- c("Date", "Valid.Order", "Total.Order", "Cancel.Rate", "New", "Returning")
#     # format dates
#     df_customers$Date <- as.Date(df_customers$Date, format = "%m/%d/%y")
#     # filter the dates
#     df_customers <- df_customers[df_customers$Date >= dates[1] & 
#                                      df_customers$Date <= dates[2], ]
#     # select useful columns
#     df_customers <- df_customers[, c("Date", "Cancel.Rate", "New", "Returning")]
#     
#     return(df_customers)
# }






