# ----------------------------------------------------------------------- #
# DEMAND PREDICTION PIPELINE - Convertion of interactions into driver need
# Antoine - Version 2.0, 11.07.2016
# ----------------------------------------------------------------------- # 
# Description:
#   - FIRST, the data is transformed to a bi-hourly timeframe and 
#           NORMALIZE the data according to the daily level
#   - THEN, it TRAINS the hourly models and perform FORECASTING for each individual weekday
# ----------------------------------------------------------------------- # 


# ------------------------------ # 
# train the model to convert interactions into nb of drivers
train_converter <- function(method, df_train) {
    require(caret)
    # ------------------------ #
    if(method == "xgbTree") {
        require(xgboost)
        tr_ctrl <- trainControl(method = "repeatedcv",
                                number = 3,
                                repeats = 3)
        grid_parameters <- expand.grid(nrounds = seq(50, 150, 25),
                                       max_depth = seq(1, 5, 1),
                                       colsample_bytree = seq(0.7, 1, 0.1),
                                       eta = 0.3,
                                       gamma = 0,
                                       min_child_weight = 1)
        model <- train(nb_driver ~ 0 + Interaction + factor(Weekday) + factor(From),
                       trControl = tr_ctrl,
                       tuneGrid = grid_parameters,
                       data = df_train, 
                       method = "xgbTree",
                       verbose = 1)
    }
    
    # ------------------------ #
    if(method == "single_Tree") {
        require(rpart)
        require(rpart.plot)
        model <- train(nb_driver ~ Interaction + factor(Weekday) + factor(From),
                       data = df_train, 
                       method = "rpart")
    }
    
    # ------------------------ #
    if(method == "glm") {
        model <- glm(data = df_train,
                     nb_driver ~ 0 + Interaction + factor(Weekday) + factor(From))
    }
    return(model)
}



# ----------------------------------------------------------------------- #
# test - forecast
convert_next_interactions <- function(model, df_test) {
    return(predict(model, 
                   newdata = df_test))
}


