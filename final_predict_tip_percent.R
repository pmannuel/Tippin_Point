library(ggplot2)
library(reshape2)
library(GPArotation)
library(psych)
library(lubridate)
library(plyr)
library(ggpubr)
library(corrplot)
library(lattice)
library(Amelia)
library(tidyr)
library(fitdistrplus)
library(bst)
library(MASS)
library(caret)
library(randomForest)

final_predict_tip_percent <- function(train,newdata,seed.in){
  # Set seed to ensure reproducibility between runs
  set.seed(seed.in)
  
  clean <- function(df){
    
    names(df) <- tolower(names(df))
    
    ######################
    # fix missing values
    ######################
    
    #drop Ehail_fee because there is too much missing values
    df <- subset(df, select = -c(ehail_fee))
    
    #replace 4 missing values in trip_type with mode
    df[is.na(df$trip_type),c("trip_type")] <- as.numeric(names(sort(-table(df$trip_type)))[1])
    
    ######################
    # all variables that should not be negative have their absolute values
    ######################
    df[,11:18] <- lapply(df[,11:18], abs)
    
    ######################
    # cast variables in the correct range of levels
    ######################
    #RateCodeId should be int between 1 and 6
    df[df$ratecodeid>6 | df$ratecodeid<1,5] = as.numeric(names(sort(-table(df[df$ratecodeid<7 | df$ratecodeid>0,5]))[1]))
    
    #Extra should only be 0.50 or 1 or 0
    df[df$extra!=1 & df$extra!=0 & df$extra!=0.5 ,c("extra")] = 0
    
    #Fare and total amount should be minimum $2.50
    df[df$fare_amount<2.50, c("fare_amount")] = 2.50
    
    total_amount_sum = c("fare_amount","extra","mta_tax","tolls_amount","improvement_surcharge")
    df[df$total_amount<2.50, c("total_amount")] = rowSums(df[df$total_amount<2.50, total_amount_sum])
    
    #Based on highest fare amount of $580.50, the max trip distance (best case) possible is 231.20 miles
    df[df$trip_distance>231.20,c("trip_distance")] = 231.20
    
    ######################
    #cast variables to correct class
    ######################
    df$vendorid <- as.factor(df$vendorid)
    
    df$lpep_pickup_datetime <- as.POSIXct(df$lpep_pickup_datetime,format='%Y-%m-%d %H:%M:%S')
    df$lpep_dropoff_datetime <- as.POSIXct(df$lpep_dropoff_datetime,format='%Y-%m-%d %H:%M:%S')
    
    classes <- c("character",
                 "character","numeric","numeric","numeric",
                 "numeric","integer","numeric","numeric",
                 "numeric","numeric","numeric","numeric",
                 "numeric","numeric","character","character")
    
    df[4:ncol(df)] <- Map(`class<-`, df[4:ncol(df)], classes)
    
    #cast chr to factors
    df <- as.data.frame(unclass(df))
    
    return(df)
  }
  
  engineer.feature <- function(df){
    
    ##### drop columns #####
    df <- subset(df, select = -c(store_and_fwd_flag, dropoff_latitude, dropoff_longitude))
    
    ##### time variables #####
    df$pickup_hr <- hour(df$lpep_pickup_datetime)
    df$pickup_hr <- as.factor(df$pickup_hr)
    
    df$dropoff_hr <- hour(df$lpep_dropoff_datetime)
    df$dropoff_hr <- as.factor(df$dropoff_hr)
    
    df$week <- week(df$lpep_pickup_datetime)
    df$week <- as.factor(df$week)
    df$week <- factor(df$week, levels=levels(df$week), labels=seq(1,5))
    
    
    ##### trip duration in min #####
    df$duration_min <- floor(as.double(df$lpep_dropoff_datetime - df$lpep_pickup_datetime)/60.0) #in min
    
    
    ##### ave speed in mph #####
    df$speed_mph    <- df$trip_distance/df$duration * 60 #in mph
    speed_errors <- which(df$speed_mph > 106 | df$speed_mph < 15 | is.nan(df$speed_mph))
    df[speed_errors,c("speed_mph")] <- NA_character_
    df$speed_mph <- as.numeric(df$speed_mph)
    speed.mis <- df[,-c(1:6)]
    speed.mis <- speed.mis[,-c(10:14)]
    speed_idx <- which(colnames(speed.mis)=="speed_mph")
    speed_bound <- t(as.matrix(c(speed_idx,15,240)))
    amelia_fit <- amelia(speed.mis, m=1, idvars=range(1,10), parallel = "multicore", p2s=0, bound = speed_bound)
    df$speed_mph <- amelia_fit$imputations$imp1$speed_mph
    
    
    ##### is airport trip #####
    df$is_airport_trip <- df$ratecodeid
    df$is_airport_trip <- factor(df$is_airport_trip, levels=c(1,2,3,4,5,6), labels=c("0", "1", "1","0","0","0"))
    df$is_airport_trip <- as.character(df$is_airport_trip) #drop excess levels
    df$is_airport_trip <- factor(df$is_airport_trip)
    
    
    ##### with tip ######
    df$with_tip <- as.factor(as.character(as.numeric(df$tip_amount>0)))
    
    
    ##### tip percent #####
    df$percent  <- df$tip_amount/df$total_amount * 100
    
    ##### drop columns #####
    df <- subset(df, select = -c(lpep_dropoff_datetime, lpep_pickup_datetime, pickup_longitude, pickup_latitude))
    
    return(df)
  }
  
  fit.randomf <- function(train){
    # Set seed to ensure reproducibility between runs
    set.seed(711)
    
    # Subset the features we want to use
    features <- c("with_tip", "payment_type", "total_amount", "trip_distance", "duration_min",
                  "speed_mph", "pickup_hr", "extra", "is_airport_trip")
    
    # Set up caret to perform 5-fold cross validation repeated 3 times
    caret.control <- trainControl(method = "repeatedcv",
                                  number = 5,
                                  repeats = 3)
    
    # Use caret to train a Random Forest using 5-fold cross 
    # validation repeated 3 times and use 5 values for tuning the
    # mtry parameter. Use 120 trees as our data is small.
    rf.cv <- train(with_tip ~ ., 
                   data = train[, features],
                   method = "rf",
                   trControl = caret.control,
                   tuneLength = 5,
                   ntree = 120, 
                   importance = TRUE)
    
    # Display the results of the cross validation run
    print(rf.cv)
    
    # Pull out the the trained model using the best parameters on all data
    rf.best <- rf.cv$finalModel
    varImpPlot(rf.best)
    
    return(rf.cv)
  }
  
  fit.regress <- function(train){
    # Set seed to ensure reproducibility between runs
    set.seed(711)
    
    # Subset the features we want to use
    features <- c("percent", "trip_distance", "duration_min", "speed_mph", "total_amount")
    
    # Set up caret to perform 5-fold cross validation repeated 3 times
    caret.control <- trainControl(method = "cv",
                                  number = 3)
    
    grid <- expand.grid(n.trees = seq(1,3,1), interaction.depth = 2, shrinkage = .1, n.minobsinnode = 20)
    
    # Use bst to train a Gradient Boosting using 5-fold cross 
    # validation repeated 3 times. and use 5 values for tuning the
    # mtry parameter.
    tip.bst.model <- train(percent ~ ., 
                           data = train[, features], 
                           method = 'gbm', 
                           trControl = caret.control,
                           tuneGrid=grid)
    plot(tip.bst.model)
    
    print(tip.bst.model)
    
    return(tip.bst.model)
  }
  
  print("Finished loading all functions")
  
  print("Cleaning training data and building new features")
  df.clean <- clean(train)
  train <- engineer.feature(df.clean)
  
  print("Cleaning new data and building new features")
  newdata <- clean(newdata)
  newdata <- engineer.feature(newdata)
  
  tip.cf.model <- fit.randomf(train)
  print("Finished fitting classification model")
  
  tip.rg.model <- fit.regress(train)
  print("Finished fitting regression model")
  
  print("Predicting (1)tip (0)no tip ...")
  newdata$pred_with_tip <- predict(tip.cf.model, newdata,  type = "raw")
  
  print("Predicting percentage of tip ...")
  newdata$pred_tip_percent <- 0
  newdata[newdata$pred_with_tip==1,]$pred_tip_percent <- predict(tip.rg.model, newdata[newdata$pred_with_tip==1,])
  
  # Write out a .CSV suitable for Kaggle submission
  write.csv(newdata, file = "output.csv", row.names = FALSE)
  
  return(newdata)
}
