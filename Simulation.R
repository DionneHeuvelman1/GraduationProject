library(fpp2)
library(tictoc)


MSE <- function(observations, forecasts){
  if(length(observations)!=length(forecasts)){
    print("could not calculate MSE: lengths not equal")
    return(-1)
  }
  error <- observations - forecasts
  MSE <- (sum(error^2)/length((observations)))
  return(MSE)
}

residual_function <- function(observations, forecasts){
  if(length(observations)!=length(forecasts)){
    print("could not calculate MSE: lengths not equal")
    return(-1)
  }
  error <- observations - forecasts
  return(error)
}

Remove_outliers_impute_mean <- function(salesForecastHistory){
  forecastNumberOfWeeks <- length(salesForecastHistory)/7
  for(b in 1:7){
    salesPerDayOfTheWeek <- rep(0, forecastNumberOfWeeks)
    for (c in 1:forecastNumberOfWeeks){
      salesPerDayOfTheWeek[c] <- salesForecastHistory[7*(c-1)+b]
    }
    outliers <- salesPerDayOfTheWeek
    meanPerDay <- mean(salesPerDayOfTheWeek)
    sumNoOutliers <- 0
    for (c in 1:forecastNumberOfWeeks){
      if(salesPerDayOfTheWeek[c]>=(0.2*meanPerDay) & salesPerDayOfTheWeek[c]<=(1.8*meanPerDay)){
        sumNoOutliers <- sumNoOutliers + 1
        outliers[c] <- NA
      }
    }
    correctedMeanPerDay <- mean(outliers, na.rm = TRUE) 
    if (sumNoOutliers!=0 & sumNoOutliers!=forecastNumberOfWeeks){
      for (c in 1:forecastNumberOfWeeks){
        if(salesPerDayOfTheWeek[c]<(0.2*meanPerDay)){
          salesForecastHistory[7*(c-1)+b] <- correctedMeanPerDay
        } else if (salesPerDayOfTheWeek[c]>(1.8*meanPerDay)){
          salesForecastHistory[7*(c-1)+b] <- correctedMeanPerDay
        }
      }
    }
  }
  return(salesForecastHistory)
}

Correct_for_weekly_seasonality <- function(salesForecastHistory){
  forecastNumberOfWeeks <- length(salesForecastHistory)/7
  #1. Calculate mean sales per week
  salesPerWeek <- rep(0, forecastNumberOfWeeks)
  for (c in 1:forecastNumberOfWeeks){
    sumPerWeek <- 0
    numberOfElements <- 0
    salesOfOneWeek <- 0
    for (d in 1:7){
        sumPerWeek <- sumPerWeek + salesForecastHistory[d + (c-1)*7]
    }
    salesPerWeek[c] <- sumPerWeek/7
  }
  
  #2. Seasonality
  totalAverageSales <- mean(salesPerWeek)
  seasonalityPerWeek <- rep(0, forecastNumberOfWeeks)
  if (totalAverageSales==0){
    for (c in 1:forecastNumberOfWeeks){
      seasonalityPerWeek[c] <- 1
    }
  } else {
    for (c in 1:forecastNumberOfWeeks){
      seasonalityPerWeek[c] <- salesPerWeek[c]/totalAverageSales
    }
  }
  
  #3. Correct daily sales for seasonality
  correctedSalesForecastHistory <- salesForecastHistory
  for(b in 1:(forecastNumberOfWeeks*7)){
      correctedSalesForecastHistory[b] <- correctedSalesForecastHistory[b]/seasonalityPerWeek[ceiling(b/7)]
  }
  return(correctedSalesForecastHistory)
}

ARMA <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      #TimeSeriesTraining <- Remove_outliers_impute_mean(TimeSeriesTraining)
      #TimeSeriesTraining <- Correct_for_weekly_seasonality(TimeSeriesTraining)
      salesPerWeek.sarima <- Arima(TimeSeriesTraining, order=c(1,0,1), seasonal=list(order=c(1,1,1), period=7))#, method = "ML")
      salesPerWeek.sarima.fore <- forecast(salesPerWeek.sarima, h=7)
      ARMA_forecast <- as.matrix(salesPerWeek.sarima.fore$mean)
      return(ARMA_forecast)
    },
    error=function(cond) {
      # Choose a return value in case of error
      return(NULL)
    })
  return(out)
}

ARMA_remove_outliers <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      TimeSeriesTraining <- Remove_outliers_impute_mean(TimeSeriesTraining)
      #TimeSeriesTraining <- Correct_for_weekly_seasonality(TimeSeriesTraining)
      salesPerWeek.sarima <- Arima(TimeSeriesTraining, order=c(1,0,1), seasonal=list(order=c(1,1,1), period=7))#, method = "ML")
      salesPerWeek.sarima.fore <- forecast(salesPerWeek.sarima, h=7)
      ARMA_forecast <- as.matrix(salesPerWeek.sarima.fore$mean)
      return(ARMA_forecast)
    },
    error=function(cond) {
      # Choose a return value in case of error
      return(NULL)
    })
  return(out)
}

HW <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      #TimeSeriesTraining <- Remove_outliers_impute_mean(TimeSeriesTraining)
      #TimeSeriesTraining <- Correct_for_weekly_seasonality(TimeSeriesTraining)
      TimeSeries.hw <- HoltWinters(TimeSeriesTraining,seasonal="additive")
      #TimeSeries.hw <- ets(TimeSeriesTraining,model="AAA")
      TimeSeries.hw.fore <- forecast(TimeSeries.hw,h=7)
      HW_forecast <- as.matrix(TimeSeries.hw.fore$mean)
      return(HW_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

HW_remove_outliers <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      TimeSeriesTraining <- Remove_outliers_impute_mean(TimeSeriesTraining)
      #TimeSeriesTraining <- Correct_for_weekly_seasonality(TimeSeriesTraining)
      TimeSeries.hw <- HoltWinters(TimeSeriesTraining,seasonal="additive")
      #TimeSeries.hw <- ets(TimeSeriesTraining,model="AAA")
      TimeSeries.hw.fore <- forecast(TimeSeries.hw,h=7)
      HW_forecast <- as.matrix(TimeSeries.hw.fore$mean)
      return(HW_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

BOXHW <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      #TimeSeriesTraining <- Remove_outliers_impute_mean(TimeSeriesTraining)
      #TimeSeriesTraining <- Correct_for_weekly_seasonality(TimeSeriesTraining)
      minimalNonZero <- min(TimeSeriesTraining[TimeSeriesTraining>0])
      lambda2 <- minimalNonZero/2
      transform <- log(TimeSeriesTraining + lambda2)
      salesPerWeek.box.ts <- ts(transform, start=c(1,1),frequency=7)
      salesPerWeek.box.hw <- HoltWinters(salesPerWeek.box.ts,seasonal="additive")
      #salesPerWeek.box.hw <- ets(salesPerWeek.box.ts,model="AAA")
      salesPerWeek.box.hw.fore <- forecast(salesPerWeek.box.hw,h=7)
      BOXHW_forecast <- as.matrix(exp(salesPerWeek.box.hw.fore$mean) - lambda2)
      return(BOXHW_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

BOXHW_remove_outliers <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      TimeSeriesTraining <- Remove_outliers_impute_mean(TimeSeriesTraining)
      #TimeSeriesTraining <- Correct_for_weekly_seasonality(TimeSeriesTraining)
      minimalNonZero <- min(TimeSeriesTraining[TimeSeriesTraining>0])
      lambda2 <- minimalNonZero/2
      transform <- log(TimeSeriesTraining + lambda2)
      salesPerWeek.box.ts <- ts(transform, start=c(1,1),frequency=7)
      salesPerWeek.box.hw <- HoltWinters(salesPerWeek.box.ts,seasonal="additive")
      #salesPerWeek.box.hw <- ets(salesPerWeek.box.ts,model="AAA")
      salesPerWeek.box.hw.fore <- forecast(salesPerWeek.box.hw,h=7)
      BOXHW_forecast <- as.matrix(exp(salesPerWeek.box.hw.fore$mean) - lambda2)
      return(BOXHW_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

BOOTHW <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      #TimeSeriesTraining <- Remove_outliers_impute_mean(TimeSeriesTraining)
      #TimeSeriesTraining <- Correct_for_weekly_seasonality(TimeSeriesTraining)
      minimalNonZero <- min(TimeSeriesTraining[TimeSeriesTraining>0])
      lambda2 <- minimalNonZero/2
      transform <- log(TimeSeriesTraining + lambda2)
      B<- 25
      forecast_total <- rep(0,7)
      bootstrapped <- bld.mbb.bootstrap(transform, B, block_size = 14)
      for(i in 1:B){
        salesPerWeek.boot.ts <- ts(bootstrapped[[i]], start=c(1,1),frequency=7)
        salesPerWeek.boot.hw <- HoltWinters(salesPerWeek.boot.ts,seasonal="additive")
        #salesPerWeek.boot.hw <- ets(salesPerWeek.boot.ts,model="AAA")
        salesPerWeek.boot.hw.fore <- forecast(salesPerWeek.boot.hw,h=7)
        forecast_total <- forecast_total + salesPerWeek.boot.hw.fore$mean
        forecast_final <- forecast_total/B
      }
      BOOTHW_forecast <- as.matrix(exp(forecast_final) - lambda2)
      return(BOOTHW_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

BOOTHW_remove_outliers <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      TimeSeriesTraining <- Remove_outliers_impute_mean(TimeSeriesTraining)
      #TimeSeriesTraining <- Correct_for_weekly_seasonality(TimeSeriesTraining)
      minimalNonZero <- min(TimeSeriesTraining[TimeSeriesTraining>0])
      lambda2 <- minimalNonZero/2
      transform <- log(TimeSeriesTraining + lambda2)
      B<- 25
      forecast_total <- rep(0,7)
      bootstrapped <- bld.mbb.bootstrap(transform, B, block_size = 14)
      for(i in 1:B){
        salesPerWeek.boot.ts <- ts(bootstrapped[[i]], start=c(1,1),frequency=7)
        salesPerWeek.boot.hw <- HoltWinters(salesPerWeek.boot.ts,seasonal="additive")
        #salesPerWeek.boot.hw <- ets(salesPerWeek.boot.ts,model="AAA")
        salesPerWeek.boot.hw.fore <- forecast(salesPerWeek.boot.hw,h=7)
        forecast_total <- forecast_total + salesPerWeek.boot.hw.fore$mean
        forecast_final <- forecast_total/B
      }
      BOOTHW_forecast <- as.matrix(exp(forecast_final) - lambda2)
      return(BOOTHW_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

B <- function(NumberOfWeeksTraining, TimeSeriesTrainingData){
  out <- tryCatch(
    {
      TimeSeriesTrainingData <- Remove_outliers_impute_mean(TimeSeriesTrainingData)
      B_forecast <- B_forecasting_method_no_startdate(NumberOfWeeksTraining, TimeSeriesTrainingData)
      return(B_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

CombinationHWB_method <- function(NumberOfWeeksTraining, TimeSeriesTrainingData){
  out <- tryCatch(
    {
      newNumberWeeks <- (NumberOfWeeksTraining-1)
      newTrainingData <- TimeSeriesTrainingData[1:(7*newNumberWeeks)]
      newTimeSeriesTraining <- ts(newTrainingData, start=c(1,1),frequency=7)
      TimeSeriesTraining <- ts(TimeSeriesTrainingData, start=c(1,1),frequency=7)
      compareData <- TimeSeriesTrainingData[((7*newNumberWeeks)+1):(7*(newNumberWeeks+1))]
      B_forecast <- B(NumberOfWeeksTraining = newNumberWeeks, TimeSeriesTrainingData = newTrainingData)
      HW_forecast <- HW(TimeSeriesTraining = newTimeSeriesTraining)
      
      if(is.null(B_forecast)){
        MSE_B <- NA
        print("NULLBCombined")
      }else {
        MSE_B <- MSE(compareData, B_forecast)
      }
      if(is.null(HW_forecast)){
        MSE_HW <- NA
        print("NULLHWCombined")
      }else {
        MSE_HW <- MSE(compareData, HW_forecast)
      }
      
      if(is.na(MSE_B) & is.na(MSE_HW)){
        combined_forecast <- NULL
      } else if(is.na(MSE_B)){
        combined_forecast <- HW(TimeSeriesTraining = TimeSeriesTraining)
      } else if(is.na(MSE_HW)){
        combined_forecast <- B(NumberOfWeeksTraining = NumberOfWeeksTraining, TimeSeriesTrainingData = TimeSeriesTrainingData)
      }else {
        if(MSE_B <= MSE_HW){
          combined_forecast <- B(NumberOfWeeksTraining = NumberOfWeeksTraining, TimeSeriesTrainingData = TimeSeriesTrainingData)
        }else {
          combined_forecast <- HW(TimeSeriesTraining = TimeSeriesTraining)
        }
      }
      return(combined_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

Combination_method <- function(NumberOfWeeksTraining, TimeSeriesTrainingData){
  out <- tryCatch(
    {
      newNumberWeeks <- (NumberOfWeeksTraining-1)
      newTrainingData <- TimeSeriesTrainingData[1:(7*newNumberWeeks)]
      newTimeSeriesTraining <- ts(newTrainingData, start=c(1,1),frequency=7)
      TimeSeriesTraining <- ts(TimeSeriesTrainingData, start=c(1,1),frequency=7)
      compareData <- TimeSeriesTrainingData[((7*newNumberWeeks)+1):(7*(newNumberWeeks+1))]
      B_forecast <- B(NumberOfWeeksTraining = newNumberWeeks, TimeSeriesTrainingData = newTrainingData)
      HW_forecast <- HW(TimeSeriesTraining = newTimeSeriesTraining)
      ARMA_forecast <- ARMA(TimeSeriesTraining = newTimeSeriesTraining)
      BOXHW_forecast <- BOXHW(TimeSeriesTraining = newTimeSeriesTraining)
      BOOTHW_forecast <- BOOTHW(TimeSeriesTraining = newTimeSeriesTraining)
      
      if(is.null(B_forecast)){
        MSE_B <- 1000
      }else {
        MSE_B <- MSE(compareData, B_forecast)
      }
      if(is.null(ARMA_forecast)){
        MSE_ARMA <- 1000
      }else {
        MSE_ARMA <- MSE(compareData, ARMA_forecast)
      }
      if(is.null(HW_forecast)){
        MSE_HW <- 1000
      }else {
        MSE_HW <- MSE(observations, HW_forecast)
      }
      if(is.null(BOXHW_forecast)){
        MSE_BOXHW <- 1000
      }else {
        MSE_BOXHW <- MSE(observations, BOXHW_forecast)
      }
      if(is.null(BOOTHW_forecast)){
        MSE_BOOTHW <- 1000
      }else {
        MSE_BOOTHW <- MSE(observations, BOOTHW_forecast)
      }
      
      if(MSE_B == 1000 & MSE_HW == 1000 & MSE_ARMA == 1000 & MSE_BOXHW == 1000 & MSE_BOOTHW == 1000){
        combined_forecast <- NULL
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, na.rm = TRUE)==MSE_B){
        combined_forecast <- B(NumberOfWeeksTraining = NumberOfWeeksTraining, TimeSeriesTrainingData = TimeSeriesTrainingData)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, na.rm = TRUE)==MSE_ARMA){
        combined_forecast <- ARMA(TimeSeriesTraining = TimeSeriesTraining)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, na.rm = TRUE)==MSE_HW){
        combined_forecast <- HW(TimeSeriesTraining = TimeSeriesTraining)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, na.rm = TRUE)==MSE_BOXHW){
        combined_forecast <- BOXHW(TimeSeriesTraining = TimeSeriesTraining)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, na.rm = TRUE)==MSE_BOOTHW){
        combined_forecast <- BOOTHW(TimeSeriesTraining = TimeSeriesTraining)
      } 
      return(combined_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}




residuals_ARMA <- c()
residuals_HW <- c()
residuals_BOXHW <- c()
residuals_BOOTHW <- c()
residuals_B <- c()
residuals_C <- c()







tic("simulation")
#Simulation 
NumberOfWeeksTraining <- c(26)
TrendMean <- c(24)
TrendSD <- c(0.4)
SeasonalityMean <- c(0)
SeasonalitySD <- c(4.3)
RandomMean <- c(0)
RandomSD <- c(4.3)
DisturbanceNumber <- c(6)
DisturbanceNumber1 <- c(25)
#startDisturbance <- c(0)
#endDisturbance <- c(6)
DisturbanceBooleanZero <- c(FALSE)
DisturbanceBooleanFactor <- c(FALSE)
DisturbanceFactorMean <- c(1)
DisturbanceFactorSD <- c(0.5)

mean_MSE_tabel <-data.frame(MSE_HW=double(), MSE_BOXHW=double(), MSE_BOOTHW=double(), MSE_B=double(), MSE_ARMA=double(), MSE_C=double(), MSE_perfect=double())#, MSE_HW_r=double(), MSE_BOXHW_r=double(), MSE_BOOTHW_r=double(), MSE_ARMA_r=double())
mean_MSE_tabel_1day <-data.frame(MSE_HW=double(), MSE_BOXHW=double(), MSE_BOOTHW=double(), MSE_B=double(), MSE_ARMA=double(), MSE_C=double(), MSE_perfect=double())#, MSE_HW_r=double(), MSE_BOXHW_r=double(), MSE_BOOTHW_r=double(), MSE_ARMA_r=double())

  
#combinations <- expand.grid(NumberOfWeeksTraining=NumberOfWeeksTraining, TrendMean=TrendMean, SeasonalityMean=SeasonalityMean, SeasonalitySD=SeasonalitySD, 
                            # RandomMean=RandomMean, RandomSD=RandomSD, DisturbanceNumber=DisturbanceNumber, DisturbanceBooleanZero=DisturbanceBooleanZero,
                            # DisturbanceBooleanFactor=DisturbanceBooleanFactor,DisturbanceFactorMean=DisturbanceFactorMean, DisturbanceFactorSD=DisturbanceFactorSD)

# combinations <- data.frame(NumberOfWeeksTraining=double(), TrendMean=double(), TrendSD=double(), SeasonalityMean=double(), SeasonalitySD=double(),
#                             RandomMean=double(), RandomSD=double(), DisturbanceNumber=double(), DisturbanceBooleanZero=double(),
#                             DisturbanceBooleanFactor=double(),DisturbanceFactorMean=double(), DisturbanceFactorSD=double())

# combinations <- dataStorage


combinations <- cbind(Storage_info, NumberOfWeeksTraining=rep(NumberOfWeeksTraining, 11), DisturbanceNumber=rep(DisturbanceNumber, 11), DisturbanceBooleanZero=rep(DisturbanceBooleanZero, 11),
                      DisturbanceBooleanFactor=rep(DisturbanceBooleanFactor, 11),DisturbanceFactorMean=rep(DisturbanceFactorMean, 11), DisturbanceFactorSD=rep(DisturbanceFactorSD, 11))
combinations1 <- cbind(Storage_info, NumberOfWeeksTraining=rep(NumberOfWeeksTraining, 11), DisturbanceNumber=rep(DisturbanceNumber1, 11), DisturbanceBooleanZero=rep(DisturbanceBooleanZero, 11),
                      DisturbanceBooleanFactor=rep(DisturbanceBooleanFactor, 11),DisturbanceFactorMean=rep(DisturbanceFactorMean, 11), DisturbanceFactorSD=rep(DisturbanceFactorSD, 11))
combinations <- rbind(combinations, combinations1)

for(j in 1:nrow(combinations)){
  NumberOfWeeksTraining <- combinations$NumberOfWeeksTraining[j]
  TrendMean <- combinations$TrendMean[j]
  SeasonalityMean <- combinations$SeasonalityMean[j]
  SeasonalitySD <- combinations$SeasonalitySD[j]
  RandomMean <- combinations$RandomMean[j]
  RandomSD <- combinations$RandomSD[j]
  DisturbanceNumber <- combinations$DisturbanceNumber[j]
  DisturbanceBooleanZero <- combinations$DisturbanceBooleanZero[j]
  DisturbanceBooleanFactor <- combinations$DisturbanceBooleanFactor[j]
  DisturbanceFactorMean <- combinations$DisturbanceFactorMean[j]
  DisturbanceFactorSD <- combinations$DisturbanceFactorSD[j]
  
  MSE_tabel <-data.frame(MSE_HW=double(), MSE_BOXHW=double(), MSE_BOOTHW=double(), MSE_B=double(), MSE_ARMA=double(), MSE_C=double(), MSE_perfect=double())#, MSE_HW_r=double(), MSE_BOXHW_r=double(), MSE_BOOTHW_r=double(), MSE_ARMA_r=double())
  MSE_tabel_1day <-data.frame(MSE_HW=double(), MSE_BOXHW=double(), MSE_BOOTHW=double(), MSE_B=double(), MSE_ARMA=double(), MSE_C=double(), MSE_perfect=double())#, MSE_HW_r=double(), MSE_BOXHW_r=double(), MSE_BOOTHW_r=double(), MSE_ARMA_r=double())
  
  Trend <- rep(0, (7*(NumberOfWeeksTraining+1)))
  sum_sd <- 0
  
  for(i in 1:1){
    #Construct time series
    #Trend <- rep(TrendMean, (7*(NumberOfWeeksTraining+1)))
    Trend[1] <- TrendMean
     for(j in 2:(7*(NumberOfWeeksTraining+1))){
       Trend[j] <- Trend[j-1] + rnorm(1, 0, (TrendSD^2))
     }
    # for(j in 1:(7*(NumberOfWeeksTraining+1))){
    #   Trend[j] <- TrendMean + j*0.05
    # }
    #Trend <- proberen met 52 weken en sinus (jaarlijks) als trend
    #Seasonality <- c(1,0,1,1,0,-1,0)
    Seasonality <- rnorm(7,SeasonalityMean,(SeasonalitySD^2))
    Seasonality10 <- rep(Seasonality,(NumberOfWeeksTraining+1))
    Random <- rnorm(7*(NumberOfWeeksTraining+1), RandomMean, (RandomSD^2))
    
    TimeSeries <- Trend + Seasonality10 + Random
    
    
    TimeSeriesCorrect <- Trend + Seasonality10 
    TimeSeriesCorrect[TimeSeriesCorrect<0] <- 0

    #Add disturbances
    #Add zeros
    # if(DisturbanceBooleanZero){
    #   Disturb1 <- sample(1:(NumberOfWeeksTraining*7),DisturbanceNumber)
    #   #Disturb1 <- sample((7*startDisturbance):(7*endDisturbance),DisturbanceNumber)
    #   TimeSeries[Disturb1]<-0
    # }
    # 
    # #Add knowledge, period mulitplied by factor
    # if(DisturbanceBooleanFactor){
    #   Disturb2 <- sample(1:(NumberOfWeeksTraining*7),DisturbanceNumber)
    #   Disturb21 <- rnorm(DisturbanceNumber, DisturbanceFactorMean, DisturbanceFactorSD)
    #   for(j in 1:DisturbanceNumber){
    #     TimeSeries[Disturb2[j]] <-TimeSeries[Disturb2[j]]*Disturb21[j]
    #   }
    # }
    # 
    
    
    #Add random disturbance
    for(k in 1:DisturbanceNumber){
      chooseDisturbance <- runif(1,0,1)
      if(chooseDisturbance <=0.5){
        Disturb1 <- sample(1:((NumberOfWeeksTraining-1)*7),1)
        #Disturb1 <- sample((7*startDisturbance):(7*endDisturbance),DisturbanceNumber)
        TimeSeries[Disturb1]<-0
      } else {
        Disturb2 <- sample(1:((NumberOfWeeksTraining-1)*7),1)
        Disturb21 <- rnorm(DisturbanceNumber, DisturbanceFactorMean, DisturbanceFactorSD)
        for(j in 1:DisturbanceNumber){
          TimeSeries[Disturb2[j]] <-TimeSeries[Disturb2[j]]*Disturb21[j]
        }
      }
    }
    
    TimeSeries[TimeSeries<0] <- 0
    TimeSeriesTrainingData <- TimeSeries[1:(7*NumberOfWeeksTraining)]
    TimeSeriesTraining <- ts(TimeSeriesTrainingData, start=c(1,1),frequency=7)
    observations <- TimeSeries[(7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)]
    TimeSeriesCorrectObservations <- TimeSeriesCorrect[(7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)]
    MSE_perfect <- MSE(observations, TimeSeriesCorrectObservations)
    MSE_perfect_1day <- MSE(observations[1], TimeSeriesCorrectObservations[1])
    

    
    
    
    
    #Holt-Winters forecasting
    HW_forecast <- HW(TimeSeriesTraining = TimeSeriesTraining)

    #ARIMA forecasting
    ARMA_forecast <- ARMA(TimeSeriesTraining = TimeSeriesTraining)

    #Box-Cox HW forecasting
    BOXHW_forecast <- BOXHW(TimeSeriesTraining = TimeSeriesTraining)

    # Holt-Winters combined with Bootstrapping
    BOOTHW_forecast <- BOOTHW(TimeSeriesTraining = TimeSeriesTraining)

    #B_ forecasting
    B_forecast <- B(NumberOfWeeksTraining = NumberOfWeeksTraining, TimeSeriesTrainingData = TimeSeriesTrainingData)

    combination_forecast <- Combination_method(NumberOfWeeksTraining = NumberOfWeeksTraining, TimeSeriesTrainingData = TimeSeriesTrainingData)

    # plot(TimeSeries, type = "l")
    # lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), HW_forecast, col = "red")
    # lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), ARMA_forecast, col = "green")
    # lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), BOXHW_forecast, col = "blue")
    # lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), BOOTHW_forecast, col = "yellow")
    # lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), B_forecast, col = "orange")
    # lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), TimeSeriesCorrectObservations, col = "pink")
    

    if(is.null(ARMA_forecast)){
      MSE_ARMA <- NA
      print("NULLARMA")
    }else {
      MSE_ARMA <- MSE(observations, ARMA_forecast)
      MSE_ARMA_1day <- MSE(observations[1], ARMA_forecast[1])
      residuals_ARMA <- c(residuals_ARMA, residual_function(observations, ARMA_forecast))
    }
    if(is.null(HW_forecast)){
      MSE_HW <- NA
      print("NULLHW")
    }else {
      MSE_HW <- MSE(observations, HW_forecast)
      MSE_HW_1day <- MSE(observations[1], HW_forecast[1])
      residuals_HW <- c(residuals_HW,residual_function(observations, HW_forecast))
    }
    if(is.null(BOXHW_forecast)){
      MSE_BOXHW <- NA
      print("NULLBOX")
    }else {
      MSE_BOXHW <- MSE(observations, BOXHW_forecast)
      MSE_BOXHW_1day <- MSE(observations[1], BOXHW_forecast[1])
      residuals_BOXHW <- c(residuals_BOXHW,residual_function(observations, BOXHW_forecast))
    }
    if(is.null(BOOTHW_forecast)){
      MSE_BOOTHW <- NA
      print("NULLBOOT")
    }else {
      MSE_BOOTHW <- MSE(observations, BOOTHW_forecast)
      MSE_BOOTHW_1day <- MSE(observations[1], BOOTHW_forecast[1])
      residuals_BOOTHW <- c(residuals_BOOTHW, residual_function(observations, BOOTHW_forecast))
    }
    if(is.null(B_forecast)){
      MSE_B <- NA
      print("NULLB")
    }else {
      MSE_B <- MSE(observations, B_forecast)
      MSE_B_1day <- MSE(observations[1], B_forecast[1])
      residuals_B <- c(residuals_B, residual_function(observations, B_forecast))
    }
    if(is.null(combination_forecast)){
      MSE_C <- NA
      print("NULLBCombinationTotal")
    }else {
      MSE_C <- MSE(observations, combination_forecast)
      MSE_C_1day <- MSE(observations[1], combination_forecast[1])
      residuals_C <- c(residuals_C, residual_function(observations, combination_forecast))
    }
    
    

    
    
    MSE_tabel <- rbind(MSE_tabel, list(MSE_HW=MSE_HW, MSE_BOXHW=MSE_BOXHW, MSE_BOOTHW=MSE_BOOTHW, MSE_B=MSE_B, MSE_ARMA=MSE_ARMA, MSE_C=MSE_C, MSE_perfect=MSE_perfect))
    MSE_tabel_1day <- rbind(MSE_tabel_1day, list(MSE_HW=MSE_HW_1day, MSE_BOXHW=MSE_BOXHW_1day, MSE_BOOTHW=MSE_BOOTHW_1day, MSE_B=MSE_B_1day, MSE_ARMA=MSE_ARMA_1day, MSE_C=MSE_C_1day, MSE_perfect=MSE_perfect_1day))
    
  }
  write.table(MSE_tabel, file = paste("G:\\My Drive\\Graduation project\\Inventory run-out\\RProjectInventoryForecast\\MSE_simulatie_",NumberOfWeeksTraining,"_",TrendMean,"_",TrendSD,"_",SeasonalityMean,"_",SeasonalitySD, 
                                      "_",RandomMean,"_",RandomSD,"_",DisturbanceNumber,"_",DisturbanceBooleanZero,
                                      "_",DisturbanceBooleanFactor,"_",DisturbanceFactorMean,"_",DisturbanceFactorSD,".csv", sep=""), append = FALSE,
              sep = ";", dec = ",", col.names = NA, qmethod = "double")

  #print((sum_sd/100))
  
  mean_MSE_HW <- mean(MSE_tabel$MSE_HW, na.rm = TRUE)
  mean_MSE_BOXHW <- mean(MSE_tabel$MSE_BOXHW, na.rm = TRUE)
  mean_MSE_BOOTHW <- mean(MSE_tabel$MSE_BOOTHW, na.rm = TRUE)
  mean_MSE_B <- mean(MSE_tabel$MSE_B, na.rm = TRUE)
  mean_MSE_ARMA <- mean(MSE_tabel$MSE_ARMA, na.rm = TRUE)
  mean_MSE_C <- mean(MSE_tabel$MSE_C, na.rm = TRUE)
  mean_MSE_perfect <- mean(MSE_tabel$MSE_perfect, na.rm = TRUE)
  
  mean_MSE_HW_1day <- mean(MSE_tabel_1day$MSE_HW, na.rm = TRUE)
  mean_MSE_BOXHW_1day <- mean(MSE_tabel_1day$MSE_BOXHW, na.rm = TRUE)
  mean_MSE_BOOTHW_1day <- mean(MSE_tabel_1day$MSE_BOOTHW, na.rm = TRUE)
  mean_MSE_B_1day <- mean(MSE_tabel_1day$MSE_B, na.rm = TRUE)
  mean_MSE_ARMA_1day <- mean(MSE_tabel_1day$MSE_ARMA, na.rm = TRUE)
  mean_MSE_C_1day <- mean(MSE_tabel_1day$MSE_C, na.rm = TRUE)
  mean_MSE_perfect_1day <- mean(MSE_tabel_1day$MSE_perfect, na.rm = TRUE)
  
  mean_MSE_tabel <- rbind(mean_MSE_tabel, list(MSE_HW=mean_MSE_HW, MSE_BOXHW=mean_MSE_BOXHW, MSE_BOOTHW=mean_MSE_BOOTHW, MSE_B=mean_MSE_B, MSE_ARMA=mean_MSE_ARMA, MSE_C=mean_MSE_C, MSE_perfect=mean_MSE_perfect))
  mean_MSE_tabel_1day <- rbind(mean_MSE_tabel_1day, list(MSE_HW=mean_MSE_HW_1day, MSE_BOXHW=mean_MSE_BOXHW_1day, MSE_BOOTHW=mean_MSE_BOOTHW_1day, MSE_B=mean_MSE_B_1day, MSE_ARMA=mean_MSE_ARMA_1day, MSE_C=mean_MSE_C_1day, MSE_perfect=mean_MSE_perfect_1day))

}
mean_MSE_tabel <- cbind(combinations, mean_MSE_tabel)
write.table(mean_MSE_tabel, file = paste("G:\\My Drive\\Graduation project\\Inventory run-out\\RProjectInventoryForecast\\MSE_simulatie_total_18-09-2020.csv", sep=""), append = FALSE,
            sep = ";", dec = ",", col.names = NA, qmethod = "double")

toc()







hist(residuals_ARMA, breaks = 50)
hist(residuals_HW, breaks = 50)
hist(residuals_BOXHW, breaks = 100)
hist(residuals_BOOTHW, breaks = 100)
hist(residuals_B, breaks = 50)
hist(residuals_C, breaks = 50)


plot(TimeSeries, type = "l")
# lines((Remove_outliers_impute_mean(TimeSeriesTrainingData)), col = "pink")
lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), HW_forecast, col = "red")
lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), ARMA_forecast, col = "green")
lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), BOXHW_forecast, col = "blue")
lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), BOOTHW_forecast, col = "yellow")
lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), B_forecast, col = "orange")

print(MSE_ARMA)
print(MSE_HW)
print(MSE_BOXHW)

test <- data.frame(TimeSeriesTraining, Remove_outliers_impute_mean(TimeSeriesTraining))

