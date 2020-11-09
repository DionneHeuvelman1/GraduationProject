#Load library

library(fpp2)


#Define functions

# Calculate Mean Squared Error
MSE <- function(observations, forecasts){
  if(length(observations)!=length(forecasts)){
    print("could not calculate MSE: lengths not equal")
    return(-1)
  }
  error <- observations - forecasts
  MSE <- (sum(error^2)/length((observations)))
  return(MSE)
}

# ARMA method
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

# ARMA method where negative forecasts are set to zero
ARMA_truncated <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      salesPerWeek.sarima <- Arima(TimeSeriesTraining, order=c(1,0,1), seasonal=list(order=c(1,1,1), period=7))#, method = "ML")
      salesPerWeek.sarima.fore <- forecast(salesPerWeek.sarima, h=7)
      salesPerWeek.sarima.fore$mean[salesPerWeek.sarima.fore$mean<0] <- 0
      ARMA_forecast <- as.matrix(salesPerWeek.sarima.fore$mean)
      return(ARMA_forecast)
    },
    error=function(cond) {
      # Choose a return value in case of error
      return(NULL)
    })
  return(out)
}

# ARMA method with Box-Cox transformation
BOXARMA <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      minimalNonZero <- min(TimeSeriesTraining[TimeSeriesTraining>0])
      lambda2 <- minimalNonZero/2
      transform <- log(TimeSeriesTraining + lambda2)
      salesPerWeek.box.ts <- ts(transform, start=c(1,1),frequency=7)
      salesPerWeek.box.sarima <- Arima(salesPerWeek.box.ts, order=c(1,0,1), seasonal=list(order=c(1,1,1), period=7))#, method = "ML")
      salesPerWeek.box.sarima.fore <- forecast(salesPerWeek.box.sarima, h=7)
      BOXARMA_forecast <- as.matrix(exp(salesPerWeek.box.sarima.fore$mean) - lambda2)
      return(BOXARMA_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

# ARMA method with Box-Cox transformation and bootstrapping
BOOTARMA <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      minimalNonZero <- min(TimeSeriesTraining[TimeSeriesTraining>0])
      lambda2 <- minimalNonZero/2
      transform <- log(TimeSeriesTraining + lambda2)
      B<- 25
      forecast_total <- rep(0,7)
      bootstrapped <- bld.mbb.bootstrap(transform, B, block_size = 14)
      for(i in 1:B){
        salesPerWeek.boot.ts <- ts(bootstrapped[[i]], start=c(1,1),frequency=7)
        salesPerWeek.boot.sarima <- Arima(salesPerWeek.boot.ts, order=c(1,0,1), seasonal=list(order=c(1,1,1), period=7))#, method = "ML")
        salesPerWeek.boot.sarima.fore <- forecast(salesPerWeek.boot.sarima, h=7)
        forecast_total <- forecast_total + salesPerWeek.boot.sarima.fore$mean
        forecast_final <- forecast_total/B
      }
      BOOTARMA_forecast <- as.matrix(exp(forecast_final) - lambda2)
      return(BOOTARMA_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

# Holt-Winters method
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

# Holt-Winters method where negative forecasts are set to zero
HW_truncated <- function(TimeSeriesTraining){
  out <- tryCatch(
    {
      TimeSeries.hw <- HoltWinters(TimeSeriesTraining,seasonal="additive")
      TimeSeries.hw.fore <- forecast(TimeSeries.hw,h=7)
      TimeSeries.hw.fore$mean[TimeSeries.hw.fore$mean <0] <- 0
      HW_forecast <- as.matrix(TimeSeries.hw.fore$mean)
      return(HW_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}

# Holt-Winters method using a Box-Cox transformation
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

# Holt-Winters method using a Box-Cox transformation and bootstrapping of the residuals
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

# Current method of Bottomline
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

# Combination of all methods to choose best method based on MSE last week
Combination_method <- function(NumberOfWeeksTraining, TimeSeriesTrainingData){
  out <- tryCatch(
    {
      newNumberWeeks <- (NumberOfWeeksTraining-1)
      newTrainingData <- TimeSeriesTrainingData[1:(7*newNumberWeeks)]
      newTimeSeriesTraining <- ts(newTrainingData, start=c(1,1),frequency=7)
      TimeSeriesTraining <- ts(TimeSeriesTrainingData, start=c(1,1),frequency=7)
      compareData <- TimeSeriesTrainingData[((7*newNumberWeeks)+1):(7*(newNumberWeeks+1))]
      B_forecast <- B(NumberOfWeeksTraining = newNumberWeeks, TimeSeriesTrainingData = newTrainingData)
      HW_forecast <- HW_truncated(TimeSeriesTraining = newTimeSeriesTraining)
      ARMA_forecast <- ARMA_truncated(TimeSeriesTraining = newTimeSeriesTraining)
      BOXHW_forecast <- BOXHW(TimeSeriesTraining = newTimeSeriesTraining)
      BOOTHW_forecast <- BOOTHW(TimeSeriesTraining = newTimeSeriesTraining)
      BOXARMA_forecast <- BOXARMA(TimeSeriesTraining = newTimeSeriesTraining)
      BOOTARMA_forecast <- BOOTARMA(TimeSeriesTraining = newTimeSeriesTraining)
      
      if(is.null(B_forecast)){
        MSE_B <- 1000
      }else {
        MSE_B <- MSE(compareData, B_forecast)
        print("MSE_B")
        print(MSE_B)
      }
      if(is.null(ARMA_forecast)){
        MSE_ARMA <- 1000
      }else {
        MSE_ARMA <- MSE(compareData, ARMA_forecast)
        print("MSE_ARMA")
        print(MSE_ARMA)
      }
      if(is.null(BOOTARMA_forecast)){
        MSE_BOOTARMA <- 1000
      }else {
        MSE_BOOTARMA <- MSE(observations, BOOTARMA_forecast)
        print("MSE_BOOTARMA")
        print(MSE_BOOTARMA)
      }
      if(is.null(BOXARMA_forecast)){
        MSE_BOXARMA <- 1000
      }else {
        MSE_BOXARMA <- MSE(observations, BOXARMA_forecast)
        print("MSE_BOXARMA")
        print(MSE_BOXARMA)
      }
      if(is.null(HW_forecast)){
        MSE_HW <- 1000
      }else {
        MSE_HW <- MSE(observations, HW_forecast)
        print("MSE_HW")
        print(MSE_HW)
      }
      if(is.null(BOXHW_forecast)){
        MSE_BOXHW <- 1000
      }else {
        MSE_BOXHW <- MSE(observations, BOXHW_forecast)
        print("MSE_BOXHW")
        print(MSE_BOXHW)
      }
      if(is.null(BOOTHW_forecast)){
        MSE_BOOTHW <- 1000
      }else {
        MSE_BOOTHW <- MSE(observations, BOOTHW_forecast)
        print("MSE_BOOTHW")
        print(MSE_BOOTHW)
      }
      
      if(MSE_B == 1000 & MSE_HW == 1000 & MSE_ARMA == 1000 & MSE_BOXHW == 1000 & MSE_BOOTHW == 1000& MSE_BOXARMA == 1000 & MSE_BOOTARMA == 1000){
        combined_forecast <- NULL
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, MSE_BOXARMA, MSE_BOOTARMA, na.rm = TRUE)==MSE_B){
        combined_forecast <- B(NumberOfWeeksTraining = NumberOfWeeksTraining, TimeSeriesTrainingData = TimeSeriesTrainingData)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, MSE_BOXARMA, MSE_BOOTARMA, na.rm = TRUE)==MSE_ARMA){
        combined_forecast <- ARMA(TimeSeriesTraining = TimeSeriesTraining)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, MSE_BOXARMA, MSE_BOOTARMA, na.rm = TRUE)==MSE_HW){
        combined_forecast <- HW(TimeSeriesTraining = TimeSeriesTraining)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, MSE_BOXARMA, MSE_BOOTARMA, na.rm = TRUE)==MSE_BOXHW){
        combined_forecast <- BOXHW(TimeSeriesTraining = TimeSeriesTraining)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, MSE_BOXARMA, MSE_BOOTARMA, na.rm = TRUE)==MSE_BOOTHW){
        combined_forecast <- BOOTHW(TimeSeriesTraining = TimeSeriesTraining)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, MSE_BOXARMA, MSE_BOOTARMA, na.rm = TRUE)==MSE_BOXARMA){
        combined_forecast <- BOXARMA(TimeSeriesTraining = TimeSeriesTraining)
      } else if(min(MSE_B, MSE_ARMA, MSE_HW, MSE_BOXHW, MSE_BOOTHW, MSE_BOXARMA, MSE_BOOTARMA, na.rm = TRUE)==MSE_BOOTARMA){
        combined_forecast <- BOOTARMA(TimeSeriesTraining = TimeSeriesTraining)
      } 
      
      return(combined_forecast)
    },
    error=function(cond) {
      return(NULL)
    })
  return(out)
}






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
DisturbanceBooleanZero <- c(FALSE)
DisturbanceBooleanFactor <- c(FALSE)
DisturbanceFactorMean <- c(1)
DisturbanceFactorSD <- c(0.5)

mean_MSE_tabel <-data.frame(MSE_HW=double(), MSE_BOXHW=double(), MSE_BOOTHW=double(), MSE_B=double(), MSE_ARMA=double(), MSE_BOXARMA=double(), MSE_BOOTARMA=double(), MSE_C=double(), MSE_perfect=double())#, MSE_HW_r=double(), MSE_BOXHW_r=double(), MSE_BOOTHW_r=double(), MSE_ARMA_r=double())


combinations <- expand.grid(NumberOfWeeksTraining=NumberOfWeeksTraining, TrendMean=TrendMean, SeasonalityMean=SeasonalityMean, SeasonalitySD=SeasonalitySD,
RandomMean=RandomMean, RandomSD=RandomSD, DisturbanceNumber=DisturbanceNumber, DisturbanceBooleanZero=DisturbanceBooleanZero,
DisturbanceBooleanFactor=DisturbanceBooleanFactor,DisturbanceFactorMean=DisturbanceFactorMean, DisturbanceFactorSD=DisturbanceFactorSD)


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
  
  MSE_tabel <-data.frame(MSE_HW=double(), MSE_BOXHW=double(), MSE_BOOTHW=double(), MSE_B=double(), MSE_ARMA=double(), MSE_BOXARMA=double(), MSE_BOOTARMA=double(), MSE_C=double(), MSE_perfect=double())#, MSE_HW_r=double(), MSE_BOXHW_r=double(), MSE_BOOTHW_r=double(), MSE_ARMA_r=double())

  Trend <- rep(0, (7*(NumberOfWeeksTraining+1)))
  sum_sd <- 0
  
  for(i in 1:1){
    #Construct time series
    Trend[1] <- TrendMean
    for(j in 2:(7*(NumberOfWeeksTraining+1))){
      Trend[j] <- Trend[j-1] + rnorm(1, 0, (TrendSD^2))
    }

    Seasonality <- rnorm(7,SeasonalityMean,(SeasonalitySD^2))
    Seasonality10 <- rep(Seasonality,(NumberOfWeeksTraining+1))
    Random <- rnorm(7*(NumberOfWeeksTraining+1), RandomMean, (RandomSD^2))
    
    TimeSeries <- Trend + Seasonality10 + Random
    TimeSeriesCorrect <- Trend + Seasonality10 
    TimeSeriesCorrect[TimeSeriesCorrect<0] <- 0
    
    #Add random disturbance, 2 types: multiply by factor or set to zero
    for(k in 1:DisturbanceNumber){
      chooseDisturbance <- runif(1,0,1)
      if(chooseDisturbance <=0.5){
        Disturb1 <- sample(1:((NumberOfWeeksTraining-1)*7),1)
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

    
    #Holt-Winters forecasting
    HW_forecast <- HW_truncated(TimeSeriesTraining = TimeSeriesTraining)
    
    #Box-Cox Holt-Winters forecasting
    BOXHW_forecast <- BOXHW(TimeSeriesTraining = TimeSeriesTraining)
    
    # Holt-Winters combined with Bootstrapping and Box-Cox transformation
    BOOTHW_forecast <- BOOTHW(TimeSeriesTraining = TimeSeriesTraining)
    
    #ARMA forecasting
    ARMA_forecast <- ARMA_truncated(TimeSeriesTraining = TimeSeriesTraining)
    
    #BOXARMA forecasting
    BOXARMA_forecast <- BOXARMA(TimeSeriesTraining = TimeSeriesTraining)
    
    #BOOTARMA forecasting
    BOOTARMA_forecast <- BOOTARMA(TimeSeriesTraining = TimeSeriesTraining)
    
    #Bottomline forecasting
    B_forecast <- B(NumberOfWeeksTraining = NumberOfWeeksTraining, TimeSeriesTrainingData = TimeSeriesTrainingData)
    
    #Combination forecasting
    combination_forecast <- Combination_method(NumberOfWeeksTraining = NumberOfWeeksTraining, TimeSeriesTrainingData = TimeSeriesTrainingData)
    
    # Create plot of time series
    plot(TimeSeries, type = "l", xlab = "Days")
    lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), HW_forecast, col = "red")
    lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), ARMA_forecast, col = "green")
    lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), BOXARMA_forecast, col = "pink")
    lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), BOOTARMA_forecast, col = "purple")
    lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), BOXHW_forecast, col = "blue")
    lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), BOOTHW_forecast, col = "yellow")
    lines(c((7*NumberOfWeeksTraining+1):(7*NumberOfWeeksTraining+7)), B_forecast, col = "orange")

    # Calculate MSE
    if(is.null(ARMA_forecast)){
      MSE_ARMA <- NA
    }else {
      MSE_ARMA <- MSE(observations, ARMA_forecast)
    }
    if(is.null(BOXARMA_forecast)){
      MSE_BOXARMA <- NA
    }else {
      MSE_BOXARMA <- MSE(observations, BOXARMA_forecast)
    }
    if(is.null(BOOTARMA_forecast)){
      MSE_BOOTARMA <- NA
    }else {
      MSE_BOOTARMA <- MSE(observations, BOOTARMA_forecast)
    }
    if(is.null(HW_forecast)){
      MSE_HW <- NA
    }else {
      MSE_HW <- MSE(observations, HW_forecast)
    }
    if(is.null(BOXHW_forecast)){
      MSE_BOXHW <- NA
    }else {
      MSE_BOXHW <- MSE(observations, BOXHW_forecast)
    }
    if(is.null(BOOTHW_forecast)){
      MSE_BOOTHW <- NA
    }else {
      MSE_BOOTHW <- MSE(observations, BOOTHW_forecast)
    }
    if(is.null(B_forecast)){
      MSE_B <- NA
    }else {
      MSE_B <- MSE(observations, B_forecast)
    }
    if(is.null(combination_forecast)){
      MSE_C <- NA
    }else {
      MSE_C <- MSE(observations, combination_forecast)
    }
    
    # Create table of results
    MSE_tabel <- rbind(MSE_tabel, list(MSE_HW=MSE_HW, MSE_BOXHW=MSE_BOXHW, MSE_BOOTHW=MSE_BOOTHW, MSE_B=MSE_B, MSE_ARMA=MSE_ARMA, MSE_BOXARMA=MSE_BOXARMA, MSE_BOOTARMA=MSE_BOOTARMA, MSE_C=MSE_C, MSE_perfect=MSE_perfect))

  }

  #Calculate mean MSE for every combination of parameters
  mean_MSE_HW <- mean(MSE_tabel$MSE_HW, na.rm = TRUE)
  mean_MSE_BOXHW <- mean(MSE_tabel$MSE_BOXHW, na.rm = TRUE)
  mean_MSE_BOOTHW <- mean(MSE_tabel$MSE_BOOTHW, na.rm = TRUE)
  mean_MSE_B <- mean(MSE_tabel$MSE_B, na.rm = TRUE)
  mean_MSE_ARMA <- mean(MSE_tabel$MSE_ARMA, na.rm = TRUE)
  mean_MSE_BOXARMA <- mean(MSE_tabel$MSE_BOXARMA, na.rm = TRUE)
  mean_MSE_BOOTARMA <- mean(MSE_tabel$MSE_BOOTARMA, na.rm = TRUE)
  mean_MSE_C <- mean(MSE_tabel$MSE_C, na.rm = TRUE)
  mean_MSE_perfect <- mean(MSE_tabel$MSE_perfect, na.rm = TRUE)
  
  # Create table with mean MSE
  mean_MSE_tabel <- rbind(mean_MSE_tabel, list(MSE_HW=mean_MSE_HW, MSE_BOXHW=mean_MSE_BOXHW, MSE_BOOTHW=mean_MSE_BOOTHW, MSE_B=mean_MSE_B, MSE_ARMA=mean_MSE_ARMA, MSE_BOXARMA=mean_MSE_BOXARMA, MSE_BOOTARMA=mean_MSE_BOOTARMA, MSE_C=mean_MSE_C, MSE_perfect=mean_MSE_perfect))

}







