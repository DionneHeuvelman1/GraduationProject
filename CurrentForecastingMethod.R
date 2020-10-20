#load data
library(readr)
dataStorageAll <- read_delim("NLTEST_by_averageSalesQty_inventory.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)

dataDaySalesBreakdownDetail <- read_delim("DaySalesBreakdownDetail.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)

dataWeekSalesBreakdownDetail <- read_delim("WeekSalesBreakdownDetail.csv", 
                                       ";", escape_double = FALSE, trim_ws = TRUE)

dataKnowledge <- read_delim("knowledge_45_storages_2019.csv", 
                        ";", escape_double = FALSE, col_types = cols(EndDate = col_datetime(format = "%Y-%m-%d %H:%M:%OS"), 
                                                                     StartDate = col_datetime(format = "%Y-%m-%d %H:%M:%OS")), 
                        trim_ws = TRUE)

dataTripStopStorage <- read_delim("delivery_45_storages_2019.csv", 
                                                                       ";", escape_double = FALSE, col_types = cols(ConfirmedArrivalDT = col_datetime(format = "%Y-%m-%d %H:%M:%OS"), 
                                                                                                                    ConfirmedDepartureDT = col_datetime(format = "%Y-%m-%d %H:%M:%OS"), 
                                                                                                                    PlannedQty = col_double(), StartInventoryQty = col_double()), 
                                                                       trim_ws = TRUE)

dataInventory <- read_delim("inventory_45_storages_2019.csv", 
                              ";", escape_double = FALSE, col_types = cols(AvgSalesQty = col_double(), 
                                                                           BookInventoryQty = col_double(), 
                                                                           ExcessQty = col_double(), InventoryDT = col_datetime(format = "%Y-%m-%d %H:%M:%OS"), 
                                                                           ReservedQty = col_double()), trim_ws = TRUE)
n <- nrow(dataStorageAll) 
#Filter 15 fast, medium and slow running storages
#dataStorage <- rbind(dataStorageAll[1:15,], dataStorageAll[(floor(n/2)):(floor(n/2)+14),], dataStorageAll[(n-14):n,])
#Storages <- c(dataStorage$StorageID)
#print(Storages)

#make dataset with knowledge of selected storages
#dataKnowledge <- subset(dataKnowledge, dataKnowledge$StorageID%in%(dataStorage$StorageID))
dataStorage <- read_delim("storage_45_storages_2019.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)


#let op! niet groot genoege dataset om 20 weken te gebruiken, dus even naar 4 gezet
#dataStorage$ForecastHistoryWeeks[2]<-4



#Normalize data
#1. Take two inventory measurements and deliveries and calculate sales
#     old inventory - new inventory + delivery
#2. Calculate sales percentage per time period between inventory measurements
#     weekpattern percentages, total period in time periods, percentage per time period
#3. Calculate sales per time period
#4. Repeat..
#5. Calculate sales per day

numberDayPeriods <- 4
salesPerPeriodDataset <- data.frame(dayOfYear=double(), percentage=double(), sales=double(), percentageTotal=double(), dayOfWeek=double())
datasetCalculatedSalesForecast <- data.frame(storage=double(), dayOfYear=double(), sales=double())

forecastStartDate <- as.POSIXct("2019-04-28 00:00:00",format="%Y-%m-%d %H:%M:%OS", tz="UTC")



for (i in 1:nrow(dataStorage)){
  #initialise parameters
  dayPattern <- 0
  deliveryQty <- 0
  diffInventory <- 0
  percentagePerDay <- 0
  Storages <- 0
  t1 <- as.POSIXct("2019-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S", tz="UTC")
  t2 <- as.POSIXct("2019-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S", tz="UTC")
  totalDiffInventory <- 0
  weekPattern <- 0
  salesPerPeriodVector <- 0
  numberPerDay <- 0
  timeUntillEndOfBlock <- 0
  salesPerPeriodDataset <- data.frame(dayOfYear=double(), percentage=double(), sales=double(), percentageTotal=double(), dayOfWeek=double())
  inventoryIndex<-1
  
#1.
  #Take two inventory measurements and deliveries and calculate sales
  #Only use forecastNumberOfWeeks of data (and the last inventory before this period)
  #This because using too much data might make the process slow
  #if there is only one inventory found in the time period, then one extra is selected
  ID <- dataStorage$StorageID[i]
  forecastNumberOfWeeks <- dataStorage$ForecastHistoryWeeks[i]
  inventory <- subset(dataInventory, dataInventory$StorageID==ID)
  inventory <- inventory[order(inventory$InventoryDT),]
  
  #do not use inventories later than forecastStartDate
  while (inventory$InventoryDT[nrow(inventory)]>forecastStartDate){
    inventory <- inventory[-c(nrow(inventory)),]
  }
  
  numberOfRows <- nrow(inventory)
  #forecastNumberOfWeeks voor laatste inventory pakken zodat altijd evenveel sales gebruikt kunnen worden. Dus niet forecastStartDate
  endDayInventory <- inventory$InventoryDT[numberOfRows]
  #je moet op minimaal 7*forecastNumberOfWeeks hele dagen uitkomen, en eerste en laatste dagen van metingen zijn geen hele dagen, en tijd op laatste dag kan na 00:00:00 
  #uur zijn, dus 3 dagen extra
  startDateInventory <- as.POSIXct(endDayInventory - (24*60*60*(7*forecastNumberOfWeeks+3)))
  
  for (a in 1:(nrow(inventory)-1)){
    if (startDateInventory <= inventory$InventoryDT[a+1] & startDateInventory > inventory$InventoryDT[a]){
      inventoryIndex <- a
    }
  }
  if(numberOfRows==inventoryIndex){
    inventory <- inventory[c((numberofRows-1):numberOfRows),]
  }else {
    inventory <- inventory[c(inventoryIndex:numberOfRows),]
  }

  
  inventory <- inventory[order(inventory$InventoryDT),]
  delivery <- subset(dataTripStopStorage, dataTripStopStorage$StorageID==ID)
  for (j in 1: (nrow(inventory)-1)){ 
    diffInventory[j] <- inventory$InventoryQty[j]-inventory$InventoryQty[j+1]
    deliveryQty[j] <- 0
    for (k in 1:nrow(delivery)){
      if (inventory$InventoryDT[j] <= delivery$ConfirmedArrivalDT[k] & inventory$InventoryDT[j+1] > delivery$ConfirmedArrivalDT[k] ){
          deliveryQty[j] <- as.numeric(deliveryQty[j]) + delivery$ConfirmedQty[k] 
      }
    }
    totalDiffInventory[j] <- diffInventory[j] + as.numeric(deliveryQty[j])
    if (totalDiffInventory[j] < 0 ){
      totalDiffInventory[j] <- 0
    }
    t1[j] <- inventory$InventoryDT[j]
    t2[j] <- inventory$InventoryDT[j+1]

#2.
    #import day and week patterns
    daySales <- dataStorage$DaySalesBreakdownID[i]
    weekSales <- dataStorage$WeekSalesBreakdownID[i]
    dataWeekSalesBreakdownDetail <- dataWeekSalesBreakdownDetail[order(dataWeekSalesBreakdownDetail$WeekDayNumber),]
    dataWeekSalesBreakdownDetail <- dataWeekSalesBreakdownDetail[order(dataWeekSalesBreakdownDetail$WeekSalesBreakdownID),]
    for (l in 1:numberDayPeriods){
      dayPattern[l] <- dataDaySalesBreakdownDetail$SalesBreakdownPct[numberDayPeriods*(daySales-1)+l]
    }
    for (m in 1:6){  #first day (number 1) is monday, sunday=0
      weekPattern[m] <- dataWeekSalesBreakdownDetail$SalesBreakdownPct[7*(weekSales-1)+m+1]
    }
    weekPattern[7] <- dataWeekSalesBreakdownDetail$SalesBreakdownPct[7*(weekSales-1)+1]
    
    #day of the week
    dayNumberStart <- as.numeric(format(t1[j],"%w", tz="UTC"))
    if (dayNumberStart==0){
      dayNumberStart <- 7
    }
    dayNumberEnd <- as.numeric(format(t2[j],"%w", tz="UTC"))
    if (dayNumberEnd==0){
      dayNumberEnd <- 7
    }
    
    #numberOfDays calculates number of unique days between measurements
    numberOfDays <- as.numeric(strftime(t2[j], format = "%j", tz="UTC")) - as.numeric(strftime(t1[j], format = "%j", tz="UTC")) + 1
    percentagePerDay <- rep(0, numberOfDays)
    numberPerDay[1] <- dayNumberStart
    
    if (numberOfDays == 1) {
      timeUntillNextInventory <- as.numeric(difftime(t2[j], t1[j] , units = c("hours")))
      #kijken of begintijd in het eerste blok valt, zo ja: tijd tot einde blok uitrekenen. dan kijken of eindtijd in volgende blok. ja: tijd tot eind, nee: +6 uur en dan herhalen
      #herhalen voor alle blokken
      timeUntillEndOfBlock1 <- as.numeric(difftime( as.POSIXct(paste(as.Date(t1[j],'%Y/%m/%d'),"05:59:59"),format="%Y-%m-%d %H:%M:%S", tz="UTC"), t1[j] , units = c("hours")))
      timeUntillEndOfBlock2 <- as.numeric(difftime( as.POSIXct(paste(as.Date(t1[j],'%Y/%m/%d'),"11:59:59"),format="%Y-%m-%d %H:%M:%S", tz="UTC"), t1[j] , units = c("hours")))
      timeUntillEndOfBlock3 <- as.numeric(difftime( as.POSIXct(paste(as.Date(t1[j],'%Y/%m/%d'),"17:59:59"),format="%Y-%m-%d %H:%M:%S", tz="UTC"), t1[j] , units = c("hours")))
      timeUntillEndOfBlock4 <- as.numeric(difftime( as.POSIXct(paste(as.Date(t1[j],'%Y/%m/%d'),"23:59:59"),format="%Y-%m-%d %H:%M:%S", tz="UTC"), t1[j] , units = c("hours")))
      timeUntillEndOfBlock <- c(timeUntillEndOfBlock1, timeUntillEndOfBlock2, timeUntillEndOfBlock3, timeUntillEndOfBlock4)
      if (timeUntillEndOfBlock1 >= 0) {
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[1]*(timeUntillEndOfBlock1/6)
        startBlock <- 1
      } else if (timeUntillEndOfBlock2 >= 0) {
        startBlock <- 2
      } else if (timeUntillEndOfBlock3 >= 0) {
        startBlock <- 3
      } else if (timeUntillEndOfBlock4 >= 0) {
        startBlock <- 4
      }
      
      timeUntillStartOfBlock1 <- as.numeric(difftime(t2[j], as.POSIXct(paste(as.Date(t2[j],'%Y/%m/%d'),"00:00:00"),format="%Y-%m-%d %H:%M:%S", tz="UTC"), units = c("hours")))
      timeUntillStartOfBlock2 <- as.numeric(difftime(t2[j], as.POSIXct(paste(as.Date(t2[j],'%Y/%m/%d'),"06:00:00"),format="%Y-%m-%d %H:%M:%S", tz="UTC"), units = c("hours")))
      timeUntillStartOfBlock3 <- as.numeric(difftime(t2[j], as.POSIXct(paste(as.Date(t2[j],'%Y/%m/%d'),"12:00:00"),format="%Y-%m-%d %H:%M:%S", tz="UTC"), units = c("hours")))
      timeUntillStartOfBlock4 <- as.numeric(difftime(t2[j], as.POSIXct(paste(as.Date(t2[j],'%Y/%m/%d'),"18:00:00"),format="%Y-%m-%d %H:%M:%S", tz="UTC"), units = c("hours")))
      timeUntillStartOfBlock <- c(timeUntillStartOfBlock1, timeUntillStartOfBlock2, timeUntillStartOfBlock3, timeUntillStartOfBlock4)
      if (timeUntillStartOfBlock4 >= 0) {
        endBlock <- 4
      } else if (timeUntillStartOfBlock3 >= 0) {
        endBlock <- 3
      } else if (timeUntillStartOfBlock2 >= 0) {
        endBlock <- 2
      } else if (timeUntillStartOfBlock1 >= 0) {
        endBlock <- 1
      }
      
      if (startBlock == endBlock) {
        percentagePerDay[1] <- weekPattern[dayNumberStart]*dayPattern[startBlock]*(timeUntillNextInventory/6)
      }
      if (endBlock - startBlock == 1) {
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[startBlock]*(timeUntillEndOfBlock[startBlock]/6)
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[endBlock]*(timeUntillStartOfBlock[endBlock]/6)
      }
      if (endBlock - startBlock == 2) {
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[startBlock]*(timeUntillEndOfBlock[startBlock]/6)
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[startBlock + 1]
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[endBlock]*(timeUntillStartOfBlock[endBlock]/6)
      }
      if (endBlock - startBlock == 3) {
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[startBlock]*(timeUntillEndOfBlock[startBlock]/6)
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[startBlock + 1]
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[startBlock + 2]
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[endBlock]*(timeUntillStartOfBlock[endBlock]/6)
      }
    } else { #(numberOfDays >= 2)
      #percentage for first day
      endOfDay <- as.POSIXct(paste(as.Date(t1[j],'%Y/%m/%d'),"23:59:59"),format="%Y-%m-%d %H:%M:%S", tz="UTC")
      timeUntillEndOfDayStart <- as.numeric(difftime(endOfDay, t1[j] , units = c("hours")))
      timesInLoop <- 1
      while (timeUntillEndOfDayStart >= 6){
        timeUntillEndOfDayStart <- timeUntillEndOfDayStart-6
        percentagePerDay[1] <- percentagePerDay[1] + weekPattern[dayNumberStart]*dayPattern[4 - timesInLoop + 1]
        timesInLoop <- timesInLoop + 1
      }
      if (timeUntillEndOfDayStart < 6){
        percentagePerDay[1] <- percentagePerDay[1] +weekPattern[dayNumberStart]*dayPattern[4 - timesInLoop + 1]*(timeUntillEndOfDayStart/6)
      }
      
      #percentage last day
      startOfDay <- as.POSIXct(paste(as.Date(t2[j],'%Y/%m/%d'),"00:00:00"),format="%Y-%m-%d %H:%M:%S", tz="UTC")
      timeUntillStartOfDay <- as.numeric(difftime(t2[j], startOfDay , units = c("hours")))
      timesInLoop <- 1
      numberPerDay[numberOfDays] <- dayNumberEnd
      while (timeUntillStartOfDay >= 6){
        timeUntillStartOfDay <- timeUntillStartOfDay-6
        percentagePerDay[numberOfDays] <- percentagePerDay[numberOfDays] + weekPattern[dayNumberEnd]*dayPattern[1 + timesInLoop - 1]
        timesInLoop <- timesInLoop + 1
      }
      if (timeUntillStartOfDay < 6){
        percentagePerDay[numberOfDays] <- percentagePerDay[numberOfDays] + weekPattern[dayNumberEnd]*dayPattern[timesInLoop]*(timeUntillStartOfDay/6)
      }
      
      #day between first en last
      if (numberOfDays>2){
        for (a in 2:(numberOfDays-1)){
          dayNumber <- dayNumberStart + a - 1
          while (dayNumber>7){
            dayNumber <- dayNumber-7
          }
          percentagePerDay[a] <- weekPattern[dayNumber]
          numberPerDay[a] <- dayNumber
        }
      }
    }
    

#3.
    totalPercentage <- sum(percentagePerDay)
    dayOfYearStart <- as.numeric(strftime(t1[j], format = "%j", tz="UTC"))
    for (a in 1:numberOfDays){
      if(totalPercentage==0){
        salesPerPeriodVector[a] <- 0
      } else {
        salesPerPeriodVector[a] <- totalDiffInventory[j]*percentagePerDay[a]/totalPercentage
      }
      salesPerPeriodDataset <- rbind(salesPerPeriodDataset, list(dayOfYear=(dayOfYearStart + a - 1), percentage=percentagePerDay[a], sales=salesPerPeriodVector[a], percentageTotal=totalPercentage, dayOfWeek=numberPerDay[a]))
    }
    
#4.
  }

#5.
  startDay <- min(salesPerPeriodDataset$dayOfYear)
  endDay <- max(salesPerPeriodDataset$dayOfYear)
  salesPerDayDataset <- data.frame(dayOfYear=double(), percentage=double(), sales=double(), dayOfWeek=double())
  for (b in startDay:endDay){
    perDay <- subset(salesPerPeriodDataset, salesPerPeriodDataset$dayOfYear==b)
    salesPerDayDataset <- rbind(salesPerDayDataset, list(dayOfYear=b, percentage=sum(perDay$percentage), sales=sum(perDay$sales), dayOfWeek=perDay$dayOfWeek[1]))
  }
  
  
  
#Correct for knowledge
  
  data45Knowledge <- read_delim("knowledge_45_storages_2019.csv", 
                                ";", escape_double = FALSE, col_types = cols(EndDate = col_datetime(format = "%Y-%m-%d %H:%M:%OS"), 
                                                                             StartDate = col_datetime(format = "%Y-%m-%d %H:%M:%OS")), 
                                trim_ws = TRUE)
  knowledge <- subset(data45Knowledge, dataKnowledge$StorageID==ID)
  
  
  if(nrow(knowledge)>0){
    for (a in 1:nrow(salesPerDayDataset)){
      for (d in 1:nrow(knowledge)){
        startDate <- as.numeric(strftime(knowledge$StartDate[d], format = "%j", tz="UTC"))
        endDate <- as.numeric(strftime(knowledge$EndDate[d], format = "%j", tz="UTC"))
        if (startDate <= salesPerDayDataset$dayOfYear[a] & endDate >= salesPerDayDataset$dayOfYear[a] ){
          if (knowledge$Type[d] == 'P'){
            salesPerDayDataset$sales[a] <- salesPerDayDataset$sales[a]/knowledge$KnowledgeQty[d]
          }
        }
      }
    }
  }  
 


#Forecasting:
#0. Outliers
#1. Calculate mean sales per week
#2. Seasonality
#3. Correct daily sales for seasonality
#4. Compute average last y weeks (moving average) per day
#5. Apply market knowledge


#0.
  #vector met alle sales waarden van afgelopen y weken
  #per dag van de week kijken of outliers
  #outliers waarde 1000 geven
  forecastNumberOfWeeks <- dataStorage$ForecastHistoryWeeks[i]
  salesForecastHistory <- rep(0, forecastNumberOfWeeks*7)
  #index of the last day in the salesPerDayDataset
  indexEndDay <- endDay-startDay+1
  outlierValue <- 1000
  forecastHistoryDayOfWeekStart <- salesPerDayDataset$dayOfWeek[indexEndDay-forecastNumberOfWeeks*7]
  for(b in 1:(forecastNumberOfWeeks*7)){
    salesForecastHistory[b] <- salesPerDayDataset$sales[indexEndDay-forecastNumberOfWeeks*7-1+b]
  }
  for(b in 1:7){
    salesPerDayOfTheWeek <- rep(0, forecastNumberOfWeeks)
    for (c in 1:forecastNumberOfWeeks){
      salesPerDayOfTheWeek[c] <- salesForecastHistory[7*(c-1)+b]
    }
    meanPerDay <- mean(salesPerDayOfTheWeek)
    sumNoOutliers <- 0
    for (c in 1:forecastNumberOfWeeks){
      if(salesPerDayOfTheWeek[c]>=(0.2*meanPerDay) & salesPerDayOfTheWeek[c]<=(1.8*meanPerDay)){
        sumNoOutliers <- sumNoOutliers + 1
      }
    }
    if (sumNoOutliers!=0){
      for (c in 1:forecastNumberOfWeeks){
        if(salesPerDayOfTheWeek[c]<(0.2*meanPerDay)){
          salesForecastHistory[7*(c-1)+b] <- outlierValue
        } else if (salesPerDayOfTheWeek[c]>(1.8*meanPerDay)){
          salesForecastHistory[7*(c-1)+b] <- outlierValue
        } 
      }
    }
  }

  

#1.
  #als geen outlierVaule dan gebruiken
  salesPerWeek <- rep(0, forecastNumberOfWeeks)
  salesOfOneWeek <- 0
  for (c in 1:forecastNumberOfWeeks){
    sumPerWeek <- 0
    numberOfElements <- 0
    for (d in 1:7){
      if(salesForecastHistory[d + (c-1)*7] != outlierValue){
        sumPerWeek <- sumPerWeek + salesForecastHistory[d + (c-1)*7]
        numberOfElements <- numberOfElements + 1
      }
      if (numberOfElements != 0){
        salesOfOneWeek <- sumPerWeek/numberOfElements
      }
    }
    salesPerWeek[c] <- salesOfOneWeek
  }
  

#2.
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
  


#3.
  #loop: voor elke week in forecastNumberOfWeeks: waarden van die week corrigeren voor seasonality door sales te delen door seasonality factor
  correctedSalesForecastHistory <- salesForecastHistory
  for(b in 1:(forecastNumberOfWeeks*7)){
    if (correctedSalesForecastHistory[b] != outlierValue){
      correctedSalesForecastHistory[b] <- correctedSalesForecastHistory[b]/seasonalityPerWeek[ceiling(b/7)]
    }
  }


#4.
  #sales of last forecastNumberOfWeeks per dag (gecorrigeerde waarde) en daarvan gemiddelde berekenen
  #forecastSalesPerDay is vector met voorspellingen, waarbij 1e entry de dag van de laatste inventory meting is 
  #forecastSales is vector met voorspellingen, waarbij 1e entry op maandag is 
  #forecastSalesFromStartDay is vector met voorspellingen, waarbij 1e entry de eerste dag is die voorspeld moet worden (=forecastStartDay)
  #met dag van de week gelijk aan daynumberEnd= forecastHistoryDayOfWeekStart
  forecastSalesPerDay <- rep(0, 7)
  forecastSales <- rep(0, 7)
  forecastSalesFromStartDay <- rep(0, 7)
  indexStartDate <- as.numeric(format(forecastStartDate,"%w", tz="UTC"))
  for(b in 1:7){
    forecastSalesPerDayOfTheWeek <- c()
    for (c in 1:forecastNumberOfWeeks){
      if (correctedSalesForecastHistory[7*(c-1)+b]!=outlierValue){
        forecastSalesPerDayOfTheWeek <- c(forecastSalesPerDayOfTheWeek, correctedSalesForecastHistory[7*(c-1)+b])
      }
    }
    if (length(forecastSalesPerDayOfTheWeek)==0){ #zou in principe niet moeten gebeuren, want als alle waarden outliers zijn, worden ze alsnog allemaal gebruikt
      meanSalesPerDay <- 0
    } else {
      meanSalesPerDay <- mean(forecastSalesPerDayOfTheWeek)
    }
    forecastSalesPerDay[b] <- meanSalesPerDay
    
    #forecastSales contains the forecast sales starting on Monday
    vectorIndex <- b+dayNumberEnd-1
    if (vectorIndex >7){
      vectorIndex <- vectorIndex - 7
    }
    forecastSales[vectorIndex] <- forecastSalesPerDay[b]
    
    #forecastSalesFromStartDay contains the forecast sales starting on the first day that has to be forecasted
    vectorIndexStart <- b-indexStartDate+dayNumberEnd
    if (vectorIndexStart <=0){
      vectorIndexStart <- vectorIndexStart + 7
    }
    if (vectorIndexStart >7){
      vectorIndexStart <- vectorIndexStart - 7
    }
    forecastSalesFromStartDay[vectorIndexStart] <- forecastSalesPerDay[b]
  }
  


#5.
  #als marktkennis voor deze datum
  #if type==percentage, if type==absolute
  #dan salesForecast aanpassen
  knowledge <- subset(dataKnowledge, dataKnowledge$StorageID==ID)
  knowledge <- knowledge[order(knowledge$StartDate),]
  dayStartDate <- as.numeric(strftime(forecastStartDate, format = "%j", tz="UTC"))
  if(nrow(knowledge)>0){
    for (d in 1:nrow(knowledge)){
      for(b in 0:6){
        startDate <- as.numeric(strftime(knowledge$StartDate[d], format = "%j", tz="UTC"))
        endDate <- as.numeric(strftime(knowledge$EndDate[d], format = "%j", tz="UTC"))
        if (startDate <= (dayStartDate+b) & endDate >= (dayStartDate+b) ){
          if (knowledge$Type[d] == 'P'){
            forecastSalesFromStartDay[b+1] <- forecastSalesFromStartDay[b+1]*knowledge$KnowledgeQty[d]
          }
          if (knowledge$Type[d] == 'A'){
            forecastSalesFromStartDay[b+1] <- knowledge$KnowledgeQty[d]
          }
        }
      }
    }
  }
  
  
  #loop maken van voorspellingsdag tot voorspellingsdag +7
  #tabel steeds rbind om forecast tabel te maken
  #salesForecast maken vector maandag t/m vrijdag
  #salesForecastCorrected met marktkennis
  
  for(b in 1:7){
    datasetCalculatedSalesForecast <- rbind(datasetCalculatedSalesForecast, list(storage=ID, dayOfYear=(dayStartDate+b-1), sales=forecastSalesFromStartDay[b]))
  }
  

}


#make csv file of forecast
#write.table(datasetCalculatedSalesForecast, file = "G:\\My Drive\\Graduation project\\Inventory run-out\\Forecast_Sales_R_Test_table.csv", append = FALSE, 
#            sep = ";", dec = ",", col.names = NA, qmethod = "double")
write.csv2(datasetCalculatedSalesForecast,"G:\\My Drive\\Graduation project\\Inventory run-out\\Forecast_Sales_R_28_04.csv", row.names = FALSE)



#Verschillen:
# outliers alleen door 80% en niet ook of 2 keer standaard deviatie van gemiddelde af
# als alle waarden outliers, dan mean of de week is 0
# als alle waarden outliers, dan mean voor moving average is 0 (ook bij Marcel volgens mij)
#inventory wordt genomen vanaf laatste meting, tot NumberForecastHistoryWeeks geleden en dan de laatste meting daarvoor. 

#knowledge werkt nu alleen voor 1 jaar door startdatum dagen tellen

#test dag van de week
#print(t1[j])
#dayNumberStart <- as.numeric(format(t1[j],"%w", tz="UTC"))
#print(dayNumberStart)


