##This file is the data viewer. You must first run the simulation
##using run simulation.r
##this creates the dataFile (the object created by drug  model)
##and outputFrame (the results of the simulation)


## Pull Out annual number of patient totals
## All non-data columns must be at the begininng of the dataframe
getAnnualSales <- function(outputFrame, nonDataCols, dataFile){
  yearStart <-  0:(dataFile$getData("User Variable", "End time (year)") - 1) * dataFile$period(T)
  
  outColsNames <- colnames(outputFrame)[1:nonDataCols]
  outColsNames <- append(outColsNames, as.character(1:length(yearStart)))
  years <- dataFile$getData("User Variable", "Start time (year)"):dataFile$getData("User Variable", "End time (year)")
  dataVector <- numeric()
  nonDataVector <- character()
  getAnnual <- function(drugSales){
    yearStart <-  0:(dataFile$getData("User Variable", "End time (year)") - 1) * dataFile$period(T)
    result <- sapply(yearStart, function(x){
      curRange <- x + 1:dataFile$period(T)
      result <- sum(drugSales[curRange])
      result
    })
    result
  }

  for (curDrugName in row.names(outputFrame)){
    curDrugSales <- outputFrame[curDrugName,  (nonDataCols + 1):length(outputFrame)]
    curAnnSales <- getAnnual(curDrugSales)
    nonDataVector <- append(nonDataVector, as.character(outputFrame[curDrugName, 1:nonDataCols]))
    dataVector <- append(dataVector, curAnnSales)    
  }
  nonData <- as.data.frame(matrix(data = nonDataVector, nrow = nrow(outputFrame), byrow = T))
  DataDF <- as.data.frame(matrix(data = dataVector, nrow = nrow(outputFrame), byrow = T), stringsAsFactors = F)
  byYear <- cbind(nonData, DataDF)
  colnames(byYear) <- outColsNames
  byYear
}
####

## Create a list with 2 data frams, a mean and a SD for sales of each drug
getMeanSales <- function(outputFrame, nonDataCols, drugNameCol, dataFile){
  drugNames <- unique(outputFrame[,drugNameCol])
  meanSalesVector <- numeric()
  SDSalesVector <- numeric()
  nonData <- character()
  print(drugNames)
  for (curDrugName in drugNames){
    
    ## Calculate the $$ each simulated patient is "worth"
    curPrice <- dataFile$getData(curDrugName, "Price per day") * 365/dataFile$period(T)
    populationMod <- dataFile$getData("User Variable", "Number of Cases") / dataFile$getData("User Variable", "Number of simulated patients")
    
    ##Partition data
    curFrame <- subset(outputFrame, drugNames == curDrugName)
    dataCols <- (nonDataCols + 1):(length(curFrame))
    
    ##Create the data
    curMeanData <- apply(curFrame[,dataCols], 2, mean) * populationMod * curPrice
    curSDData <- apply(curFrame[,dataCols], 2, sd) * populationMod * curPrice
    
        
    #print(curMeanData)
    ## Append into the data frame
    meanSalesVector <- append(meanSalesVector, curMeanData)
    SDSalesVector <- append(SDSalesVector, curSDData)
    nonData <- append(nonData, curFrame[1,1:nonDataCols])
    
  }
  
  meanDF <- as.data.frame(matrix(data = meanSalesVector, nrow = length(drugNames), byrow = T))
  SDDF <- as.data.frame(matrix(data = SDSalesVector, nrow = length(drugNames), byrow = T))
  nonDataDF <- as.data.frame(matrix(data = nonData, nrow = length(drugNames), byrow = T))
  row.names(meanDF) <- drugNames
  row.names(SDDF) <- drugNames
  
  ## Package into a list
  list(
    mean = meanDF,
    sd = SDDF )
  
}

## Create a list of 2 data frame, mean and SD for patient share
getMeanPS <- function(outputFrame, nonDataCols, drugNameCol, dataFile){
  print("hello world")
  drugNames <- dataFile$getDrugNames()
  dataCols <- (nonDataCols+1):length(outputFrame)
  ## Take outputFrame and normalize for each simulation
  ## make a list with a DF for each simulation having only Data
  simulations <- unique(outputFrame$simulation)
  patientShares <- list()
  print("About to create DFs")
  for (curSim in simulations){
    curSimDF <- subset(outputFrame, simulation==curSim)
    curSimDF$simulation <- NULL
    row.names(curSimDF) <- unique(curSimDF$drugNames)
    curSimDF$drugNames <- NULL
    
    curSum <- apply(curSimDF, 2, sum)
    
    patientShares <- append(patientShares, list(curSimDF))
  }
  print("created DFs")
  meansVec <- numeric()
  sdVec <- numeric()
  for (curDrug in drugNames){
    unProcVec <- numeric()
    
    ## Pull out current drug's values from each DF
     for (curDF in patientShares){
        unProcVec <- append(unProcVec, as.numeric(curDF[curDrug,]))
     }
    curDrugMat <- matrix(data = unProcVec, nrow = length(patientShares), byrow = T)
    curDrugMean <- apply(curDrugMat, 2, mean)
    curDrugSD <- apply(curDrugMat, 2, sd)
    meansVec <- append(meansVec, curDrugMean)
    sdVec <- append(sdVec, curDrugSD)
    
  }
  
  sdDF <- as.data.frame(matrix(data = sdVec, nrow = length(drugNames), byrow = T))
  row.names(sdDF) <- drugNames
  
  meanDF <- as.data.frame(matrix(data = meansVec, nrow = length(drugNames), byrow = T))
  row.names(meanDF) <- drugNames
  
  ##Normalize to be fraction of total
  sumOfRows <- apply(meanDF, 2, sum)
  meanDF <- sweep(meanDF, 2, sumOfRows , '/')
  sdDF <- sweep(sdDF, 2, sumOfRows, '/')
  
  list(
    mean = meanDF,
    sd = sdDF
    )
}


###Create a plot for a list containing mean and SD
plotMeanSD <- function(myData, names){
  colorPal <- rainbow(nrow(myData[["mean"]]))
  counter <- 1
  meanDF <- myData[["mean"]]
  sdDF <- myData[["sd"]]
  namesToUse <- names
  #namesToUse <- row.names(meanDF)
  
  plot(0,0, ylim=range(meanDF + sdDF), xlim = c(1,length(meanDF)))
  
  for (i in namesToUse){
   
    avg <- meanDF[i,]
    sdev <- sdDF[i,] 
    x <- 1:length(meanDF)
    
    points(x, avg,
           type = "l", lwd = 3, col = colorPal[counter]
    )
    
    ## from some helpful devil on stack exchange:
    # hack: we draw arrows but with very special "arrowheads"
    ## aalso, gget rig of any columns that have 0s as this generates a warning
    x <- x[sdev > 0]
    avg <- avg[sdev > 0]
    sdev <- sdev[sdev > 0]
    arrows(x, as.numeric(avg-sdev), x, as.numeric(avg+sdev), length=0.05, angle=90, code=3)
    counter <- counter + 1
  }
  legend("topleft", legend = namesToUse, col= colorPal,  lwd = 3)
  ####
}

##Plots all rows corresponding to the drug name
plotallRuns <- function(myData, nonDataCols){
  curFrame <- myData
  colorPal <- rainbow(nrow(curFrame))
  dataCols <- (nonDataCols + 1):(length(curFrame))
  plot(0,0, ylim = range(curFrame[,dataCols]), xlim = c(1, length(curFrame) - nonDataCols))
  for (curRow in 1:nrow(curFrame)){
     curData <- as.numeric(curFrame[curRow, dataCols])
     xAxis <- 1:length(curData)
     points(xAxis, curData, type = "l", lwd = 3, col = colorPal[curRow])
  }
}


## Add a column of the drug names
addDrugNames <- data.frame(row.names = row.names(outputFrame))
addDrugNames$drugNames <- dataFile$getDrugNames()
addDrugNames <- cbind(addDrugNames, outputFrame)



annualSales <- getAnnualSales(addDrugNames, 2, dataFile)
summarySales <- getMeanSales(annualSales, 2, "drugNames", dataFile)
summaryPS <- getMeanPS(annualSales, 2, "drugNames", dataFile)
plotMeanSD(summaryPS, c("Remicade", "Humira", "Azathioprine", "Cimiza"))
plotMeanSD(summaryPS, row.names(summaryPS$mean))

## Save Data
toSave <- list(
  dataFile = dataFile,
  rawData = addDrugNames,
  annualizedRaw = annualSales,
  #Sales = summarySales,
  PatientShare = summaryPS,
  description = dataFile$getData("User Variable", "Description")
  
  )
 outFileName <- paste("~/Patient Simulator/version 2/", dataFile$getData("User Variable", "File Name"))
 dput(toSave, "~/Patient Simulator/version 2/Time to fail.dat")


## View RawData from a single useData
#drugNames <- row.names(drugUse)
#colorPal <- rainbow(length(drugNames))
#counter <- 1
#plot(0,0, ylim = range(drugUse), xlim = c(1, length(drugUse)))
#for (curDrug in row.names(drugUse)){
#  points(1:length(drugUse), drugUse[curDrug,], col = colorPal[counter], type = "l", lwd = 3)
#  counter <- counter + 1
#}
#legend("topleft", legend = drugNames, col= colorPal,  lwd = 3)


