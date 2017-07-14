
##Point this to the location of the .csv file with drug information
drugFileLocation <- '~/Patient Simulator/CD-Drugs.csv'

##point this directory to the drug models and patient models .r files
directory <- '~/Patient Simulator/version 2/'


source(paste(directory, 'Drug Model.r', sep = ""))
source(paste(directory, 'Patient Models.r', sep = ""))

##In Drug Model.r file: reads in the input file and creates an object containing all 
## Drug info, and functions for calculating 
dataFile <- initializeDrugs(drugFileLocation)

##the function getData returns lines of data from the datafile that are stored in the object
##general format is "drug name" OR "User Variable", "Parameter Name"
##The parameters and parameter names themselves are determined in the csv file.
numOfPatients <- dataFile$getData("User Variable", "Number of simulated patients")

##Now creates a patient model containing the number of patients created and the data to create drug 
##objects that get assigned to the patients
patientList <- createPatients(numOfPatients, dataFile)

startYear <- dataFile$getData("User Variable", "Start time (year)")
endYear <- dataFile$getData("User Variable", "End time (year)")
numOfReps <- (1 + endYear - startYear) * dataFile$period(T)
popGrowth <- dataFile$getData("User Variable", "Population Growth Rate") * dataFile$period()

##This dataframe actually stores the data
drugUse <- data.frame(row.names = dataFile$getDrugNames())
numOfSims <- 10


outputFrame <- data.frame(row.names = dataFile$getDrugNames())
for (simNum in 1:numOfSims){
  
  ## Initiate round
  patientList <- createPatients(numOfPatients, dataFile) 
  curTargetNum <- numOfPatients
  startTime <- Sys.time()
  drugUse <- data.frame(row.names = dataFile$getDrugNames())
  drugUse$simulation <- simNum
  
  for (time in 1:numOfReps){
    
    ## Update Patients
    
    patientList$update(time)
    
    
    ## Decide how many to add    
    curTargetNum <- curTargetNum + curTargetNum * popGrowth
    toAdd <- curTargetNum - patientList$living()
    
    
    
    patientList$addPatients(toAdd, time)
    
    ## How many on each drug
    drugUse <- cbind(drugUse, patientList$getDrugs())
      
    
  }
  
  outputFrame <- rbind(outputFrame, drugUse)
  
  ## Calculate timing  
  endTime <- Sys.time()  
  print(paste("Round", simNum,  "Completed in", endTime - startTime))
  
}

