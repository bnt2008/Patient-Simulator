## Make a list with all of the drug data frames
## Read the file and create the master drug Frame
initializeDrugs <- function(fileName){
  rawDrugFrame <- read.csv(fileName, header = T, stringsAsFactors = F)
  colnames(rawDrugFrame) <- c("Name", "dataType", "Data")
  drugFrame <- list()
  
  ## Get the names of the drugs
  
  drugList <- unique(rawDrugFrame$Name)
  for (curDrug in drugList){
    curDrugFrame <- subset(rawDrugFrame, Name == curDrug)
    curDrugFrame <- list(curDrugFrame)
    names(curDrugFrame) <- curDrug
    drugFrame <- c(drugFrame, curDrugFrame)
  }
  
  drugNames <- drugList[drugList != "User Variable"]
  
  ## Create the special entry for Dead; this is treated as a drug in terms of assignment
  ## but signals that the patient should no longer be updated, and ensures they 
  ## are no longer counted to drug totals
  curDrugFrame <- drugFrame[[1]]
  curDrugFrame$Name <- "Dead"
  curDrugFrame$Data <- 0
  curDrugFrame <- list(curDrugFrame)
  names(curDrugFrame) <- "Dead"
  drugFrame <- c(drugFrame, curDrugFrame)  
  
  
  ##Create Functions to retrieve data
  getData <- function(drugName, fieldText, column = "Data"){
     curFrame <- drugFrame[[drugName]]
     curVal <- curFrame[curFrame$dataType == fieldText, column]
     if (!is.na(suppressWarnings(as.numeric(curVal)))){curVal <- as.numeric(curVal)}
     curVal
  }
  
  getDrugNames <- function(){drugNames}
  
  makeDrugProbability <- function(){ 
    ##Creates a function that outputs the drug probabilities for a given time
    ##Thus, an unlaunched drug will be set to 0 and others scaled to equal 1
    ##Probabilities are calculated on creation and stored in a list where index 
    ##equates to time. All lists are ordered by name before output
    
    period <- 1/getData("User Variable", "Number of periods per year")
    start <- getData("User Variable", "Start time (year)")
    end <- getData("User Variable", "End time (year)") * 1/period
    listOfProbs <- list()
    
    
    
    ##Create a list of named vectors with the probabilities calculated for each drug at each time
    for (time in start:end){
      
      
      ##Create a data frame with the names, probabilities entered by user and launch dates
      ## This does need to be reset each round
      names <- getDrugNames()
      probabilities <- sapply(names, function(x){getData(x, "Chance of Use First Line")})      
      launchDates <- sapply(names, function(x){getData(x, "Launched On")})    
      probDF <- data.frame(name = names, launchDate = launchDates, probability = probabilities)
      
      
      ## Set the probability of those not yet launched to 0
      probDF[time <= probDF$launchDate / period, "probability"] <- 0
    
      ## scale to 1
      multiplier <- 1/sum(probDF$probability)  
      probDF$probability <- probDF$probability * multiplier
    
      ##Make sure it adds to 1
      remainder <- 1- sum(probDF$probability)
      probDF <- probDF[order(probDF$probability, decreasing = T),]
      probDF$probability[1] <- probDF$probability[1] + remainder
    
      output <- probDF$probability
      names(output) <- probDF$name
      
      ## Put them back in order by names
      output <- output[order(names(output))]
      
      listOfProbs <- append(listOfProbs, list(output))
     }
     function(time){listOfProbs[[time]]}
  }
  
  ##Functions to calculate win or lose
  ## These can be any functions that take a parameter, and retun a function that 
  ## takes the argument currect time, but do not need to use it, returns True or False
  ## Drug Will be discontinued if the function returns TRUE
  ## THese functions are then added to the drugRecord object below
  Pfail1fun <- function(parameter, beganOn){
    output <- function(time){
      if (time - beganOn == 1)
      {
        test <- runif(1,0,1)
        #print(paste("Primary Fail=", test))
        return(test < parameter)
      } else {
        return(FALSE)
      }
    }
    return(output)
  }
  
  T2failfun <- function(parameter){
    output <- function(time){
      test <- rpois(1, 1/(parameter))
      #print(paste("Secondary Fail", test))
      if (test == 0) {return(FALSE)} else {return(TRUE)}
    }
  }
  
  
  ## Function to output a new drug record to give to a patient\
  ## This object will have properties and methods needed for a drug
  ## and is accessed by the patient object to check whether it 
  ## has been discontinued
  ## Time in this case is the time the drug is started
  drugRecord <- function(drugName, time){
    beganOn <- time
    discontinued <- FALSE
    discontinuedOn <- NULL
    
    discontinue <- function(time){
      discontinued <<- TRUE
      discontinuedOn <<- time
    }
    
    isDiscontinued <- function(){
      if (discontinued){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    
    
    pfail1Param <- getData(drugName, "Chance of Primary Failure")
    pfail2Param <- getData(drugName, "Time to Secondary Failure (years)") * getData("User Variable", "Number of periods per year")
 
    ##Here is where you insert the functions to determine failure.
    ##When the patient checks to see if it discontinues current therapy
    ##it sill test each funtion with the parameter time (time of current cycle)
    ##if any function returns true, patient will discontinue
    drugFuns <- list(
      fOne = Pfail1fun(pfail1Param, beganOn),
      fTwo = T2failfun(pfail2Param)
    )
    
    ##Creates the actual drugRecord object
    output <- list(
      drugName = function(){unique(drugName)},
      drugFuns = drugFuns,
      getbeganOn = function(){beganOn},
      discontinue = discontinue,
      discontinuedOn = function(){discontinuedOn},
      isDiscontinued = function(){
        if (discontinued){
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    )
    return(output)
  }
  
  
  ## Some important User Variables; for time saving
  period <- 1/getData("User Variable", "Number of periods per year")

##outputs the drugList object, which is the object the simulation interacts with
 list(
   getDrugNames = getDrugNames,
   getData = getData,
   getProbability = makeDrugProbability(),
   makeNewRecord = drugRecord,
   period = function(invert = FALSE){if (invert){ 1/period}else{period}},
   userVars = function(){drugFrame[["User Variable"]]},
   drugAttributes = function(drugName){drugFrame[[drugName]]}
   )
}


