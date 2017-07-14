
##This function creates a patient object, 
##which has properties and methods for an individual patient
##it is called from the an object created by createPatients
##which is the scope drugList is created in
patient <- function(time, drugList = drugList){

  age <- 1 ##not using age at the moment
  drugHistory <- list()  ##Updatable list of all drugs the patient has used
  
  ##makeNewRecord creates a drug record. in this case the patient is initialized as "Untreated" 
  drugCurrent <- list(drugList$makeNewRecord("Untreated", time))
  #drugProbabilities <- drugProbs
  dead <- FALSE
  diedOn <- NULL
  lifeSpan <- drugList$getData("User Variable", "Average Patient Time (years)")
  
  ## Variables for penalizing the used drugs
  ## keeps an ongoing list of modifiers to the probability for each d
  penalty <- drugList$getData("User Variable", "Reuse Penalty")
  probabilityMods <- numeric(length(drugList$getDrugNames())) ##This part appears wrong!
  probabilityMods <- probabilityMods + 1                      ##Should be init as 1
  names(probabilityMods) <- drugList$getDrugNames()
  probabilityMods <- probabilityMods[order(names(probabilityMods))]
  
  ## Updates a patient's record; this is the main method called during the simulation
  updatePatient <- function(time){
    age <<- age + drugList$period(T)
    if (!dead) {

      for (curDrug in drugCurrent){
        ## Go through the relevant functions to check for discontinue
        ## this allows multiple functions to be used for determining
        ## when a paitent fails on a drug; functions simply return T (discontinue) or F (don't)
        ## the function is defined when the drug model is initatied
        for (curFun in curDrug$drugFuns){
          if (curFun(time)){ curDrug$discontinue(time)} 
        } ## End of function tesing 
        
        ## If a drug is discontinued, remove it from current therapy, move it to history,
        ## update probabilities by multiplying by the appropriate penalty
        ## Pick a new drug
        if (curDrug$isDiscontinued()){
          curName <- curDrug$drugName()
          drugCurrent[[curName]] <<- NULL
          drugHistory <<- c(drugHistory, list(curDrug))
          probabilityMods[curName] <<- probabilityMods[curName] * penalty
          drugProbabilities <- recalcProbs(time)
          drugToAdd <-pickDrug(drugProbabilities)[[1]]
          drugCurrent <<- list(drugList$makeNewRecord(drugToAdd, time))
        }
      }## end of drug
      
        
    ## Check to see if the patient died (defined below)
    if (willDie()){
      dead <<- TRUE
      diedOn <<- time
      for (curDrug in drugCurrent){
        curDrug$discontinue(time)
        drugHistory <<- c(drugHistory, list(curDrug))
      }
      drugCurrent <<- list(drugList$makeNewRecord("Dead", time))
    }
   } ## End of if not Dead 
  }## end of update patient
  
  ## pickDrug returns a drug (not just the name, but the drug's info) to be an input to makee a 
  ## new drug record  
  ## This function requires the list drugList to be available  
  pickDrug <- function(drugProbabilities){
   # print("Picking New Drug")
    drugProbabilities <- sort(drugProbabilities)
    test <- runif(1,0,1)
    cumulative <- 0
    #print(test)
    foundIt <- FALSE
    for(curDrugNum in 1:length(drugProbabilities)){
      curDrug <- drugProbabilities[curDrugNum]
      cumulative <- cumulative + curDrug
      
      names(cumulative) <- names(curDrug)
    #  print(paste("CUmulative:", cumulative, test)
     
      if (cumulative > test & !foundIt) {
        drugToAdd <- names(curDrug)
        foundIt <- TRUE
      }
    }
    return(drugToAdd)
  }
  
  ## requires variable lifeSpan 
  ## returns true/false
  willDie <- function(){
    test <- rpois(1, 1/(lifeSpan/drugList$period()))
    if (test > 0 ){
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
  
  
  ## This sets the probablilies of drugs being picked asa next therapy
  ## it is recalculated based on time in the model so that a drug might only become available 
  ## sometime after the model begins.
  ## The function getProbablility is part of the drug model, more details are found there
  ## This part just applies the use modifiers and makes sure they add up to 1 
  recalcProbs <- function(time, penalty){
    drugProbabilities <- drugList$getProbability(time) * probabilityMods
    
    modifier <- 1/sum(drugProbabilities)
    drugProbabilities <- drugProbabilities * modifier
    remainder <- 1-sum(drugProbabilities)
    drugProbabilities[1] <- drugProbabilities[1] + remainder
    #print(drugProbabilities)
    return(drugProbabilities)
  }
  
  ##Returns the drug history of the patient; Useful for post-run analysis
  getHistory <- function(){
    histDF <- data.frame(drug = character(), from = numeric(), to = numeric())
    for (curHist in drugHistory){
       curName <- curHist$drugName()
       beganOn <- curHist$getbeganOn()
       discontinued <- curHist$discontinuedOn()
       histDF <- rbind(histDF, data.frame(drug = curName, from = beganOn, to = discontinued))
    }
    histDF
  }
  
  ##Returns the current drug(s) the patient is on
  getCurrent <- function(){
    curDF <- data.frame(drug = character(), beganOn = numeric())
    for (curHist in drugCurrent){
      curName <- curHist$drugName()
      beganOn <- curHist$getbeganOn()
      curDF <- rbind(curDF, data.frame(drug = curName, beganOn = beganOn))
    }
    curDF
  }
  
  ##Returns the object
  output <- list(
    updatePatient = updatePatient,
    pickDrug = pickDrug,
    willDie = willDie,
    getcurDrug = getCurrent,
    getHistory = getHistory,
    isDead = function(){dead},
    TimeOfDeath = function(){diedOn},
    curProbs = function(time){recalcProbs(time)}
  )
  return(output)
}









## Initiate a list of patients
## drugFrame is the object created by druggList in Drug Model.r
## it is passed from the run simulation script
## it is this object that the simulator interacts with
## which has a list of patient objects created by the patient function
createPatients <- function(numOfPatients, drugFrame){
  output <- list()
  if (numOfPatients < 1){return(NULL)}
  for (i in 1:numOfPatients){
    curPatient <- patient(1, drugFrame)
    
    output <- c(output, list(curPatient))
  }
  
  
  ### Gets a list of drugs and how many patients are on each
  ### for output to the simulator
  getDrugs <- function(){
    outFrame <- data.frame(row.names = drugFrame$getDrugNames())
    outFrame[,1] <- 0
    for (curPatient in output){
      curDrugs <- as.character(curPatient$getcurDrug()$drug)
      for (curDrug in curDrugs){
         if (curDrug != "Dead") {outFrame[curDrug,1] <- outFrame[curDrug,1] + 1}
      }      
    }
    outFrame
  }
  
  ##runs updata on all patients
  upDatePatients <- function(time){
    for (curPatient in output){
      curPatient$updatePatient(time)
    }
  }
  
  ##Returns the number of living (not dead) patients
  living <- function(){
    result <- 0
    for (curPatient in output){
      if (!curPatient$isDead()){result <- result + 1}
    }
    result
  }
  
  ##Adds the specified number of new patients
  addMore <- function(numToAdd, time){
    if (numToAdd > 0){
     for (i in 1:numToAdd){
      output <<- c(output, list(patient(time, drugFrame)))
     }
    }
  }
  

  
  list(
    getDrugs = getDrugs,
    update = upDatePatients,
    getPatient = function(patientNum){output[[patientNum]]},
    living = living,
    addPatients = addMore,
    numOfRecords = function(){length(output)},
    allPatients = function(){output}
    )
  
}

