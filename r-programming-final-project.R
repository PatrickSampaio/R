hospital_dataset = read.csv("/home/patrick/Downloads/R/dataset/hospital-dataset/outcome-of-care-measures.csv"
                            ,colClasses = "character")

VALID_STATES = unique(hospital_dataset["State"][,1])

VALID_OUTCOMES = c('Pneumonia', 'Heart Failure', 'Heart Attack')

BuildHistogram = function(){
  histogram_dataset =  as.numeric(hospital_dataset[, 11])
  hist(histogram_dataset)
}

GetHospitalPerState = function(nameState){
  return(subset(hospital_dataset, hospital_dataset$State==nameState))
}

ValidateArguments= function(nameState, outcomeName){
  if(!nameState%in%VALID_STATES){
    print("Invalid State")
    return(FALSE)
  }
  
  if(!outcomeName%in%outcomeName){
    print("Invalid outcome")
    return(FALSE)
  }
  return(TRUE)
}

BestHospital = function(nameState, outcomeName){
  
  if(ValidateArguments(nameState, outcomeName)==FALSE)
    return()
  
  state_data = GetHospitalPerState(nameState)
  hospital = ""
  
  if(outcomeName == "Heart Attack")
    hospital = GetLowestHeartAttack(state_data)
  
  if(outcomeName == "Pneumonia")
    hospital = GetLowestPneumonia(state_data)
  
  if(outcomeName == "Heart Failure")
    hospital = GetLowestHeartFailure(state_data)
  
  return(hospital)
}

RankHospital = function(state, outcome, rankOptions){
  ValidateArguments(state, outcome)
  
  state_data = GetHospitalPerState(state)
  outcome_data = ""
  
  if(outcome = "Heart Attack"){
      outcome_data = GetLowerHeartAttackList(state_data)
  }
  
  if(outcome = "Pneumonia"){
      outcome_data = GetLowerHeartAttackList(state_data)
  }
  
  if(outcome = "Heart Failure"){
      outcome_data = GetLowerHeartAttackList(state_data)
  }
  
  print(outcome_data)
}

GetLowerHeartAttackList = function(state_date){
  list_heartattack_rates = state_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  list_heartattack_rates = as.numeric(list_heartattack_rates)
  
  filtered_heartattack_rates = list_heartattack_rates[complete.cases(list_heartattack_rates)]
  return(filtered_heartattack_rates)
}

GetLowerPneumoniaList = function(state_data){
  list_pneumonia_rates = state_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  list_pneumonia_rates = as.numeric(list_pneumonia_rates)
  
  filtered_pneumonia_rates = list_pneumonia_rates[complete.cases(list_pneumonia_rates)]
  return(filtered_pneumonia_rates)
}

GetLowerHeartFailureList = function(state_data){
  list_heartfailure_rates = state_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  list_heartfailure_rates = as.numeric(list_heartfailure_rates)
  
  filtered_heartfailure_rates = list_heartfailure_rates[complete.cases(list_heartfailure_rates)]
  return(filtered_heartfailure_rates)
}

GetLowestHeartAttack = function(state_data){
  filtered_heartattack_rates = GetLowerHeartAttackList(state_date)
  minimal_heartattack_rate = format(min(filtered_heartattack_rates), nsmall=1)
  
  best_hospital = subset(state_data, 
                         as.character(state_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                         ==minimal_heartattack_rate)
  
  return(best_hospital$Hospital.Name)
}

GetLowestPneumonia = function(state_data){
  filtered_pneumonia_rates = GetLowerPneumoniaList(state_data)
  
  minimal_pneumonia_rate = format(min(filtered_pneumonia_rates), nsmall=1)
  
  best_hospital = subset(state_data, 
                         as.character(state_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                         ==minimal_pneumonia_rate)
  
  return(best_hospital$Hospital.Name)
}

GetLowestHeartFailure = function(state_data){
  filtered_heartfailure_rates = GetLowerHeartFailureList(state_data)
  minimal_heartfailure_rate = format(min(filtered_heartfailure_rates), nsmall=1)
  
  best_hospital = subset(state_data, 
                         as.character(state_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                         ==minimal_heartfailure_rate)
  
  return(best_hospital$Hospital.Name)
}