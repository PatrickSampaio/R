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

RankAllHospitals = function(outcome, index){
 hospitals_outcome_state = RankHospitalDataSet(hospital_dataset, outcome)
 hospital_states = sort(unique(hospitals_outcome_state["State"][,1]))

 current_outcome_df = data.frame()
 
 for(state in hospital_states){
    hospitals_current_state = subset(hospitals_outcome_state, hospitals_outcome_state$State==state)
    current_state = hospitals_current_state[,3][[1]]
    current_hospital = hospitals_current_state[,1][[1]]
    
    hospitals_names = hospitals_current_state[,1]
    
    if(is.numeric(index)){
      if(length(hospitals_names) < index)
        print("RankNumber is beyound the hospitals quantities")
      else
        current_hospital = hospitals_names[index]
    }
    if(c(rank)%in%"worst")
      current_hospital = hospitals_names[length(hospitals_names)]
    if(c(rank)%in%"best")
      current_hospital = hospitals_names[1]
    
    print("-----------------------------------------------------------")
    print(current_state)
    print(hospitals_current_state[,1])
    print("-----------------------------------------------------------")

    best_hospital_state = data.frame(current_hospital, current_state)
    names(best_hospital_state) = c("Hospital.Name", "State")
    current_outcome_df = rbind(current_outcome_df, best_hospital_state)
 }
 print(current_outcome_df)
}

RankHospitalDataSet = function(hospitals_selecteds, outcome){
 hospitals_per_state = ""
 if(outcome == "Heart Attack"){
    outcome_data = GetLowerHeartAttackList(hospitals_selecteds)
    hospitals_per_state = subset(hospitals_selecteds, select=c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "State"))
    
    hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack =
      as.numeric(hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    
    hospitals_per_state = hospitals_per_state[order(decreasing = FALSE, 
                                                    hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                                    hospitals_per_state$Hospital.Name),]
  }
  
  if(outcome == "Pneumonia"){
      hospitals_per_state = subset(hospitals_selecteds, select=c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", "State"))
      
      hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia =
        as.numeric(hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      
      hospitals_per_state = hospitals_per_state[order(decreasing = FALSE, 
                                                      hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                                      hospitals_per_state$Hospital.Name), ]
  }
  
  if(outcome == "Heart Failure"){
      hospitals_per_state = subset(hospitals_selecteds, select=c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "State"))
      
      hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure =
        as.numeric(hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      
      hospitals_per_state = hospitals_per_state[order(decreasing = FALSE, 
                                                      hospitals_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                                      hospitals_per_state$Hospital.Name), ]
  }
  return(hospitals_per_state)
}

RankHospital = function(state, outcome, rankOptions){
  ValidateArguments(state, outcome)
  rank = rankOptions
  
  state_data = GetHospitalPerState(state)
  hospitals_per_state = ""
 
  hospitals_per_state = RankHospitalDataSet(state_data, outcome)  
    
  rank_list = c(1:length(hospitals_per_state[,2]))
  hospitals_per_state$rank = rank_list
  print(rank)
  
  hospitals_names = hospitals_per_state[[1]]
  print(hospitals_per_state)
  
  if(is.numeric(rank)){
    if(length(hospitals_names) < rank)
      print("RankNumber is beyound the hospitals quantities")
    else
      print(hospitals_names[rank])
  }
  if(c(rank)%in%"worst")
    print(hospitals_names[length(hospitals_names)])
  if(c(rank)%in%"best")
    print(hospitals_names[1])
  
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
