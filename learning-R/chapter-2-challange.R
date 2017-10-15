  pollutantmean <- function(directory, pollutant, id=1:332){
    current_mean = 0
    mean_stack = c()
    
    for (index in id){
      current_csv_mean = 0
      name_archive = index
      
      if(index < 10)
        name_archive = paste0("00",index)
      else if(index >= 10 && index < 100)
        name_archive = paste0("0", index)
      
      current_file = paste0(directory ,"/",name_archive, ".csv", collapse ='')
      pollution = read.table(current_file, header = TRUE, fill = TRUE, sep=",")
      
      if(pollutant=="sulfate"){
        non_na = complete.cases(pollution[,2])
        current_csv_mean = pollution[,2][non_na]
      }
      else if(pollutant=="nitrate"){
        non_na = complete.cases(pollution[,3])
        current_csv_mean = pollution[,3][non_na]
      }
      
      else{
        cat("Pollutant not recognized")
        break
      }
      
      mean_stack = c(mean_stack, current_csv_mean)
    }
    print(mean(mean_stack))
  }
  
  complete <- function(directory, id=1:332){
    complete_cases_directory = data.frame(x= numeric(0), y= integer(0))
    
    for (index in id){
      count_cases = 0
      name_archive = index
      
      if(index < 10)
        name_archive = paste0("00",index)
      else if(index >= 10 && index < 100)
        name_archive = paste0("0", index)
      
      current_file = paste0(directory ,"/",name_archive, ".csv", collapse ='')
      print(current_file)
      pollution = read.table(current_file, header = TRUE, fill = TRUE, sep=",")
      
      count_cases = nrow(pollution[complete.cases(pollution),])
      id = mean(pollution[,4])
      current_line = data.frame(id, count_cases)
      complete_cases_directory = rbind(complete_cases_directory, current_line)
      
    }
    print(complete_cases_directory)
  }
  
  corr <- function(directory, full_limiar=0){
    correlation_vector = c()
    
    for (index in 1:332){
      name_archive = index
      
      if(index < 10)
        name_archive = paste0("00",index)
      else if(index >= 10 && index < 100)
        name_archive = paste0("0", index)
      
      current_file = paste0(directory ,"/",name_archive, ".csv", collapse ='')
      pollution = read.table(current_file, header = TRUE, fill = TRUE, sep=",")
      
      clean_pollution_data = pollution[complete.cases(pollution),]
      
      if(nrow(clean_pollution_data) > full_limiar){
        #correlation_vector = c(correlation_vector, transform(clean_pollution_data[,3]/clean_pollution_data[,2])[,1])
        correlation_vector = c(correlation_vector, cor(clean_pollution_data[,3], clean_pollution_data[,2]))
        #print(clean_pollution_data[,3])
        #print(clean_pollution_data[,2])
        #print(max(transform(clean_pollution_data[,2]/clean_pollution_data[,3])[,1]))
      }
    }
    print(correlation_vector)
  }
  
  
  
  