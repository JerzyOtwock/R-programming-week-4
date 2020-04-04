
rankall <- function( outcome = 'heart attack', num = 20 ) {
    library(data.table)
    library(dplyr)
    
    
    care_measures_data <- data.table(read.csv2('H:\\LITERATURA_i_POBRANE\\R_kurs\\Cursera R\\R-programming-week-4\\outcome-of-care-measures.csv', sep = ',', dec ='.', colClasses = "character" ))
    
 
    
    ## Check that state and outcome are valid
    
    # New names
    
    old <- care_measures_data %>% 
                            select( starts_with("Hospital.30.Day.Death.")) %>% 
                            names()
 
    # old <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure' ,
    #          
    #          'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
    
    new <- c('heart attack' , 'heart failure' , 'pneumonia')
    
    setnames(care_measures_data , old , new)
    
    
    
    care_measures_data_2 <- care_measures_data %>%
        
        select( Hospital.Name, State, outcome ) 
    
    

  
    
    
    # min if else 
    if (outcome == 'heart attack'){
      care_measures_data_2[`heart attack` == "Not Available", `heart attack`:= "NA"]
      care_measures_data_2$`heart attack` <- as.numeric(care_measures_data_2$`heart attack`)
      care_measures_data_2 <- na.omit(care_measures_data_2)
      care_measures_data_2 <- data.table(care_measures_data_2)  
      
    care_measures_data_2 <- care_measures_data_2[order(State , `heart attack`)]
    care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`heart attack`) , 0), by = .(State)]
    care_measures_data_2[order( State,Rank)]
  
    if( num == 'best'){
      outcome_final <-  care_measures_data_2[Rank == 1,] 
    }else if(num == "worst"){
      outcome_final <-   care_measures_data_2[rank_worst == Rank,] 
      
    }else{
      
      outcome_final <-  care_measures_data_2[Rank == num,] 
      outcome_final[, unique_no := frank(Hospital.Name), by = State]
      outcome_final <- outcome_final[unique_no == 1,][order(State)]
      
    }
   
    
    } else if (outcome == 'heart failure'){
       
      care_measures_data_2[`heart failure` == "Not Available", `heart failure`:= "NA"]
      care_measures_data_2$`heart failure` <- as.numeric(care_measures_data_2$`heart failure`)
      care_measures_data_2 <- na.omit(care_measures_data_2)
      care_measures_data_2 <- data.table(care_measures_data_2)  
      
      care_measures_data_2 <- care_measures_data_2[order(State , `heart failure`)]
      care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`heart failure`) , 0), by = .(State)]
      care_measures_data_2[order(Rank, State)]

      
      if( num == 'best'){
        outcome_final <-  care_measures_data_2[Rank == 1,] 
      }else if(num == "worst"){
        outcome_final <-   care_measures_data_2[rank_worst == Rank,] 
        
      }else{
        outcome_final <-  care_measures_data_2[Rank == num,] 
        outcome_final[, unique_no := frank(Hospital.Name), by = State]
        outcome_final <- outcome_final[unique_no == 1,][order(State)]
        
      }
        
    } else{
        
        
      care_measures_data_2[`pneumonia` == "Not Available", `pneumonia`:= "NA"]
      care_measures_data_2$`pneumonia` <- as.numeric(care_measures_data_2$`pneumonia`)
      care_measures_data_2 <- na.omit(care_measures_data_2)
      care_measures_data_2 <- data.table(care_measures_data_2)  
      
      care_measures_data_2 <- care_measures_data_2[order(State , `pneumonia`)]
      care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`pneumonia`) , 0), by = .(State)]
      care_measures_data_2[, rank_worst := max(Rank), by = .(State)]
      care_measures_data_2[order(Rank, State)]
      
      if( num == 'best'){
        outcome_final <-  care_measures_data_2[Rank == 1,] 
      }else if(num == "worst"){
        outcome_final <-   care_measures_data_2[rank_worst == Rank,] 
        
      }else{
        outcome_final <-  care_measures_data_2[Rank == num,] 
        outcome_final[, unique_no := frank(Hospital.Name), by = State]
        outcome_final <- outcome_final[unique_no == 1,][order(State)] 
        
      }
        
    }
        
     # final    
    
    return(outcome_final)
   
  
    
    
# end function
}

