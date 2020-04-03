
rankhospital <- function(state = 'TX', outcome = 'heart failure', num = "worst" ) {
    library(data.table)
    library(dplyr)
    
    
    care_measures_data <- data.table(read.csv2('H:\\LITERATURA_i_POBRANE\\R_kurs\\Cursera R\\R-programming-week-4\\outcome-of-care-measures.csv', sep = ',', dec ='.', colClasses = "character" ))
    
    hospital_states <- unique(care_measures_data$State)
    if (!state %in% hospital_states){ 
       msg <- paste0("Error in best(",state , ",", outcome,") : invalid state")
       return (msg)
        # Error in best("BB", "heart attack") : invalid state
    }
    
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
        
        select( Hospital.Name, State, outcome ) %>%
        
        filter(State == state )
    
    
    # cleaning 
    care_measures_data_2[,3] <- ifelse(care_measures_data_2[,3] == "Not Available", "NA", care_measures_data_2[,3] )
    care_measures_data_2[,3] <- as.numeric(care_measures_data_2[,3])
    care_measures_data_2 <- na.omit(care_measures_data_2)
    care_measures_data_2 <- data.table(care_measures_data_2)
    
    
    # min if else 
    if (outcome == 'heart attack'){
    care_measures_data_2 <- care_measures_data_2[order(State , `heart attack`)]
    if(num == "best"){
            care_measures_data_2 <- care_measures_data_2[order(State , `heart attack`)]
            care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`heart attack`) , 0), by = .(State)]
    }else{
        care_measures_data_2 <- care_measures_data_2[order(State , - `heart attack`)]
        care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`heart attack`) , 0), by = .(State)]
        
    }
    
    outcome_final <-  head(care_measures_data_2 , 4)
    } else if (outcome == 'heart failure'){
        care_measures_data_2 <- care_measures_data_2[order(State , `heart failure`)]
        if(num == "best"){
            care_measures_data_2 <- care_measures_data_2[order(State , `heart failure`)]
            care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`heart failure`) , 0), by = .(State)]
        }else{
            care_measures_data_2 <- care_measures_data_2[order(State , - `heart failure`)]
            care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`heart failure`) , 0), by = .(State)]
            
        }
        
        outcome_final <-  head(care_measures_data_2 , 4) 
    } else{
        care_measures_data_2 <- care_measures_data_2[order(State , `pneumonia`)]
        if(num == "best"){
            care_measures_data_2 <- care_measures_data_2[order(State , `pneumonia`)]
            care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`pneumonia`) , 0), by = .(State)]
        }else{
            care_measures_data_2 <- care_measures_data_2[order(State , - `pneumonia`)]
            care_measures_data_2 <- care_measures_data_2[,Rank := round(frank(`pneumonia`) , 0), by = .(State)]
            
        }
        
    }
        
     # final    
    
    return(outcome_final)
   
    
}

