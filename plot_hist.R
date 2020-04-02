outcome <- read.csv("H:\\LITERATURA_i_POBRANE\\R_kurs\\Cursera R\\R-programming-week-4\\outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
 outcome[, 11] <- as.numeric(outcome[, 11])
 ## You may get a warning about NAs being introduced; that is okay
     hist(outcome[, 11])
     