# finding the best hospital function
best <- function(state, outcome) {
        ## Read outcome data
        outcome.df <- read.csv("./outcome-of-care-measures.csv", colClasses = "character",na.strings=c("Not Available","NA"))
        names(outcome.df) <- gsub("\\.\\.+", "\\.", names(outcome.df)) #removing multiple "." characters
        # creating numeric mortality rate columns
        outcome.df$Mortality.Rate.heart.attack.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Attack)
        outcome.df$Mortality.Rate.heart.failure.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Failure)
        outcome.df$Mortality.Rate.pneumonia.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Pneumonia)
        
        ## Check that state is valid
        if(!(state %in% outcome.df$State)) 
                stop("invalid state")
        
        ## Check that outcome is valid
        if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) 
        stop("invalid outcome")

        ## Return hospital name in that state with lowest 30-day death
        if(outcome=="heart attack")
        {
                new.subset <- subset(outcome.df,State==state, select=c(Hospital.Name,Mortality.Rate.heart.attack.Numeric))
                minimum <- min(new.subset[,2], na.rm=T)
                subset2 <- subset(new.subset, Mortality.Rate.heart.attack.Numeric==minimum, select=c(Hospital.Name))
                subset2.ordered <- order(subset2)
                order1 <- subset2.ordered[1]
                
        }
        if(outcome=="heart failure")
        {
                new.subset <- subset(outcome.df,State==state, select=c(Hospital.Name,Mortality.Rate.heart.failure.Numeric))
                minimum <- min(new.subset[,2], na.rm=T)
                subset2 <- subset(new.subset, Mortality.Rate.heart.failure.Numeric==minimum, select=c(Hospital.Name))
                subset2.ordered <- order(subset2)
                order1 <- subset2.ordered[1]
                
        }
        if(outcome=="pneumonia")
        {
                new.subset <- subset(outcome.df,State==state, select=c(Hospital.Name,Mortality.Rate.pneumonia.Numeric))
                minimum <- min(new.subset[,2], na.rm=T)
                subset2 <- subset(new.subset, Mortality.Rate.pneumonia.Numeric==minimum, select=c(Hospital.Name))
                subset2.ordered <- order(subset2)
                order1 <- subset2.ordered[1]
                
        }
        
        ## rate
        subset2[order1,]
}

