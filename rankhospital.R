rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        if(outcome=="heart attack")
        {
                new.subset <- subset(outcome.df,State==state & !is.na(Mortality.Rate.heart.attack.Numeric), select=c(Hospital.Name,Mortality.Rate.heart.attack.Numeric))
                subset.ordered <- new.subset[with(new.subset, order(Mortality.Rate.heart.attack.Numeric, Hospital.Name)), ]
        }
        if(outcome=="heart failure")
        {
                new.subset <- subset(outcome.df,State==state & !is.na(Mortality.Rate.heart.failure.Numeric), select=c(Hospital.Name,Mortality.Rate.heart.failure.Numeric))
                subset.ordered <- new.subset[with(new.subset, order(Mortality.Rate.heart.failure.Numeric, Hospital.Name)), ]
        }
        if(outcome=="pneumonia")
        {
                new.subset <- subset(outcome.df,State==state & !is.na(Mortality.Rate.pneumonia.Numeric), select=c(Hospital.Name,Mortality.Rate.pneumonia.Numeric))
                subset.ordered <- new.subset[with(new.subset, order(Mortality.Rate.pneumonia.Numeric, Hospital.Name)), ]
        }
        if(class(num)=="numeric")
        {
                rate <- subset.ordered$Hospital.Name[num]
        }
        if(num=="best")
        {
                rate <- head(subset.ordered$Hospital.Name,1)
        }
        if(num=="worst")
        {
                rate <- tail(subset.ordered$Hospital.Name,1)
        }     
        ## 30-day death rate
        print(rate)
}
