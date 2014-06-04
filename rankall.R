rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome.df <- read.csv("./outcome-of-care-measures.csv", colClasses = "character",na.strings=c("Not Available","NA"))
        names(outcome.df) <- gsub("\\.\\.+", "\\.", names(outcome.df)) #removing multiple "." characters
        # creating numeric mortality rate columns
        outcome.df$Mortality.Rate.heart.attack.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Attack)
        outcome.df$Mortality.Rate.heart.failure.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Failure)
        outcome.df$Mortality.Rate.pneumonia.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Pneumonia)
        
       ## Check that outcome is valid
        if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) 
                stop("invalid outcome")
        
        ## For each state, find the hospital of the given rank
        splitList <- split( outcome.df , f = outcome.df$State )
        outcome.df$State <- as.factor(outcome.df$State)
        splitList <- split( outcome.df , f = outcome.df$State )
        rankall.df <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
        
        if(outcome=="heart attack")
        {
          for (i in 1:length(names(splitList)))
          {
            new.subset <- subset(splitList[[i]],!is.na(Mortality.Rate.heart.attack.Numeric), select=c(Hospital.Name,Mortality.Rate.heart.attack.Numeric))
            subset.ordered <- new.subset[with(new.subset, order(Mortality.Rate.heart.attack.Numeric, Hospital.Name)), ]
            if(class(num)=="numeric")
            {
              HospitalName <- subset.ordered$Hospital.Name[num]
            }
            if(num=="best")
            {
              HospitalName <- head(subset.ordered$Hospital.Name,1)
            }
            if(num=="worst")
            {
              HospitalName <- tail(subset.ordered$Hospital.Name,1)
            }
            
            new.df <- data.frame(hospital = HospitalName,state = names(splitList[i]))
            rankall.df <- rbind(rankall.df, new.df)
            
          }
        }
        if(outcome=="heart failure")
        {
          for (i in 1:length(names(splitList)))
          {
            new.subset <- subset(splitList[[i]],!is.na(Mortality.Rate.heart.failure.Numeric), select=c(Hospital.Name,Mortality.Rate.heart.failure.Numeric))
            subset.ordered <- new.subset[with(new.subset, order(Mortality.Rate.heart.failure.Numeric, Hospital.Name)), ]
            if(class(num)=="numeric")
            {
              HospitalName <- subset.ordered$Hospital.Name[num]
            }
            if(num=="best")
            {
              HospitalName <- head(subset.ordered$Hospital.Name,1)
            }
            if(num=="worst")
            {
              HospitalName <- tail(subset.ordered$Hospital.Name,1)
            }
            
            new.df <- data.frame(hospital = HospitalName,state = names(splitList[i]))
            rankall.df <- rbind(rankall.df, new.df)
            
          }
        }
        if(outcome=="pneumonia")
        {
          for (i in 1:length(names(splitList)))
          {
            new.subset <- subset(splitList[[i]],!is.na(Mortality.Rate.pneumonia.Numeric), select=c(Hospital.Name,Mortality.Rate.pneumonia.Numeric))
            subset.ordered <- new.subset[with(new.subset, order(Mortality.Rate.pneumonia.Numeric, Hospital.Name)), ]
            if(class(num)=="numeric")
            {
              HospitalName <- subset.ordered$Hospital.Name[num]
            }
            if(num=="best")
            {
              HospitalName <- head(subset.ordered$Hospital.Name,1)
            }
            if(num=="worst")
            {
              HospitalName <- tail(subset.ordered$Hospital.Name,1)
            }
            
            new.df <- data.frame(hospital = HospitalName,state = names(splitList[i]))
            rankall.df <- rbind(rankall.df, new.df)
            
          }
        }
                
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        rankall.df
}
