# setwd("C:/Users/misha/Documents/coursera/intro-to-R/ProgrammingAssignment3")
setwd("C:/Users/Flann/Documents/GitHub/ProgrammingAssignment3")

outcome.df <- read.csv("./outcome-of-care-measures.csv", colClasses = "character",na.strings=c("Not Available","NA"))
names(outcome.df) <- gsub("\\.\\.+", "\\.", names(outcome.df)) #removing multiple "." characters
# creating numeric mortality rate columns
outcome.df$Mortality.Rate.heart.attack.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Attack)
outcome.df$Mortality.Rate.heart.failure.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Failure)
outcome.df$Mortality.Rate.pneumonia.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Pneumonia)


# head(outcome.df)
# ncol(outcome.df)
# outcome.df[, 11] <- as.numeric(outcome.df[, 11])
# ## You may get a warning about NAs being introduced; that is okay
# hist(outcome.df[, 11])
# 
names(outcome.df)
# head(outcome.df$Hospital.Name)


# hospitalheartfailure <- outcome.df[outcome.df$State=="AL", "outcome.df$Mortality.Rate.heart.failure.Numeric",]
# class(hospitalheartfailure)
# outcome <- gsub(" ", "\\.", outcome) # fixing the "outcome input to match the column name format
# outcome <- tolower(outcome)


state <- "TX"
outcome <- "pneumonia"

# checking state is valid
if(!(state %in% outcome.df$State)) stop("invalid state")

# checking outcome is valid
## valid outcome: "heart attack","heart failure","pneumonia"

if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) 
        stop("invalid outcome")

# subsetting data based on state and outcome
if(outcome == "heart attack") relevantColumn <- "Mortality.Rate.heart.attack.Numeric"
if(outcome == "heart failure") relevantColumn <- "Mortality.Rate.heart.failure.Numeric"
if(outcome == "pneumonia") relevantColumn <- "Mortality.Rate.pneumonia.Numeric"
relevantData <- subset(outcome.df, State==state)
# head(relevantData)

if(outcome == "heart attack") relevantData <- subset(outcome.df, State==state, select = c(Mortality.Rate.heart.attack.Numeric,Hospital.Name))
if(outcome == "heart failure") relevantData <- subset(outcome.df, State==state, select = c(Mortality.Rate.heart.failure.Numeric,Hospital.Name))
if(outcome == "pneumonia") relevantData <- subset(outcome.df, State==state, select = c(Mortality.Rate.pneumonia.Numeric,Hospital.Name))
# head(relevantData)

bestHospital <- min(relevantData, na.rm=T)

# finding the best hospital function
best <- function(state, outcome) {
        ## Read outcome data
        outcome.df <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
        names(outcome.df) <- gsub("\\.\\.+", "\\.", names(outcome.df)) #removing multiple "." characters
        
        ## Check that state is valid
        if(!(state %in% outcome.df$State)) 
                stop("invalid state")
        
        ## Check that outcome is valid
        if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) 
        stop("invalid outcome")

        ## Return hospital name in that state with lowest 30-day death
        mortality.column <- paste0("outcome.df$Mortality.Rate.", outcome, ".Numeric")
        relevant.data <- outcome.df[outcome.df$State == state & !is.na(mortality.column),]
        mortality.column <- paste0("Mortality.Rate.", outcome, ".Numeric")
        class(relevant.data$mortality.column)
        ## rate
}

source("best.R")
best("BB", "heart attack")

source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}
###########################################################################
###########################################################################
setwd("C:/Users/misha/Documents/coursera/intro-to-R/ProgrammingAssignment3")

outcome.df <- read.csv("./outcome-of-care-measures.csv", colClasses = "character",na.strings=c("Not Available","NA"))
names(outcome.df) <- gsub("\\.\\.+", "\\.", names(outcome.df)) #removing multiple "." characters
# creating numeric mortality rate columns
outcome.df$Mortality.Rate.heart.attack.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Attack)
outcome.df$Mortality.Rate.heart.failure.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Failure)
outcome.df$Mortality.Rate.pneumonia.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Pneumonia)

#################### INPUT
state <- "TX"
outcome <- "heart failure"
num <- "worst"
##########################

# checking state is valid
if(!(state %in% outcome.df$State)) stop("invalid state")

# checking outcome is valid
# valid outcome: "heart attack","heart failure","pneumonia"

if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) 
        stop("invalid outcome")

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
        subset.ordered$Hospital.Name[num]
}
if(num=="best")
{
        head(subset.ordered$Hospital.Name,1)
}
if(num=="worst")
{
        tail(subset.ordered$Hospital.Name,1)
}     

source("rankhospital.R")
rankhospital("TX", "heart attack", 1)

source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()

###########################################################################
###########################################################################
setwd("C:/Users/Flann/Documents/GitHub/ProgrammingAssignment3")

outcome.df <- read.csv("./outcome-of-care-measures.csv", colClasses = "character",na.strings=c("Not Available","NA"))
names(outcome.df) <- gsub("\\.\\.+", "\\.", names(outcome.df)) #removing multiple "." characters
# creating numeric mortality rate columns
outcome.df$Mortality.Rate.heart.attack.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Attack)
outcome.df$Mortality.Rate.heart.failure.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Heart.Failure)
outcome.df$Mortality.Rate.pneumonia.Numeric <- as.numeric(outcome.df$Hospital.30.Day.Death.Mortality.Rates.from.Pneumonia)

splitList <- split( outcome.df , f = outcome.df$State )
outcome.df$State <- as.factor(outcome.df$State)
splitList <- split( outcome.df , f = outcome.df$State )
rankall.df <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
#################### INPUT
outcome <- "heart attack"
num <- 20
##########################
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
source("rankall.R")
tail(rankall("heart failure"), 10)

source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()

