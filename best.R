best <- function(state, outcome) {
 
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check that state and outcome are valid
  w1<- unique(outcomedata[,7])
  
  if (!(state %in% w1)) stop("invalid state") 
  if (!(outcome %in% c('heart attack','heart failure','pneumonia'))) stop("invalid outcome") 
  
  if (outcome=='heart attack') newoutcome="Heart.Attack"
  if (outcome=='heart failure') newoutcome="Heart.Failure"
  if (outcome=='pneumonia') newoutcome="Pneumonia"

  z<- paste("Hospital.30.Day.Death..Mortality..Rates.from",newoutcome,sep=".")
  ## newoutcomedatatest <- outcomedata[, c("State","Hospital.Name",z)]
  x.sub1 <- subset(outcomedata,outcomedata$State==state)
  x.sub1[,z] = suppressWarnings(as.numeric(x.sub1[,z]))
  y1<-min(x.sub1[,z],na.rm=TRUE)
## Return hospital name in that state with lowest 30-day death
## rate

  y2<-x.sub1$Hospital.Name[subset(x.sub1, select=z) == y1] 
  ## sort the list of the hospital alphabetically
  y3<- sort(y2) 
  #form the vector containging the smallest rate and the hospital name
  ## y5<- c(y1, y3[1])
  return(y3[1])
}




