
rankhospital <- function(state, outcome, num="best") {
  
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check that state and outcome are valid
  
  w1<- unique(outcomedata[,7])
  
  if (!(state %in% w1)) stop("invalid state") 
  if (!(outcome %in% c('heart attack','heart failure','pneumonia'))) stop("invalid outcome") 
  if (outcome=='heart attack') newoutcome="Heart.Attack"
  if (outcome=='heart failure') newoutcome="Heart.Failure"
  if (outcome=='pneumonia') newoutcome="Pneumonia"
  #x1<- word(outcome,1)
 # x2<- word(outcome,2)
 # newoutcome<- paste(x1,x2,sep=".")
  
  z<- paste("Hospital.30.Day.Death..Mortality..Rates.from",newoutcome,sep=".")
  ## newoutcomedatatest <- outcomedata[, c("State","Hospital.Name",z)]
  x.sub1 <- subset(outcomedata,outcomedata$State==state,select=c(z,"State","Hospital.Name"))
  x.sub2 <- subset(x.sub1,x.sub1[,z]!="Not Available") ## count how many rows have value for the outcome
  nohho <- nrow(x.sub2)
  w2 <- c("best","worst",1:nohho)
  ## if (!(num %in% w2)) stop("invalid number for rank") 
  sort.x.sub2 <- x.sub2[order(as.numeric(x.sub2[,z]), x.sub2$Hospital.Name), ]
## Return hospital name in that state with the given rank
  if (num=="best") h<-1
  if (num=="worst") {h<- nohho
  num=nohho}
  if (num%in% c(1:nohho)) h<- num
  hosp.name<- sort.x.sub2[h,3]
  if (num>nohho) hosp.name="NA"
  
  
  
## 30-day death rate
  d.rate<- sort.x.sub2[h,1]
  return(hosp.name)
}




