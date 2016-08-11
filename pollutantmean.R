


pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
 ## directory1<- paste("/Users/xiuhongdu/Desktop/",directory)
  
  directory1<- "/Users/xiuhongdu/Desktop/specdata"
  file.names <- list.files(directory1)
  file.numbers <- as.numeric(sub('\\.csv$','', file.names))
  selected.files <- na.omit(file.names[match(id, file.numbers)])
  selected.dfs <- lapply(file.path(directory1,selected.files), read.csv)
  y<- unlist(c(sapply(selected.dfs, function(x) x[ ,pollutant])))
  mean(y, na.rm=TRUE)
}



source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "nitrate", 23)



