corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        correlation <- c()
	  for (i in 1:length(dir(directory))) {
		tab <- read.csv(file.path(directory,dir(directory)[i]), header = T, sep = ",")
            good <- complete.cases(tab)
            tab = tab[good,]
            if (length(tab[["sulfate"]]) > threshold) {
               #tem = data.frame(aa = tab[["sulfate"]], bb = tab[["sulfate"]])
               correlation <- c(correlation, cor(tab[["sulfate"]],tab[["nitrate"]] ))
            }           
	  }
        correlation
}