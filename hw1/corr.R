corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        ###get file list in directory
        files <- list.files(directory, pattern="*.csv", full.names=TRUE)
        ###initial result as empty numeric vector
        result <- numeric()
        ###check every file for required condition
        for (i in files) {
                temp <- read.csv(i)
                if (sum(complete.cases(temp)) > threshold) {
                    tempcor <- cor(temp$nitrate,temp$sulfate,use="complete.obs",method="pearson")
                    result <- c(result,tempcor)
                }        
        } 
        result
}