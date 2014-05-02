complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        ###get file names by id
        files <- paste(formatC(id,width=3,flag="0"),".csv",sep="")
        ###get full file name by adding path
        filePath <- file.path(getwd(),directory,files)
        ###get the observation number of each file
        for (i in filePath) {
                temp <- read.csv(i)
                tempnobs <- sum(complete.cases(temp))
                tempid <- temp[1,"ID"]
                if (!exists("result")) result <- data.frame(id=tempid, nobs=tempnobs)
                else result <- rbind(result,data.frame(id=tempid,nobs=tempnobs))            
        }      
        result
}