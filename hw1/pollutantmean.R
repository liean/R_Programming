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
        
        ###get file names by id
        files <- paste(formatC(id,width=3,flag="0"),".csv",sep="")
        ###get full file name by adding path
        filePath <- file.path(getwd(),directory,files)
        ###merge those files into a new dataframe
        for (i in filePath) {
                if (!exists("dataset")) dataset <- read.csv(i)
                else dataset <- rbind(dataset, read.csv(i))
        }
        
        ###get the mean of assigned pollutant and remove NA elements
        result <- mean(dataset[,pollutant], na.rm=TRUE)
        result
}