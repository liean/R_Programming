rankall <- function(outcome,num) {
        #read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        #get state list
        statelist <- sort(unique(data[,7]))
        outcomeList <- matrix(nrow=0,ncol=2)
        for (i in seq(11,23,by=6)) {
                #get colnames
                name <- colnames(data)[i]
                #get detail words in colnames
                nametrans <- strsplit(name,split="\\.")
                #find the postion of "from"
                index <- which(nametrans[[1]]=="from")
                #total number of words in name
                length <- length(nametrans[[1]])
                #number of words to extrac
                wordlen <- length - index
                #transfer words to a string
                temp <- paste(tail(nametrans[[1]],wordlen), sep="", collapse=" ")
                #get the outcome list with col number
                outcomeList <- rbind(outcomeList,c(temp,i))
        }
        
        #check state and outcome are valid
        if (!any(grepl(outcome,outcomeList[,1],ignore.case=TRUE))) {
                stop("invalid outcome")
        }
        else {
                hoscol <- as.numeric(outcomeList[grepl(outcome,outcomeList[,1],ignore.case=TRUE),2])
                hosrow <- which(data[,hoscol]!="Not Available")
                newdata <- data[hosrow,]
                newdata[,hoscol] <- as.numeric(newdata[,hoscol])
        }
        
        #initial data frame
        df <- data.frame("hospital"=character(),"state"=character(),stringsAsFactors=FALSE)
        
        #for each state, find the hospital of the given rank
        for (i in statelist) {
                statedata <- newdata[which(newdata$State == i),]
                #if no data available for that state, just append NA for that state
                if (!nrow(statedata)) {
                        df[nrow(df)+1,] <- c(NA,i)
                }
                #if data available
                else {
                        sortdata <- statedata[order(statedata[,hoscol],statedata[,2]),]
                        if (num == "best") {
                                df[nrow(df)+1,] <- c(sortdata[1,2],i)
                        }
                        else if (num == "worst") {
                                last <- nrow(sortdata)
                                df[nrow(df)+1,] <- c(sortdata[last,2],i)
                        }
                        else if (num > nrow(sortdata)) {
                                df[nrow(df)+1,] <- c(NA,i)
                        }
                        else {
                                df[nrow(df)+1,] <- c(sortdata[num,2],i)
                        }
                }
        }
        
        
        #return the data frame
        return(df)       
}

