best <- function(state, outcome) {
        #read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
        if (!any(data$State==state)) {
                stop("invalid state")
        }
        else if (!any(grepl(outcome,outcomeList[,1],ignore.case=TRUE))) {
                stop("invalid outcome")
        }
        else {
                hosrow <- which(data$State==state)
                hoscol <- as.numeric(outcomeList[grepl(outcome,outcomeList[,1],ignore.case=TRUE),2])
                newdata <- data[hosrow,]
        }
              
        #Return hospital name in that state with lowest 30-day death rate
        newdata[,hoscol] <- as.numeric(newdata[,hoscol])
        return(newdata[which.min(newdata[,hoscol]),2])
}