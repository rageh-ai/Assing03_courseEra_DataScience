rankall <- function(outcome, num = "best")
{
        outcomeData <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
        possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
        outcomeData <- outcomeData[order(outcomeData$State),]
        chosenCols <- c(2,7)
        
        
        if (outcome %in% possibleOutcomes)
        {
                if (outcome == "heart attack")
                {
                        chosenCols <- append(chosenCols, 11)
                }
                
                if (outcome == "heart failure")
                {
                        chosenCols <- append(chosenCols, 17)
                }
                
                if (outcome == "pneumonia")
                {
                        chosenCols <- append(chosenCols, 23)
                }
                
        }
        outcomeData <- outcomeData[][chosenCols]
        outcomeData <- outcomeData[outcomeData[,3] != "Not Available",]
        cols <- colnames(outcomeData)
        outcomeData <- split(outcomeData, outcomeData$State)
        numberOfStates <- length(outcomeData)
        hospitalNames <- c()
        hospitalState <- c()
        
        if (num == "best")
        {
                num <- 1
        }
        

        
        for (i in 1:numberOfStates)
        {
                relevantData <- data.frame(outcomeData[i])
                relevantData[,3] <- as.numeric(relevantData[,3])
                relevantData <- relevantData[order(relevantData[,3],relevantData[,1]),]
                relevantLength <- nrow(relevantData)
                hospitalState <- append(hospitalState, names(outcomeData)[i])
                
                if (!is.numeric(num))
                {
                        if (num == "worst")
                        {
                                hospitalNames <- append(hospitalNames, relevantData[relevantLength,1])

                        }
                }
                else if (is.numeric(num))
                {
                        if (num > relevantLength)
                        {
                                hospitalNames <- append(hospitalNames,NA)
                        }
                        else if (num <= relevantLength)
                        {
                                hospitalNames <- append(hospitalNames, relevantData[num,1])
                                
                        }
                }
        }
        
        finalData <- data.frame("hospital" = hospitalNames, "state" = hospitalState)
        finalData
        
       
}