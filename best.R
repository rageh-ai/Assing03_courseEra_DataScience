best <- function(state, outcome)
{
      
        outcomeData <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
        possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
        chosenCols <- c(2,7)
        
        
        if (!(state %in% outcomeData$State))
        {
                stop("invalid state")
                
        }
        

        
        if (!(outcome %in% possibleOutcomes))
        {
                stop("invalid outcome")
        }
        
        if (outcome == "heart attack")
        {
                chosenCols <- append(chosenCols, 11)
                
        }
        
        else if (outcome == "heart failure")
        {
                chosenCols <- append(chosenCols, 17)
                
        }
        
        else if (outcome == "pneumonia")
        {
                chosenCols <- append(chosenCols, 23)
        }
        
        
        outcomeData <- outcomeData[][chosenCols]
        outcomeData <- outcomeData[outcomeData[,2] == state,]
        outcomeData <- outcomeData[outcomeData[,3] != "Not Available",]
        minCol <- as.numeric(outcomeData[,3])
        minInd <- which.min(minCol)
        minVal <- as.character(outcomeData[minInd,3])
        
        relevantIndexes <- which(outcomeData[,3] == minVal)
        
        if (length(relevantIndexes) > 1)
        {
                outcomeData <- outcomeData[relevantIndexes,]
                outcomeData[order(outcomeData$Hospital.Name),]
        }
        
        else{
                outcomeData <- outcomeData[minInd,1]
        }
        
        outcomeData
        
        
        

        
}