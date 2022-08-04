rankhospital <- function(state, outcome, num)
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
        outcomeData <- outcomeData[outcomeData[,2] == state,]
        outcomeData <- outcomeData[outcomeData[,3] != "Not Available",]


        
        
        
        if (!is.numeric(num))
        {
                if (num == "best")
                {
                        source("Assign03/best.R")
                        return(best(state, outcome))
                }
                else if (num == "worst")
                {
                        maxCol <- as.numeric(outcomeData[,3])
                        maxInd <- which.max(maxCol)
                        maxVal <- outcomeData[maxInd,3]
                        
                        return(outcomeData[outcomeData[,3] == maxVal,"Hospital.Name"])
                }
        }
        else if ((num  > nrow(outcomeData)) & (is.numeric(num)))
        {
                         return(NA)
                         
        }
        
        
        
        
        
        
        
        
        
        i <- 1
        
        while (i <= num)
        {
                minCol <- as.numeric(outcomeData[,3])
                minRow <- which.min(minCol)
                mask <- outcomeData[,3] == outcomeData[minRow,3]
                firstVal <- which(mask == TRUE)
                trueindexes <- c()
                
                while(TRUE %in% mask)
                {
        
                        trueindexes <- append(trueindexes,firstVal)
                        mask <- mask[-firstVal]
                        firstVal <- which(mask == TRUE)
                        
                }
                
                savedData <- outcomeData[trueindexes,]
                outcomeData <- outcomeData[-trueindexes,]
                
                i <- i + length(trueindexes)
                

        }
        
        savedData <- savedData[order(savedData$Hospital.Name),]
        savedData[num - i,"Hospital.Name"]



}