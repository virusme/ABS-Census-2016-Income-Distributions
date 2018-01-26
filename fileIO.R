
#
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
library(reshape)
library(leaflet)
library(shiny)




#
# read 2-headers-columns x 1-header-rows data into table
#
# data format: 
#   Age      x1 x2
#   Income   y1 y2
#   POA
#    z1     a1, a2
#    z3     a22, a24
#
readTwoHeaderRows2table <- function(file, referenceDimension = 2) {
    #
    ## first and second dimension
    # read row headers and convert them
    headers <- read.csv(file = file, nrows = 2, header = FALSE)
    # remove last column
    headers <- headers[, -ncol(headers)]
    # replace '' by NA
    headers[headers == ''] <- NA
    # fill NA with first available value and grab dim1 and dim2
    dim1 <- unlist(fillNAgaps(as.list(t(headers[1,])), firstBack = TRUE))
    dim1Name <- dim1[1]
    dim1List <- dim1[-1]
  
    # usually dim2 is the one that is supposed to used for row-building  (for example Weekly income)
    dim2 <- unlist(fillNAgaps(as.list(t(headers[2,])), firstBack = TRUE))
    dim2Name <- dim2[1]
    dim2List <- dim2[-1]
    #

    ## for 3rd dimension read the data and grab the name of the first column
    # however for sake of simplicity, I am using "POA" as dim3Name
    ## read the data
    #data <- read.csv(file = file, header = FALSE)
    ### third dimension (skip the first two rows and grab the first column)
    #dim3 <- as.character(data[-(1:2), 1])
    #dim3Name <- dim3[1]
    dim3Name <- 'POA'

    # read the data  (skip first rows and skip the last column after reading the data)
    data <- data.table(read.csv(file = file, skip = 3, header = FALSE))
    data <- data[, -tail(names(data),1), with=FALSE]
   
    # reference and value Columns
    if (referenceDimension == 2) {
        refColList <- dim2List
        refColName <- dim2Name
        valColList <- dim1List
        valColName <- dim1Name

    }
    else {
        if (referenceDimension == 1) {
            refColList <- dim1List
            refColName <- dim1Name
            valColList <- dim2List
            valColName <- dim2Name
        }
    }
        

    # refColNames
    refColNames <- unique(refColList)
    
    # perform a hacky-melting. Grab each section (section=1) of dimension1, melt it, with variable = dimension2 and value = dimension1[section] 
    colNames <- c(dim3Name, valColList)
    names(data) <- colNames
    #
    # 
    outputDT <- NULL
    for (dim1 in unique(valColList)) {
        print(dim1)
        # grab each section. get the first column as it is postalcode . cols = vector of true/false
        cols <- c(TRUE, valColList == dim1)
        tmpData <- data[, cols, with = FALSE]
        # rename columns to incomeColumns
        if (dim1 == 'Not applicable') {
            if (dim(tmpData)[2] < (length(refColNames) + 1)) {
                refColNames <- refColNames[-length(refColNames)]
            }
        }
        names(tmpData) <- c(dim3Name, refColNames)
        # melt
        tmpData.melt <- melt(tmpData, id.vars = dim3Name, variable.name = refColName, value.name = dim1)
        #
        if (is.null(outputDT)) {
            outputDT <- tmpData.melt
        }
        else {
            outputDT <- merge(outputDT, tmpData.melt, by = c(dim3Name, refColName))
        }
    }
    # 
    return(outputDT)
}

#
# fill NA gaps with the first encountered non-NA value
#
fillNAgaps <- function(x, firstBack = FALSE) {
    ## NA's in a vector or factor are replaced with last non-NA values
    ## If firstBack is TRUE, it will fill in leading NA's with the first
    ## non-NA value. If FALSE, it will not change leading NA's.

    # If it's a factor, store the level labels and convert to integer
    lvls <- NULL
    if (is.factor(x)) {
        lvls <- levels(x)
        x <- as.integer(x)
    }

    goodIdx <- !is.na(x)

    # These are the non-NA values from x only
    # Add a leading NA or take the first good value, depending on firstBack   
    if (firstBack) goodVals <- c(x[goodIdx][1], x[goodIdx])
    else goodVals <- c(NA, x[goodIdx])

    # Fill the indices of the output vector with the indices pulled from
    # these offsets of goodVals. Add 1 to avoid indexing to zero.
    fillIdx <- cumsum(goodIdx) + 1

    x <- goodVals[fillIdx]

    # If it was originally a factor, convert it back
    if (!is.null(lvls)) {
        x <- factor(x, levels = seq_along(lvls), labels = lvls)
    }

    x
}

# unroll a list into a string with elements
unrollList2String <- function(lst) {
    #
   return(paste(lst, collapse = ','))
}