# Programming assignments as part of a Coursera module titled
# "R Programming" by Roger D. Peng


#==============================================================================
# Assignment 1
#==============================================================================
# monthMean("/home/arvindpdmn/Downloads/q1.csv", c("Ozone", "Solar.R", "Wind"))
monthMean <- function(datafile, fields) {
    data <- read.csv(datafile)
    sapply(split(data, data$Month), function(x) colMeans(x[, fields], na.rm = TRUE))
}


#==============================================================================
# Assignment 2
#==============================================================================
# pollutantmean("/home/arvindpdmn/Downloads/q2", "sulfate", c(1,2,20))
pollutantmean <- function(directory, pollutant, id = 1:332) {
    csvfiles <- dir(directory, "*\\.csv$", full.names = TRUE)
    data <- lapply(csvfiles[id], read.csv)
    
    # implementation 1
    numDataPoints <- 0L
    total <- 0L
    for (filedata in data) {
        d <- filedata[[pollutant]] # relevant column data
        d <- d[complete.cases(d)] # remove NA values
        numDataPoints <- numDataPoints + length(d)
        total <- total + sum(d)
    }
    total / numDataPoints
    
    # implementation 2
    data = do.call(rbind.data.frame, data)
    mean(data[,pollutant], na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
    csvfiles <- dir(directory, "*\\.csv$", full.names = TRUE)
    csvfiles <- csvfiles[id]
    data <- lapply(csvfiles, read.csv)
    histogram <- matrix(nrow=length(data), ncol=2)
    for (i in seq_along(data)) {
        d <- data[[i]]
        d <- d[complete.cases(d),] # remove NA values
        histogram[i,] <- c(as.integer(gsub("[^0-9]", "", csvfiles[i])), length(d[[1]]))
    }
    colnames(histogram) <- c("id", "nobs")
    data.frame(histogram)
}

corr <- function(directory, threshold = 0) {
    csvfiles <- dir(directory, "*\\.csv$", full.names = TRUE)
    data <- lapply(csvfiles, read.csv)
    corrData <- numeric(length(data))
    for (i in seq_along(data)) {
        d <- data[[i]]
        d <- d[complete.cases(d),] # remove NA values
        if (length(d[[1]]) >= threshold || threshold == 0) {
            corrData[i] <- cor(d["sulfate"], d["nitrate"])
        }
        else {
            corrData[i] <- NA
        }
    }
    corrData[complete.cases(corrData)]
}


#==============================================================================
# Assignment 3
#==============================================================================
processNum <- function(num) {
    if (num == "best") {
        num = 1
        func <- head
    }
    else if (num == "worst") {
        num = 1
        func <- tail
    }
    else if ((is.numeric(num) || is.integer(num)) && num > 0) {
        func <- head
    }
    else {
        stop("invalid num")
    }
    list(num = num, func = func)
}

mortalityRates <- function() {
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    hist(as.numeric(d[,11]))
}

bestHospitalForMortality <- function(state, outcome) {
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    stateFilter <- d$State == state
    if (!any(stateFilter)) {
        stop("invalid state")
    }

    patt <- paste("^Hospital.*30\\.Day.*Mortality\\.\\.Rates.*", gsub(" ", ".", outcome), sep="")
    colFilter <- grep(patt, colnames(d), ignore.case = TRUE)
    if (length(colFilter) == 0) {
        stop("invalid outcome")
    }

    d <- d[stateFilter,]
    nameFilter <- grep("^Hospital\\.Name$", colnames(d))
    minVal <- min(as.numeric(d[,colFilter]), na.rm=T)
    minVal <- sprintf("%.1f", minVal) # in original format for comparison
    sort(d[d[colFilter] == minVal, nameFilter])
}

rankHospitalsForMortality <- function(state, outcome, num = "best") {
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    stateFilter <- d$State == state
    if (!any(stateFilter)) {
        stop("invalid state")
    }
    
    patt <- paste("^Hospital.*30\\.Day.*Mortality\\.\\.Rates.*", gsub(" ", ".", outcome), sep="")
    colFilter <- grep(patt, colnames(d), ignore.case = TRUE)
    if (length(colFilter) == 0) {
        stop("invalid outcome")
    }

    p <- processNum(num)
    
    # Retain only relevant columns and remove NA
    nameFilter <- grep("^Hospital\\.Name$", colnames(d))
    d <- d[stateFilter,c(nameFilter,colFilter)]
    colnames(d) <- c("Hospital.Name", "Rate")
    d["Rate"] <- as.numeric(d$Rate)
    d <- d[order(d$Rate, d$Hospital.Name), ]
    d <- d[complete.cases(d),]

    # Return as specified by num
    if (nrow(d) >= p$num) {
        top <- p$func(d, n = p$num)

        # Return top num records
        top["Rank"] <- 1:p$num # add a new column
        #return(top)
        
        # For this exercise, return the nth hospital name
        return(top[p$num,]$Hospital.Name)
    }
    else {
        return(NA)
    }
}

rankSummaryForMortality <- function(outcome, num = "best") {
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    patt <- paste("^Hospital.*30\\.Day.*Mortality\\.\\.Rates.*", gsub(" ", ".", outcome), sep="")
    colFilter <- grep(patt, colnames(d), ignore.case = TRUE)
    if (length(colFilter) == 0) {
        stop("invalid outcome")
    }
    
    p <- processNum(num)
    
    # Retain only relevant columns and remove NA
    nameFilter <- grep("^Hospital\\.Name$", colnames(d))
    stateFilter <- grep("^State$", colnames(d))
    d <- d[,c(nameFilter,colFilter,stateFilter)]
    colnames(d) <- c("Hospital", "Rate", "State")
    d["Rate"] <- as.numeric(d$Rate)
    d <- d[order(d$State, d$Rate, d$Hospital), ]
    d <- d[complete.cases(d),]

    # Return as specified by num
    d <- sapply(split(d, d$State), function(x, num = p$num, func = p$func) {
        if (nrow(x) >= num) {
            top <- func(x, n = num)
            curr <- top[num,]
            return(c(curr$Hospital,curr$State))
        }
        else {
            return(c(NA, x[1,"State"]))
        }
    })
    d <- t(data.frame(d))
    colnames(d) <- c("Hospital", "State")
    return(d)
}

