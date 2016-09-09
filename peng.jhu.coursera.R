# Programming assignments as part of a Coursera series of modules titled
# Data Science Specialization by Roger D. Peng, Brian Caffo, Jeff Leek
# 2. R Programming
# 3. Getting and Cleaning Data
# 4. Exploratory Data Analysis
# 5. Reproducible Research
# 6. Statistical Inference
# 7. Regression Models
# 8. Practical Machine Learning
# 9. Developing Data Products


#==============================================================================
# Assignment 2.1
#==============================================================================
# monthMean("q2.1/data.csv", c("Ozone", "Solar.R", "Wind"))
monthMean <- function(datafile, fields) {
    data <- read.csv(datafile)
    sapply(split(data, data$Month), function(x) colMeans(x[, fields], na.rm = TRUE))
}


#==============================================================================
# Assignment 2.2
#==============================================================================
# pollutantmean("q2.1", "sulfate", c(1,2,20))
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
# Assignment 2.3
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
    d <- read.csv("q2.3/outcome-of-care-measures.csv", colClasses = "character")
    hist(as.numeric(d[,11]))
}

bestHospitalForMortality <- function(state, outcome) {
    d <- read.csv("q2.3/outcome-of-care-measures.csv", colClasses = "character")
    
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
    d <- read.csv("q2.3/outcome-of-care-measures.csv", colClasses = "character")
    
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
    d <- read.csv("q2.3/outcome-of-care-measures.csv", colClasses = "character")
    
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


#==============================================================================
# Assignment 3.1
#==============================================================================
realEstateIdaho1Data <- function() {
    # Read directly if we don't want to save the file
    # d<- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")
    if (!file.exists("q3.1/data.csv"))
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "q3.1/data.csv", method="curl", quiet=T)
    d <- read.csv("q3.1/data.csv")
}

realEstateIdaho1 <- function() {
    d <- realEstateIdaho1Data()
    highValue <- d$VAL>=24 # from code book, 24 => property value >= $1,000,000
    cat(table(highValue)[2]) # take only the count for TRUE
}

naturalGas <- function() {
    library(xlsx)
    if (!file.exists("q3.1/data.xlsx"))
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx", "q3.1/data.xlsx", method="curl", quiet=T)
    dat <- read.xlsx("q3.1/data.xlsx", 1, rowIndex = 18:23, colIndex = 7:15)
}

restaurants <- function() {
    library(XML)
    if (!file.exists("q3.1/data.xml"))
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml", "q3.1/data.xml", method="curl", quiet=T)
    doc <- xmlTreeParse("q3.1/data.xml", useInternalNodes=T)
    m <- xpathSApply(doc, "//zipcode[text()='21231']", xmlValue)
    length(m)
}

realEstateIdaho2 <- function() {
    library(data.table)
    if (!file.exists("q3.1/data2.csv"))
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "q3.1/data2.csv", method="curl", quiet=T)
    DT <- fread("q3.1/data2.csv")

    # Calculate the mean in different ways
    print(system.time(res <- tapply(DT$pwgtp15,DT$SEX,mean))) # about same elapsed time as sapply but bigger user time
    print(res)
    print(system.time(res <- DT[,mean(pwgtp15),by=SEX])) # fastest
    print(res)
    print(system.time({res1 <- mean(DT[DT$SEX==1,]$pwgtp15); res2 <- mean(DT[DT$SEX==2,]$pwgtp15)})) # slowest
    print(c(res1, res2))
    print(system.time(res <- sapply(split(DT$pwgtp15,DT$SEX),mean)))
    print(res)
}


#==============================================================================
# Assignment 3.2
#==============================================================================
charCount <- function() {
    con <- url("http://biostat.jhsph.edu/~jleek/contact.html") # won't work for HTTPS
    d <- readLines(con)
    close(con)
    cat(sapply(content[c(10,20,30,100)], nchar))
}

sumFourthRow <- function() {
    if (!file.exists("q3.2/data.for"))
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for", "q3.2/data.for", method="curl", quiet=T)
    d <- read.table("q3.2/data.for", skip = 3)

    # exploration
    apply(d[,2:ncol(d)], 1, sum, na.rm=T) # row-wise summation
    sum(d[4,2:ncol(d)], na.rm=T) # sum of only 4th row

    # answer to the question
    sum(d[,6]) # sum 4th of 9 columns, which is stored in 6th column of 13 columns
}


#==============================================================================
# Assignment 3.3
#==============================================================================
realEstateIdaho3 <- function() {
    d <- realEstateIdaho1Data()
    agricultureLogical <- d$ACR==3 & d$AGS==6
    which(agricultureLogical)
}

imageAnalysis <- function() {
    library(jpeg)
    if (!file.exists("q3.3/data.jpg"))
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", "q3.3/data.jpg", method="curl", quiet=T)
    d <- readJPEG("q3.3/data.jpg", native=T)
    quantile(d, prob = c(0.30,0.80))
}

gdpEduDS <- function() {
    if (!file.exists("q3.3/gdp.csv"))
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", "q3.3/gdp.csv", method="curl", quiet=T)
    gdp <- read.csv("q3.3/gdp.csv", header=F, colClasses = "character", skip = 5)
    gdp <- gdp[complete.cases(as.numeric(gdp$V2)),c("V1","V2","V4","V5")]
    
    if (!file.exists("q3.3/edu.csv"))
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", "q3.3/edu.csv", method="curl", quiet=T)
    edu <- read.csv("q3.3/edu.csv", colClasses = "character")
    
    a <- merge(gdp, edu, by.x="V1", by.y="CountryCode")
    a[,"V2"] <- as.numeric(a[,"V2"])
    a[,"V5"] <- as.numeric(gsub("[^0-9]", "", a[,"V5"]))

    return(a)
}

gdpAndEdu <- function() {
    a <- gdpEduDS()

    # Descending order of ranking and show 13th country
    s <- a[order(-a$V2),]
    s[13,"Short.Name"]

    # Mean of ranking by income group
    tapply(a$V2, a$Income.Group, mean)

    # Analysis of ranking vs income groups
    q  <- quantile(a$V2, probs = seq(0,1,0.2)) # four quantile points => five groups
    groups <- cut(a$V2, breaks=q)
    table(groups, a$Income.Group)
}


#==============================================================================
# Assignment 3.4
#==============================================================================
realEstateIdaho4 <- function() {
    d <- realEstateIdaho1Data()
    strsplit(names(d), "wgtp")
}

gdpAnalysis <- function() {
    a <- gdpEduDS()

    mean(a[,"V5"])

    grep("^United", a$V4, value = TRUE)

    length(grep("Fiscal +year +end.*June", a[,"Special.Notes"]))
}

stockPrices <- function() {
    library(quantmod)
    amzn = getSymbols("AMZN",auto.assign=FALSE)
    sampleTimes = index(amzn)

    length(sampleTimes[sapply(sampleTimes, format, "%Y")=="2012"])
    length(sampleTimes[sapply(sampleTimes, format, "%a%Y")=="Mon2012"])
}


#==============================================================================
# Assignment 3.5
#==============================================================================
readAccGyroDataSet <- function(type, labelnames, featurenames) {
    # Main table will be as follows:
    #   subjectid, {accx}, {accy}, {accz}, {gyrox}, {gyroy}, {gyroz}, {totalx}, {totaly}, {totalz}, label, {features}
    # {accx} component has 128 acceleration measurements; likewise, for y and z.
    # {gyrox} component has 128 gyroscope measurements; likewise, for y and z.
    # {totalx} component has 128 total measurements; likewise, for y and z.
    # {features} component will have 561 columns

    subjects <- fread(paste0("q3.4/", type, "/subject_", type, ".txt"))
    
    x <- fread(paste0("q3.4/", type, "/Inertial Signals/body_acc_x_", type, ".txt"))
    y <- fread(paste0("q3.4/", type, "/Inertial Signals/body_acc_y_", type, ".txt"))
    z <- fread(paste0("q3.4/", type, "/Inertial Signals/body_acc_z_", type, ".txt"))
    bodyacc <- data.table(x=x, y=y, z=z)
    
    x <- fread(paste0("q3.4/", type, "/Inertial Signals/body_gyro_x_", type, ".txt"))
    y <- fread(paste0("q3.4/", type, "/Inertial Signals/body_gyro_y_", type, ".txt"))
    z <- fread(paste0("q3.4/", type, "/Inertial Signals/body_gyro_z_", type, ".txt"))
    bodygyro <- data.table(x=x, y=y, z=z)
    
    x <- fread(paste0("q3.4/", type, "/Inertial Signals/total_acc_x_", type, ".txt"))
    y <- fread(paste0("q3.4/", type, "/Inertial Signals/total_acc_y_", type, ".txt"))
    z <- fread(paste0("q3.4/", type, "/Inertial Signals/total_acc_z_", type, ".txt"))
    totalacc <- data.table(x=x, y=y, z=z)
    
    label <- fread(paste0("q3.4/", type, "/y_", type, ".txt"))
    namedlabel <- factor(label[,V1], levels=labelnames$V1, labels=labelnames$V2)
    features <- fread(paste0("q3.4/", type, "/X_", type, ".txt"))
    colnames(features) <- featurenames
    
    all <- cbind(subjectid=subjects[,V1], bodyacc=bodyacc, bodygyro=bodygyro, totalacc=totalacc, namedlabel=namedlabel, features=features)
}

accGyroAnalysis <- function() {
    labelnames <- fread("q3.4/activity_labels.txt")
    featurenames <- fread("q3.4/features.txt")
    featurenames <- gsub("[()]", "", featurenames[,V2])
    featurenames <- gsub("[-,]", ".", featurenames)

    # Get the entire data into a single data table    
    traindata <- readAccGyroDataSet("train", labelnames, featurenames)
    testdata <- readAccGyroDataSet("test", labelnames, featurenames)
    all <- rbind(traindata, testdata)

    # Do the analysis of main features
    mainfeatures <- grep("^features\\.t.*(mean|std)\\.[XYZ]$", names(all), value = TRUE)
    maincols <- append(c("subjectid","namedlabel"), mainfeatures)
    main <- all[, maincols, with=F]
    #means <- main[, lapply(.SD, mean), by=.(subjectid,namedlabel)]
    #sortedmeans <- means[, .SD, keyby=.(subjectid,namedlabel)]
    sortedmeans <- main[, lapply(.SD, mean), by=.(subjectid,namedlabel)][order(subjectid,namedlabel)] # use of chaining
}


#==============================================================================
# Module 4
#==============================================================================
plotJpeg <- function(path, add=FALSE) {
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[1:2] # get the resolution
    if (!add) # initialize an empty plot area if add==FALSE
        plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
    return(jpg)
}

componentAnalysis <- function() {
    d <- plotJpeg("m4/obama.jpg")
    par(mfrow = c(2,2))
    plot(s$d^2/sum(s$d^2), xlab="Singular vector", ylab="Variance explained", main="Component analysis")
    approx <- s$u[, 1:5] %*% diag(s$d[1:5]) %*% t(s$v[, 1:5])
    image(approx[, nrow(approx):1], main="5 components")
    approx <- s$u[, 1:10] %*% diag(s$d[1:10]) %*% t(s$v[, 1:10])
    image(approx[, nrow(approx):1], main="10 components")
    approx <- s$u[, 1:20] %*% diag(s$d[1:20]) %*% t(s$v[, 1:20])
    image(approx[, nrow(approx):1], main="20 components")
}


#==============================================================================
# Assignment 4.1
#==============================================================================
powerConsumption <- function() {
    # Since data is huge, read in only what's relevant
    
    # One possible implementation but it's too slow
    # Incomplete: we are not saving the matched lines
    # con <- file("q4.1/household_power_consumption.txt", open="r")
    # header <- readLines(con, n=1, warn=F)
    # cols <- strsplit(header, ";")[[1]]
    # line <- readLines(con, n=1, warn=F)
    # i <- 0
    # while (length(line)>0) {
    #     if (length(grep("^[12]/02/2007", line))) i <- i + 1
    #     line <- readLines(con, n=1, warn=F)
    # }
    # close(con)
    # print(c("No. of records:", i))

    # One possible method: fastest method
    con <- file("q4.1/household_power_consumption.txt", open="r")
    header <- readLines(con, n=1, warn=F)
    d <- read.csv(pipe("grep -E '^[12]/2/2007' q4.1/household_power_consumption.txt"), header=F, sep=";")
    colnames(d) <- strsplit(header, ";")[[1]]
    
    # Another possible method
    #library(sqldf)
    #d <- read.csv.sql("q4.1/household_power_consumption.txt", sql = "SELECT * FROM file WHERE Date='1/2/2007' OR Date='2/2/2007'", sep = ";", eol = "\n")

    dt <- strptime(paste(d$Date, d$Time), "%d/%m/%Y %H:%M:%S")
    
    hist(d$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
    cat("Enter to continue ..."); readline()
    
    plot(dt, d$Global_active_power, type="l", xlab = "", ylab = "Global Active Power (kilowatts)")
    cat("Enter to continue ..."); readline()
    
    plot(dt, d$Sub_metering_1, type="l", col="black", xlab="", ylab="Energy sub metering")
    lines(dt, d$Sub_metering_2, type="l", col="red")
    lines(dt, d$Sub_metering_3, type="l", col="purple")
    legend("topright", lty=1,  col=c("black","purple","red"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    cat("Enter to continue ..."); readline()
    
    par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
    with (d, {
        plot(dt, d$Global_active_power, type="l", xlab = "", ylab = "Global Active Power")

        plot(dt, d$Voltage, type="l", xlab = "datetime", ylab = "Voltage")
        
        plot(dt, d$Sub_metering_1, type="l", col="black", xlab="", ylab="Energy sub metering")
        lines(dt, d$Sub_metering_2, type="l", col="red")
        lines(dt, d$Sub_metering_3, type="l", col="purple")
        legend("topright", lty=1,  col=c("black","purple","red"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        
        plot(dt, d$Global_reactive_power, type="l", xlab = "datetime", ylab = "Global_reactive_power")
    })
    par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0))
}

