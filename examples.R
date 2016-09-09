mycolors <- function() {
    cc <- colorRamp(c("red","blue"))
    cc(0) # only red
    cc(1) # only blue
    cc(0.5) # equal mix of red and blue
    cc(seq(0, 1, len=10)) # 10 different colours varying from red to blue

    cc <- colorRampPalette(c("red","yellow"))
    cc(2) # returns 2 colours in hex: just red and yellow
    cc(10) # interpolation between red and yellow
    
    library(RColorBrewer)
    cols <- brewer.pal(3, "BuGn")
    pal <- colorRampPalette(cols)
    par(mfrow = c(1,2))
    image(volcano, col=pal(20))
    image(volcano) # default
    
    # Lots of points: smoothScatter plots the histogram
    x <- rnorm(10000)
    y <- rnorm(10000)
    par(mfrow = c(1,2))
    smoothScatter(x, y)
    smoothScatter(x, y, colramp = pal)
    
    rgb(1, 0, 0) # FF0000
    rgb(.1, 0, 0) # 1A0000: 1A = 16+10 = 26 ~ 10% of 255
    rgb(1, 0, 0, .1) # add transparency
    par(mfrow = c(1,2))
    plot(x, y, pch=19)
    plot(x, y, col=rgb(1, 0, 0, .1), pch=19) # use transparency to convey density of points
}

dimensionReduction <- function() {
    # create random data
    set.seed(12345)
    d <- matrix(rnorm(400), nrow=40)
    image(1:10, 1:40, t(d)[, nrow(d):1]) # no pattern here

    # simulate a pattern within the data
    set.seed(678910)
    for (i in 1:40) {
        coinFlip <- rbinom(1, size=1, prob=0.5)
        if (coinFlip) {
            d[i,] <- d[i,] + rep(c(0,3), each=5)
        }
    }
    image(1:10, 1:40, t(d)[, nrow(d):1]) # pattern visible on columns
    heatmap(d)
    clusters <- hclust(dist(d))
    dOrdered <- d[clusters$order, ]
    par(mfrow = c(1,3))
    image(1:10, 1:40, t(dOrdered)[, nrow(dOrdered):1])
    plot(rowMeans(dOrdered), 40:1, xlab="Row Mean", ylab="Row")
    plot(colMeans(dOrdered), xlab="Column", ylab="Column Mean")
    
    # Singular Value Decomposition
    s <- svd(scale(dOrdered))
    par(mfrow = c(1,3))
    image(1:10, 1:40, t(dOrdered)[, nrow(dOrdered):1])
    plot(s$u[, 1], 40:1, xlab="Row", ylab="First left singular vector")
    plot(s$v[, 1], xlab="Column", ylab="First right singular vector")
    par(mfrow = c(1,2))
    plot(s$d, xlab="Column", ylab="Singular value")
    plot(s$d^2/sum(s$d^2), xlab="Column", ylab="Proportion of variance explained")

    # SVD and Principal Component Analysis (PCA) are equivalent
    p <- prcomp(dOrdered, scale=T)
    plot(p$rotation[, 1], s$v[, 1], xlab="Principal component 1", ylab="Right singular value 1")
    plot(p$rotation[, 2], s$v[, 2], xlab="Principal component 2", ylab="Right singular value 2")
}

clustering <- function() {
    # hierarchical
    set.seed(1234)
    par(mar = c(1,1,1,1))
    x <- rnorm(12, mean = rep(1:3, each=4), sd = 0.2) # each value taken from a dist with its own mean
    y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd = 0.2)
    plot(x, y, col="blue", pch=19, cex=2)
    text(x+0.05, y+0.05, labels=as.character(1:12))
    d <- data.frame(x, y)
    distances <- dist(d)
    hClusters <- hclust(distances)
    plot(hClusters)
    heatmap(data.matrix(d))
    
    # kmeans
    k <- kmeans(d, centers=3)
    plot(x, y, col=k$cluster, pch=19, cex=2)
    points(k$centers, col=1:3, pch=3, cex=3, lwd=3) # plot cluster centers
    image(t(d)[, nrow(d):1], yaxt="n") # heatmap in original order
    image(t(d)[, order(k$cluster)], yaxt="n") # heatmap ordered by cluster
    par(mfrow = c(1,2))
}

ggplotExamples <- function() {
    library(ggplot2)
    library(datasets)
    
    qplot(displ, hwy, data=mpg)
    qplot(displ, hwy, data=mpg, shape=drv)
    qplot(displ, hwy, data=mpg, col=drv)
    
    qplot(displ, hwy, data=mpg, geom=c("point","smooth")) # gray area is 95% confidence interval
    qplot(displ, hwy, data=mpg, col=drv, geom=c("point","smooth"))
    
    # histogram from a single variable
    qplot(hwy, data=mpg)
    qplot(hwy, data=mpg, fill=drv)
    qplot(log(hwy), data=mpg, bins=10)
    qplot(log(hwy), data=mpg, geom="density")
    qplot(log(hwy), data=mpg, geom="density", col=drv) # separating the peaks
    
    # facets are like panels of lattice plots
    qplot(displ, hwy, data=mpg, facets=.~drv)
    qplot(hwy, data=mpg, facets=drv~., binwidth=2)

    # use transform
    airquality = transform(airquality, Month = factor(Month))
    qplot(Wind, Ozone, data = airquality, facets = . ~ Month)
    
    # combine two calls
    qplot(displ, hwy, data=mpg, col=drv) + geom_smooth(method="lm")
    qplot(displ, hwy, data=mpg, facets=.~drv) + geom_smooth(method="lm")
    
    # using the basic ggplot
    g <- ggplot(mpg, aes(displ, hwy))
    print(g) # plot will be blank
    p <- g + geom_point()
    print(p) # scatterplot comes up
    g + geom_point() # as above but autoprinted to screen
    g + geom_point() + geom_smooth() # could be affected by noise (where too few data points are available)
    g + geom_point() + geom_smooth(method="lm") # linear regression
    g + geom_point() + facet_grid(. ~ drv) + geom_smooth(method="lm") # columns of drv
    g + geom_point() + facet_grid(drv ~ .) + geom_smooth(method="lm") # rows of drv
    g + geom_point(color="steelblue", size=.5, alpha=.8) # color is constant
    g + geom_point(aes(color=drv), size=2, alpha=.8) # color is wrapped in aes: linked to value of a data variable
    g + geom_point(aes(color="steelblue"), size=2, alpha=.8) # wrong usage: only one colour with legend of one value "steelblue"
    g + geom_point() + labs(title="xy", x="xxx", y="yyy") # title and axis labels
    g + geom_point() + labs(title="xy", x=expression("X "*YZ[abc]), y="yyy") # subscript in label
    g + geom_point() + geom_smooth(size=2, linetype=3, method="lm", se=F)
    g + geom_point() + geom_smooth(size=.8, linetype=2, method="lm", se=F)
    g + geom_point() + theme_bw()
    g + geom_point() + theme_bw(base_family = "Verdana") # change font
    g + geom_point() + ylim(0,50) # ensures y-axis starts from zero
    g + geom_line() + ylim(10,40) # points out of range will be ignored
    g + geom_line() + coord_cartesian(ylim = c(10,40)) # points out of range will be considered but outside plot boundaries

    g + geom_point() + facet_grid(. ~ cty) # cannot do useful conditioning on a variable with many values or on continuous variables
    bins <- quantile(mpg$cty, seq(0,1,length=4), na.rm=T)
    mpg$bins <- cut(mpg$cty, bins)
    g <- ggplot(mpg, aes(displ, hwy)) # need to reconstruct
    g + geom_point() + facet_grid(. ~ bins) # this will show an NA group
    bins["0%"] <- bins["0%"]-1 # to remove NA group
    mpg$bins <- cut(mpg$cty, bins)
    g <- ggplot(mpg, aes(displ, hwy)) # need to reconstruct
    g + geom_point() + facet_grid(. ~ bins)
}

latticeplotExamples <- function() {
    library(lattice)
    library(datasets)
    
    xyplot(Ozone ~ Wind, data = airquality)

    airquality <- transform(airquality, Month = factor(Month))
    xyplot(Ozone ~ Wind | Month, data = airquality, layout=c(5,1))

    p <- xyplot(Ozone ~ Wind | Month, data = airquality, layout=c(5,1))
    print(p) # do manual printing    

    x <- rnorm(100)
    f <- rep(0:1, each=50)
    y <- x + f - f * x + rnorm(100, sd=0.5)
    f <- factor(f, labels=c("Group 1","Group 2"))
    xyplot(y ~ x | f, layout=c(2,1)) # two panels are produced

    # Custom panel function
    xyplot(y ~ x | f, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...) # use default panel function
        panel.abline(h=median(y), lty=2) # customize
        panel.lmline(x, y, col="red") # customize
    })
    
}

baseplotExamples <- function() {
    par("bg")
    par("lty")
    par("pch")
    par("bg", "lty", "pch")
    par("mar") # bottom, left, top, right margins
    
    plot(rnorm(100), rnorm(100), xlab="x", ylab="y")

    hist(airquality$Ozone)
    hist(airquality$Ozone, breaks=20)
    abline(v=median(airquality$Ozone, na.rm=T), col="purple", lwd=4)
    rug(airquality$Ozone)

    par(bg="wheat")
    plot(airquality$Ozone, airquality$Wind)
    par(bg="white")

    boxplot(Ozone ~ Month, airquality)
    abline(h=100, lty="dashed", col="red")

    airquality <- transform(airquality, Month = factor(Month))
    boxplot(Ozone ~ Month, airquality, xlab="Month", ylab="Ozone (ppb)")
    title(main = "Monthly Ozone Levels")

    # a line graph
    plot(seq(1,1000,length.out=100), rnorm(100), type="l", col="wheat", lwd=2)
    
    d <- sample(c("Yes", "No"), 100, replace=T)
    barplot(table(d))
    
    # title set via plot(), plot the points separately, add legend
    with(airquality, plot(Wind, Ozone, main = "Ozone & Wind", type = "n"))
    with(subset(airquality,Month==5), points(Wind, Ozone, col="blue"))
    with(subset(airquality,Month!=5), points(Wind, Ozone, col="red"))
    legend("topright", pch=1,  col=c("blue","red"), legend=c("May","Other Months"))

    # alternative way of subsetting
    with(airquality, plot(Wind, Ozone, main = "Ozone & Wind", type = "n"))
    with(airquality[airquality$Month==5,], points(Wind, Ozone, col="blue"))
    with(airquality[airquality$Month!=5,], points(Wind, Ozone, col="red"))
    legend("topright", pch=1,  col=c("blue","red"), legend=c("May","Other Months"))
    
    model <- lm(Ozone ~ Wind, airquality)
    abline(model, lty="dotted", lwd=2)
    
    par(mfrow=c(1,3), mar=c(4,4,2,1), oma=c(0,0,2,0))
    with (airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
        plot(Temp, Ozone, main = "Ozone and Temperature")
        mtext("Ozone and Weather in NY City", outer=T)
    })
    par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0))

    # plot to a file
    pdf(file = "sampleplot.pdf")
    plot(rnorm(100), rnorm(100), xlab="x", ylab="y")
    dev.off()

    # plot to screen and then save
    plot(rnorm(100), rnorm(100), xlab="X", ylab="Y")
    dev.copy(png, file = "sampleplot.png")
    dev.off()
}

dplyrExamples <- function() {
    library(dplyr)
    
    df <- data.frame(ID = c("b","b","b","b","a","a","c"), A = 1:7, B = 7:13, C = 14:20)
    df[,match("ID", colnames(df)):match("B", colnames(df))] # a range of cols by name
    select(df, ID:B) # lot easier with dplyr
    select(df, -(ID:B))
    filter(df, A>3 & B<12)
    arrange(df, ID)
    arrange(df, ID, desc(C)) # ID ascending, C descending
    df <- rename(df, AA=A, CC=C) # rename columns
    df <- mutate(df, Adev = AA - mean(AA, na.rm=T)) # add a column
    df <- mutate(df, oe = factor(df$B%%2, labels=c("ODD", "EVEN"))) # category labelling
    oddeven <- group_by(df, oe)
    summarize(oddeven, AAm=mean(AA, na.rm=T), Bm=median(B, na.rm=T))
    df <- mutate(df, date = c("2015-05-11", "2016-05-11", "2012-05-11", "2012-04-11", "2013-05-11", "2013-04-11", "2014-05-11"))
    df <- mutate(df, year = as.POSIXlt(date)$year + 1900)
    summarize(group_by(df, year), AAm=mean(AA, na.rm=T), Bm=median(B, na.rm=T))
    df <- df[, !names(df)=="year"] # drop year column
    df %>% mutate(year = as.POSIXlt(date)$year + 1900) %>% group_by(year) %>% summarize(AAm=mean(AA, na.rm=T), Bm=median(B, na.rm=T)) # pipelining operations
    
    a <- data.frame(id=sample(1:10), x=rnorm(10))
    b <- data.frame(id=sample(1:10), y=rnorm(10))
    c <- data.frame(id=sample(1:10), z=rnorm(10))
    arrange(join(a,b), id) # join two datasets by common names
    join_all(list(a,b,c)) # join more than two datasets
}

textExamples <- function() {
    n <- c("lat.long", "height.weight", "bmi", "name")
    strsplit(n, ".", fixed=T)
    strsplit(n, "\\.")
    sapply(strsplit(n, "\\."), function(x) x[1]) # get only first parts

    grep("e", n) # indices
    grep("e", n, values=T) # values
    grepl("e", n) # logical vector
    n[grepl("e", n)] # only those with e in it
    n[!grepl("e", n)] # only those without e in it
    table(grepl("e", n))
    length(grep("x", n))==0 # check if something doesn't exist
    
    toupper(n)
    tolower(n)
    nchar(n) # count characters per element
    substr(n, 1, 6) # substring with start and stop markers
    paste(1:length(n), n, sep=".") # add a prefix

    trimws("  program    ")
    trimws("  program    ", which = "right")
}

dateExamples <- function() {
    d <- date()
    class(d) # character
    
    sd <- Sys.Date() # yyyy-mm-dd
    class(sd) # Date
    format(sd, "%d-%m-%Y")
    format(sd, "%a %b %d %Y")  # Wed Aug 24 2016
    format(sd, "%a %B %d '%y")  # Wed August 24 '16
    weekdays(sd)
    months(sd)
    julian(sd) # number of days since epoch
    
    dates <- c("1jan2005", "21feb2013", "8OCT2014")
    z <- as.Date(dates, "%d%b%Y")
    z[1]-z[2] # get time difference in days
    as.numeric(z[1]-z[2]) # get time difference
    
    library(lubridate)
    ymd("20141023")
    mdy("10/23/2014")
    dmy("23-10-2014")
    ymd_hms("20141023 13:15:03")
    ymd_hms("20141023 13:15:03", tz="Asia/Kolkata")
    wday(dmy("23-10-2014"))
    wday(dmy("23-10-2014"),label=T)

    Sys.timezone() # returns NA
}

miscExamples <- function() {
    rep(0:1) # 0 1
    rep(0:1, times=5) # 0 1 0 1 0 1 0 1 0 1
    rep(0:1, length.out=5) # 0 1 0 1 0
    rep(0:1, each=5) # 0 0 0 0 0 1 1 1 1 1
    
    d <- paste("V", c(1,2,3,5), c(7,8,9,4), sep="x")
    print(d)

    y <- rnorm(1:10)
    yesno <- sample(c("yes","no"), size=10, replace=T)
    y[yesno=="yes"]

    y <- rnorm(1:10)
    seq_along(y)
    seq(1, 20, along=y)
    seq(1, 20, by=2)
    seq(1, 20, length=10)

    y <- rnorm(1:10)
    ifelse(y<0, "Neg", "Pos")
    groups <- cut(y, breaks=quantile(y)) # groups is a factor variable
    table(groups)
    table(groups, y) # shows which values fall into which groups
    
    set.seed(45)
    d <- data.frame(a=1:5, b=6:10, c=11:15)
    d <- d[sample(1:5),] # rearrange rows
    d$b[c(1,3)] <- NA # set couple of elements to NA
    sum(is.na(d$b)) # count of NA values
    any(is.na(d$b))
    all(is.na(d$b))
    colSums(is.na(d))
    rowSums(is.na(d))
    table(d$a %in% c(4,2)) # count of TRUE/FALSE from this condition
    table(d$a == 2) # == works for a single value
    d[d$a %in% c(4,2),] # use logical vector to subset
    table(d$b, useNA="ifany") # count NA occurences as well
    table(d$a, d$b) # count combination of a and b
    d[(d$a>3 | d$c>6),] # subset by conditions: condition returns logical vector
    d[(d$a>3 & d$c>6),]
    d[which(d$b>6),] # need to use which() when some values are NA: which() returns indices
    sort(d$b, na.last=T) # NA values are retained and come at the end
    d[order(-d$c),] # descending order by column c values
    d$d <- 16:20 # add a new column at the end
    d <- cbind(d, e=21:25) # alternative to add a new column
    print(d)

    a <- data.frame(id=1:5, bid=3:7, value=rnorm(5))
    b <- data.frame(id=3:7, value=rnorm(5), err=rnorm(5))
    merge(a, b, by.x="bid", by.y="id", all=T) # merge from multiple datasets
    intersect(names(a), names(b))
    
}

dataTableExamples <- function() {
    # Ref. https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-intro.html
    DT <- data.table(ID = c("b","b","b","b","a","a","c"), A = 1:7, B = 7:13, C = 14:20)

    # append a new column
    print("==Append==")
    DT[, D:=C+10]
    print(DT)
    
    # aggregation and in some cases add new columns; [i, j, by]
    print("==Aggregate==")
    print(DT[, sum(C+D)])
    print(DT[, sum(C+D), by=ID])
    print(DT[, .N, by=ID]) # histogram (count) by ID
    print(DT[, .N, by=.(ID,A)]) # .() for when multiple columns are involved
    print(DT[, .(.N, Z=sum(B+C)), by=.(ID,A)])
    print(DT[, .(.N, sum(B+C)), by=.(ID,A)]) # as above but column name autogenerated
    print(DT[, sum((C+B)>23), by=ID]) # count of records with (C+B)>10 and aggregate by ID
    print(DT[, sum((C+B)>23), .N]) # total count matching the expression
    print(DT[, .(ma=mean(A), md=mean(D))])
    print(DT[, .(ma=mean(A), md=mean(D)), by=ID])
    print(DT[A%%2!=0, .(ID,A,C,D,B)]) # only rows matching a condition, reorder columns
    print(DT[A%%2!=0, .(A,C,D,B), by=ID]) # by=ID has no effect since there's no aggregation
    print(DT[A%%2!=0, sum(A+B+C+D), by=ID])
    print(DT[A%%2!=0, .(A,C,D,B), keyby=.(ID)]) # sort after aggregration
    print(DT[, .N, .(B<10,D>25)]) # by can accept expressions, not just columns

    # aggregration by using .SD: subset of data
    print("==Aggregate .SD==")
    print(DT[, print(.SD), by=ID])
    print(DT[, lapply(.SD, mean), by=ID])
    print(DT[, lapply(.SD, sd), by=ID]) # standard deviation will return NA when there is only one sample
    print(DT[, lapply(.SD, max), by=ID, .SDcols=c("A","B")]) # only some columns
    print(DT[, head(.SD, 1), by=ID]) # only 1st row of each group
    print(DT[, .(val = c(A,B)), by=ID]) # concatenate A and B by ID
    print(DT[, .(val = list(c(A,B))), by=ID]) # concatenate and return as list
    
    # returns a vector
    print("==Return vector==")
    ccol <- DT[,C]
    print(c(class(ccol), ccol))

    # returns a data.table
    print("==Return data.table==")
    ccol <- DT[,list(C)]
    print(c(class(ccol), ccol))
    ccol <- DT[,.(C)] # dot is an alternative to list
    print(c(class(ccol), ccol))
    ccol <- DT[,.(aa=A,cc=C)] # multiple columns and rename
    print(c(class(ccol), ccol))

    # returns a data.table by traditional data.frame syntax
    print("==Return data.table using with=FALSE==")
    ccol <- DT[, c("A","C"), with=F]
    print(c(class(ccol), ccol))
    ccol <- DT[, !c("A","C"), with=F] # all columns except A and C
    print(c(class(ccol), ccol))
    
    # subsetting rows
    print("==Subset Rows==")
    print(DT[DT$ID=="b",]) # using comma with data.table is accepted though not necessary
    print(DT[ID=="b"]) # comma not required as in data.frame, ID can be used instead of DT$ID
    print(DT[1:2]) # first two rows

    # ordering
    print("==Order==")
    print(DT[order(ID,-B)]) # ascending ID, descending B
    print(DT[A%%2!=0, .(A,B,Z=sum(C+D)), by=ID][order(-Z,ID)])  # shows chaining of operations using [][]...

    return(DT)
}

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

irisAnalysis <- function() {
    library(datasets)
    data(iris)
    
    # Mean of Sepal.Length for virginica
    v <- iris[iris["Species"]=="virginica",]
    print(round(mean(v$Sepal.Length)))

    # Mean of metrics across species
    print(colMeans(iris[, 1:4]))
    print(sapply(iris[, 1:4], mean))
}

mtcarsAnalysis <- function() {
    library(datasets)
    data(mtcars)

    # Mean miles per gallon by no. of cylinders
    print(tapply(mtcars$mpg, mtcars$cyl, mean))
    print(with(mtcars, tapply(mpg,cyl,mean)))
    print(sapply(split(mtcars$mpg, mtcars$cyl), mean))
    
    # Absolute difference in horsepower
    hp <- tapply(mtcars$hp, mtcars$cyl, mean)
    print(round(abs(hp["4"] - hp["8"])))
}

printme <- function(x) {
    if (x > 0) print("x is positive.")
    else print("x is zero or negative.")
    invisible(x)
}

colmean <- function(x, removeNA = TRUE) {
    nc <- ncol(x)
    means <- numeric(nc)
    for (i in 1:nc) {
        means[i] <- mean(x[, i], na.rm = removeNA)
    }
    means
}

make.power <- function(n) {
    pow <- function(x) {
        x^n
    }
    pow
}
cube <- make.power(3)
sqr <- make.power(2)

orderme <- function() {
    dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
            levels = c("Low", "Med", "Hi"), ordered = TRUE),
            x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
            z = c(1, 1, 1, 2))
    print(dd[with(dd, order(-z, b)), ])
    print(dd[order(-dd$z, dd$b),])
    print(dd[ order(-dd[,4], dd[,1]), ])
}

secsnow <- function() {
    now <- Sys.time() # now is in POSIXct
    print(unclass(now))
    print(strftime(now, "%B %d, %Y %H:%M:%S"))
    y <- unclass(as.POSIXlt(now))
    print(names(y))
    print(y$sec)
}

namer <- function() {
    df <- setNames(data.frame(as.list(1:5)), LETTERS[1:5])
    print(df[,c("A","B","E")])
}

