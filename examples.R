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

