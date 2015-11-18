## rprog-034: Week 3, Programming Assignment 2
## Kneale
## 11/18/2015

## 

## makeCacheMatrix function accepts a matrix value and caches both
## the value passed in and the inversed value of the matrix value
## the function also defines several functions that are used to 
## set and retrieve the matrix data.  
## the function returns a list of those functions in the form of a list 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y)
    {
        ## cache the y value passed into the set function
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## cacheSolve function accepts the "makeCacheMatrix" function
## cacheSolve calls the "getInverse" function from the makeCacheMatrix
## function that was passed in and returns the cached value. 
## If no value was cached, the cacehSolve function calls 
## the makeCacheMatrix "get" function to retrieve the cached matrix value,
## creates an inverse of the value, calls the "setInverse" function from the
## makeCacheMatrix to cache the inverse value and returns the inversed result.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i
}

## example of useage
## define the new matrix
m <- rbind(c(1, -1/4), c(-1/4, 1))
## verify the class type
class(m)
## returns "matrix"
## call the makeCacheMatrix and pass in the value of "m"
cacheTest <- makeCacheMatrix(x = m)
## we can test the get function
cacheTest$get()
## results:
##      [,1]  [,2]
##[1,]  1.00 -0.25
##[2,] -0.25  1.00
## pass cacheTest to the cacheSolve function
cacheSolveTest <- cacheSolve(cacheTest)
## check the value of cacheSolveTest
cacheSolveTest
## result
##      [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
