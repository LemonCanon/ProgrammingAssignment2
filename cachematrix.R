## Ryan Porter
## 23-02-2016

## Create a special matrix for caching the inverse. Based on sample provided
## with the assignment. 

## $set() sets the value of the matrix
## $get() prints the value of the matrix
## $setinv() sets the value of the inverse matrix
## $getinv() prints the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x  <<- y
        i  <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cachsolve() returns the inverse of the matrix 'x'. If the value is 
## already known then it will print the already calculated value. 
## NOTE: this function is intended to work with a special matrix created 
## by the makeCacheMatrix() funtion.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    return(i)
}

## All work based heavily on examples provided in the assignment for the R
## programming course provided on Coursera.
