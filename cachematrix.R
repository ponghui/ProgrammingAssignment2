## The first function makeCacheMatrix creates a matrix which has a cached inverted version
## of itself. This cached version has to be manually computed using the second function
## cacheSolve. There is no validation check require as solution assume matrix is always
## invertible by solve function as stated in the assignment.
##
## Possible improve can be made by calling the cacheSolve function automatically when
## setting the x parameter in the set function. However this will increase processing time.

## makeCacheMatrix is 4 functions 
##   get and set methods for a basic matrix
##   get and set methods for the cached inverted matrix of the basic matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverted <- function(inverted) i <<- inverted
        getInverted <- function() i
        list(set = set, 
             get = get, 
             setInverted = setInverted, 
             getInverted = getInverted)
}


## cacheSolve takes in a matrix and attempts to get the inverted matrix
## by accessing the cache (created from the function makeCacheMatrix). If cache is not
## available, this method will apply function solve on the parameter x

cacheSolve <- function(x, ...) {
        inverted <- x$getInverted()
        if(!is.null(inverted)){
                message("using cached matrix")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data,...) ##assume data is always invertible (no checks needed)
        x$setInverted(inverted)
        inverted
}
