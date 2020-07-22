## Put comments here that give an overall description of what your
## functions do
# These two functions together will return the inverse of a matrix. Before returning the inverse, it will first check to see if the inverse has already been calculated, and, if so, return the cached version. If it hasn't previously been calculated, it will cache the inverse.


## Write a short comment describing this function
#This function creates a matrix object that can cache its inverse. It returns a list object, which contains a set of four functions: set, which sets the matrix, get, which gest the matrix, setsolve, which sets the inverse of the matrix (calculated by the cacheSolve function), and getsolve, which gets the solved matrix (the inverse matrix). 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



## Write a short comment describing this function
# Ths functions calculates the inverse of the matrix created by makeCacheMatrix, using the Solve function. It first checks to see if the inverse has already been cached, and if so, returns the cached version instead.
cacheSolve <- function(x, ...) {
    # This function requires an object that is returned by makeVector in order to retrieve the mean
    # from the cached value that is stored in the makeVector object's environment
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
