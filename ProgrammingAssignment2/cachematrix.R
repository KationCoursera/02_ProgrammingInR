## Calculate the inverse of a matrix and save it to cache in order to reduce computation time for next inverse computations
#
### Usage
#
# x_cache <- makeCacheMatrix(x)
# x_inverse <- cacheSolve(x_cache, ...)
# 
# (will return a message "Getting cached inverse of x." when inverse is not computed, but retrieved from cache)
#
### Arguments
#
# x - an inversible matrix
#
### Description (makeCacheMatrix)
#   
#   "Create" a matrix object that can cache its inverse. Returns a list of functions that can:
#
#   - set: initialize the matrix we are inverting
#   - get: get the value of the matrix we are caching
#   - setInverse: compute the inverse of the matrix and save it to cache
#   - getInverse: retrieve the cached version of the inverse or null if inverse was not yet computed
#

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


### Description (cacheSolve)
#   
#   Checks if there is a cached version of the matrix. If cache is empty, it calculates the inverse and saves it to cache.
#

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    
    ## Check if cache is available
    
    if(!is.null(i)) {
        message("Getting cached inverse of x.")
        return(i)
    }
    
    ## Calculate inverse if cache is empty
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
