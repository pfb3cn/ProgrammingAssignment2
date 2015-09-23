## These functions create a special matrix object and cache its inverse.
## The intention is to minimize unneccesary computing time for larger matrices
## by caching the inverse value instead of recomputing it every time it's needed.

## makeCacheMatrix creates a special matrix which contains functions to set the
## matrix, get the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If the inverse has already been calculated, cacheSolve returns the inverse
## from the cache (no computation).  Otherwise, it computes the inverse of the
## matrix from makeCacheMatrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("retrieving cached data")
        return(inv)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
