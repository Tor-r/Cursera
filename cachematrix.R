# The following function creates a special "matrix",
# which is a list containing a function to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse in order to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solv
    getInverse <- function() m
    # return a list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function calculates the inverse of the special "matrix" 
# created with the "makeCacheMatrix" function. 
# This function first checks if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value 
# of the inverse in the cache via the "setinv" function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}


