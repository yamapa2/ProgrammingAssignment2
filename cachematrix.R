## Matrix inversion is usually a costly computation, especially for large matrices
## These functions provide a way to optimize the computation by caching the matrix inverse,
## so repeated calls to find an inverse of a matrix can save computation cost

## This function provides wrapper object that can be used to cache the inverse of a given matrix
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Compute the inverse of givn cached matrix created using "makeCacheMatrix" function
## IF the inverse is already cached, this function returns the same
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    
    if(!is.null(xinv)) {
        message("getting cached inverse")
    }
    else {
        message("calculating inverse")
        data <- x$get()
        xinv <- solve(data)
        x$setinv(xinv)
    }

    xinv
}