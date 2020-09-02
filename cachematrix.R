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
        xinv <- solve(data, ...)
        x$setinv(xinv)
    }

    xinv
}

## A test function to evaluate the cacheSolve
## First parameter proves the dimensions, second parameter provides the number of times
## the inverse have to be called
## Returns the some of all elements in the product of the matrix and its inverse
## It should be equal to the dimension of the matrix (n)
test <- function(n, k) {
    A <- matrix(rexp(n*n, rate=.1), ncol=n)
    fA <- makeCacheMatrix(A)
    for(i in 1:k)
        fAinv <- cacheSolve(fA)
    sum(fAinv %*% A)
}

print("10x10 matrix")
print(test(10, 4))

print("20x20 matrix")
print(test(20, 7))

print("1000x1000 matrix")
print(test(1000, 10))