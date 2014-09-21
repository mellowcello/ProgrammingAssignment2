## Here are 2 functions to enable the creation of a matrix object that can cache
##its inverse and a function to return the cached value of the inverse, 
##or to compute the inverse if it has not been cached.

## makeCacheMatrix creates a "matrix" object that can cache its inverse. It creates
## a list containing a function to set the value of the vector, get that value, 
## set the inverse and get that value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of input "x", this will be the cached value
## if that has been computed, if not it will compute the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

