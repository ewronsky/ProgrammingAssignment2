## Matrix inversion is a costly computation 
## and there may be some benefit to caching the inverse
## of a matrix rather than computing it repeatedly.


## makeCacheMatrix does the following:
## 1) sets & gets the value of the matrix
## 2) sets & gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)   
}

## cachesolve returns the inverse of the matrix
## constructed in the previous function

cachesolve <- function(x, ...) {

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cache")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

