## The following functions cache and return the result of
## the inverse of a given square matrix. The
## matrix is assumed to be invertible.

## This function returns a custom matrix object
## that supports inverse matrix caching.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
    
        x <<- y
        inv <<- NULL
    }

    get <- function() {
        x
    }

    setinv <- function(inverse) {
        inv <<- inverse
    }

    getinv <- function() {
        inv
    }

    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if inverse matrix is cached
## and returns the cached value or calculates the inverse
## and stores it in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    
    data <- x$get()

    inv <- solve(data, ...)

    x$setinv(inv)

    inv
}
