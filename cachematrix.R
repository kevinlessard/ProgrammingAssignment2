## This is a set of functions that can create a special matrix object
## and can cache its inverse (makeCacheMatrix) and then either returns 
## the cached inverse matrix or computes, caches and returns the 
## inverse if not yet calculated, or the matrix has changed (cacheSolve)

##  Note - Matrix created must be an invertible matrix


## Creates a special Matrix oject that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  
}

## Retrives and returns the inverse matrix from the cache in makeCacheMatrix.
## If no cache exists, or matrix has changed, then calculates, caches
## and returns the inverse matrix set in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
        

