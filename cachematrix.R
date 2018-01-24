## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss
## here).

## This pair of functions can be used to calculate and cache the inverse of a
## matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        get <- function() x
        set <- function(m) {
                x <<- m
                
                # Invalidate cached value
                inverse <<- NULL
        }
        getinverse <- function() inverse
        setinverse <- function(inv) inverse <<- inv
        
        # return list with methods
        list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse))
        {
                message("Cached value used.")
                return(inverse)
        }
        
        inverse <- solve(x$get())
        x$setinverse(inverse)
        
        # Return inverse of matrix 'x'
        inverse
}
