## These functions demonstrates caching of matrices and their inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){ #set matrix data and inverse data
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse #caches inverse of matrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of a special "matrix" return by makeCacheMatrix above.  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' If cache data already exist retrieve the cache data or else compute it.
        inv <- x$getInverse() #retrieve cache data
        if(!is.null(inv)){ #if cache data exist retrieve cache
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...) #compute inverse of matrix
        x$setInverse(inv)
        inv
}
