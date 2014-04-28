## The first function, makeCacheMatrix creates a matrix and caches its inverse.
## The second function, cacheSolve determines the inverse of the matrix if it is
## not cached. Else it retrieves the cached inverse.

## makeCacheMatrix creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        xinverse <- NULL
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        get <- function () x
        setinv <- function (solve) xinverse <<- solve
        getinv <- function () xinverse
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
        
}


## cacheSolve computes the inverse of the matrix returned by the function makeCacheMatrix
## The matrix is first converted to a data frame to allow its use with the $ operator.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x <- as.data.frame(x)
        xinverse <- x$getinv()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
        data <- x$get()
        xinverse <- solve(data, ...)
        x$setinv(xinverse)
        xinverse
}
