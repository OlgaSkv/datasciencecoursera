## Assignment: Caching the Inverse of a Matrix
## makeCacheMatrix() and cacheSolve(x) are a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inv) x_inv <<- inv
        get_inv <- function() x_inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if (!is.null(inv)){
                message("getting cached inverse matrix value")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$set_inv(inv)
        return(inv)
}
