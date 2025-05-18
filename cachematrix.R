## This file contains two functions that work together to cache the inverse of a matrix.
## Since computing the inverse of a matrix is a potentially costly operation, these
## functions are useful to avoid recomputing it if the matrix has not changed.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL

    # Function to set a new matrix and reset cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # Function to get the current matrix
    get <- function() x

    # Function to set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse

    # Function to get the cached inverse
    getinverse <- function() inv

    # Return a list of the above functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Try to get the cached inverse

    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }

    # If not cached, compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)

    # Cache the computed inverse
    x$setinverse(inv)

    inv
}
