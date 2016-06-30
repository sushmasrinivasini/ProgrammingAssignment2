## Put comments here that give an overall description of what your
## functions do
## the below function check the cache before computing the inverse of the matrix
## if the inverse already exists, then it is simply retrieved and not computed again

## Write a short comment describing this function
## makeCacheMatrix creates an object which caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Write a short comment describing this function
## This computes the inverse of a matrix, but only after checking if it is
## alreadt=y created

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
}
