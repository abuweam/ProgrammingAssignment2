## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mat <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mat);
    setInverse <- function(value) inverse <<- value;
    getInverse <- function() return(inverse);
    return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(mat, ...) {
    inverse <- mat$getInverse()
    if(!is.null(inverse)) {
        message("returning the cached value")
        return(inverse)
    }
    data <- mat$get()
    inverse <- solve(data, ...)
    mat$setInverse(inverse)
    return(inverse)
}