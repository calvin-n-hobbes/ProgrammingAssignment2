## Creates a special matrix object that caches its inverse

makeCacheMatrix <- function(mtx = matrix()) {
    inv <- NULL

    # function to initialize a matrix and clear the cached inverse
    set <- function(y) {
        mtx <<- y
        inv <<- NULL
    }

    # function to return the original matrix
    get <- function() mtx

    # function to cache a matrix's inverse
    setinverse <- function(inverse) inv <<- inverse

    # function to return the cached inverse
    getinverse <- function() inv

    list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## Uses the R solve function to compute the inverse of a square matrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # if the inverse wasn't cached, calculate it
    inv <- solve(a = x$get())
    x$setinverse(inv)
    inv
}
