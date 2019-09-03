## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    createmat <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmat <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(createmat = createmat, getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$getmat()
    inv <- solve(data)
    x$setinv(inv)
    return(inv)
}
