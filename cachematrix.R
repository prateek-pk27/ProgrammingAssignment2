## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    createmat <- function(y) {
        ##Initialize the matrix whose and assigns the inverse NULL value 
        x <<- y
        inv <<- NULL
    }
    getmat <- function() x ##Returns the matrix
    setinv <- function(inverse) inv <<- inverse ##Assigns the inverse of matrix to the variable
    getinv <- function() inv ##Returns the inverse of the matrix
    list(createmat = createmat, getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv() 
    ##Checks and returns if the inverse of the matric is calculated before 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$getmat() ##Gets the matrix
    inv <- solve(data) ##Calculates the inverse
    x$setinv(inv) ##Saves the inverse to the variable
    return(inv)
}
