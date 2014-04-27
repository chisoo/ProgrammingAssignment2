## This program accepts a matrix and cache's its inverse 
## functions do

## This program creates a special vector, which is really a list containing a function to 
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    
    setInverse <- function(solve) s <<- solve
    getInverse <- function() s
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This program checks if the inverse of the matrix has been already calculated. 
## If it has, it returns that matrix, if not, it calculates a new inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## checks if the inverse has already been calculated. 
    ## If so, it gets the inverse from the cache and skips the computation.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## calculates the inverse of the data
    data <- x$get()
    inv <- solve(data)
    
    ## sets the value of the inverse in the cache via the setsolve function
    x$setInverse(inv)
    inv
}
