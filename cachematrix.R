## Calculating the inverse of a matrix is usually a time consuming operation.
## The following couple of functions helps you caching the result so it should
## not be calculated again.
## To use them create a special matrix object with the makeCacheMatrix function
## then call cachSolve passing this special matrix as its argument.
## Example:
##   m <- cbind(c(1,2),c(3,4))
##   cm <- makeCacheMatrix(m)
##   cacheSolve(cm)

## Creates a special matrix object that can be passed to the cacheSolve function
## The argument of this function should be a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of a matrix caching its result. If called more than one
## time on the same object it get the result from the cache instead of 
## computing it again.
## The argument should be a special matrix object obtained calling the 
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
