## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of function to set the value, 
## get the value, set the inverse value and finally get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NUL
}
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}

## Checking if the computing of the inverse of the matrix has been calculated and returning the inverse of the matrix

cacheSolve <- function(x, ...) {
           inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
