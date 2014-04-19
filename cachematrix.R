## This file contains two functions. The first returns a list containing
## closures which store a normal matrix object and cache the inverse the 
## matrix when first calculated. The other function does the inversion 
## and stores the result in the object returned by the first method.

## The "makeCacheMatrix" function returns a list which is a wrapper 
## around a normal matrix object. It holds the matrix object itself, 
## the inverse of the matrix (if calculated) and methods for setting 
## and getting the matrix object. Furthermore it exposes methods for 
## getting and setting a variable containing the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}

## The "cacheSolve" function uses the list that the "makeCacheMatrix" 
## function returns (let us call that a CacheMatrix) to get a cached 
## version of the inverse  if available and otherwise calculate it and 
## store it in the CacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
