## R PROGRAMING Course
## Programing Assigment 2
## Created: 5 May, 2014
## Last modified: 5th May, 2014
## Author: Antonio F.

##

## Creates a special matrix, really a lit containing 4 functions
##      $set : set matrix value
##      $get : get matrix value
##      $setinverse : set inverse matrix value
##      $getinverse ; get inverse matrix value

makeCacheMatrix <- function(a = matrix()) {
    inv <- NULL
    set <- function(b) {
        a <<- b
        inv <<- NULL
    }
    get <- function() a
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of the special matrix created with makeCacheMatrix()
## First check if the inverse has already been calculated. In that case returns
## the inverse from the cache, otherwise calculates, stores and returns
## the inverse

cacheSolve <- function(x, ...) {
    
    ## Try to get te stored value of the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
    } else {
        message("solving")
        inv <- solve(x$get())
        x$setinverse(inv)
    }
    return(inv)
}
