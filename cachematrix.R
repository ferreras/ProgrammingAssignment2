##
## R PROGRAMING Course
## Programing Assigment 2
## Created: 5th May, 2014
## Last modified: 6th May, 2014
## Author: Antonio F.
##

##
## Creates a special matrix, in fact a list containing 4 functions
##      $set : set matrix value
##      $get : get matrix value
##      $setinverse : set inverse matrix value
##      $getinverse : get inverse matrix value
##

makeCacheMatrix <- function(a = matrix()) {
    
    inv <- NULL
    
    ## Define the 4 functions
    set <- function(b) {
        a <<- b
        inv <<- NULL
    }
    get <- function() a
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## Return a formated list with the four functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##
## Calculates the inverse of the special matrix created with makeCacheMatrix()
## First check if the inverse has already been calculated. In that case returns
## the inverse from the cache, otherwise calculates, stores and returns
## the inverse
##

cacheSolve <- function(a, ...) {
    
    ## Try to get te stored value of the inverse of 'a'
    inv <- a$getinverse()
    
    ## If not exists then calculate and store the inverse
    if(is.null(inv)) {
        inv <- solve(a$get())
        a$setinverse(inv)
    }
    
    return(inv)
}
