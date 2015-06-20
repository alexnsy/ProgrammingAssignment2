## This file contains 2 functions that attempt to reduce unneccessary computation of the 
## inverse of a matrix by allowing its inverse matrix to be cached from previous computations. 

## makeCacheMatrix function creates a special matrix object that stores the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  ## set: passes in and stores the matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  ## get: retrieves the stored matrix
  get <- function() x
  
  ## setinverse: passes in and caches the inverse of the matrix
  setinverse <- function(inv) inverse <<- inv
  
  ## getinverse: retrieves the cached inverse of the matrix
  getinverse <- function() inverse
  
  list(set = set, 
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function passes in the special matrix object and returns its inverse matrix.
## If there is already a cached inverse matrix, it will return the cached inverse matrix.
## Otherwise it will solve for the inverse matrix and cache it. 
## It is assumed that the matrix passed in is square and invertible. 
cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  
  ## If there is already a cached inverse matrix, return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Otherwise solve for the inverse matrix, cache and return it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
