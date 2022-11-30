## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 1. makeCacheMatrix
## The first function in the file, makeCacheMatrix() creates a 
## special "matrix" object that stores a vector and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initialize the objects and define the functions of objects
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  # Create a new object by returning a list()
  list(set = set, 
       get =get, 
       setInverse = setInverse,
       getInverse = getInverse)

}

## Write a short comment describing this function

##2. cacheSolve
## The second function, cacheSolve() requires an argument that is returned 
## by makeCacheMatrix() in order to retrieve the inverse from the cached value 
## that is stored in the makeCacheMatrix() object's environment.
## Without cacheSolve(), the makeCacheMatrix() function is incomplete. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## call the getInverse() function on the input object
  inv <- x$getInverse()
  # check to see if the value here is not equal to NULL, 
  # hence we have a valid, cached inverse and can return 
  # it to the parent environment
  
  if (!is.null(inv)) {
    message ("get cached data")
    return(inv)
    
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}


