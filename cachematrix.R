## makeCachematrix returns a list of functions which set and get 
## the definition of the given input matrix, and its inverse

## usage: mylist = makeCacheMatrix(myMatrix)
## myMatrix must be a square invertible matrix

## Now each of the functions can be accessed as follows:
##  mylist$set(myMatrix) - set the value of my Matrix
##  mylist$get(myMatrix) - get the value of my Matrix
##  mylist$setinverse(myMatrix) - set the inverse of my Matrix
##  mylist$getinverse(myMatrix) - get the inverse of my Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set the matrix to the given value y; initialize inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##  fetch the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get the inverse of the matrix
  getinverse <- function() inv
  
  # return a list of the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function returns the inverse of the given matrix using
## a cached value if one exists.
## 
## Usage myInverse = cacheSolve(B)
## where B is a square invertible matrix.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## set up the functions which manipulate the matrix
  xx <- makeCacheMatrix(x)
  
  ## get the inverse from the cache if possible
  inv <- xx$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## calculate the inverse if the cache value was NULL
  inv <- solve(x,...)
  
  ## set the inverse in the cache
  xx$setinverse(inv)
  
  ## return the inverse of the given matrix
  inv
}
