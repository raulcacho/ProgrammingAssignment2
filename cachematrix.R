## Put comments here that give an overall description of what your
## functions do
## This program calculates the inverse of a matrix. If it has already been calculated,
## the preogram returns the value from the cache

## Write a short comment describing this function
## makeCacheMatrix changes the format of the input matrix to a list to store the requested values
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve actually calculates the inverse matrix of the input matrix.
## It checks for the inverse in the cache and returns it from there, if possible. 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
