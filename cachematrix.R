## A pair of functions that cache the inverse
## of a matrix

## makeCacheMatrix creates a special matrix
## that can cache its inverse

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
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of a
## makeCacheMatrix. if the inverse has already
## been calculated and the makeCacheMatrix is
## unchanged, then cacheSolve retrieves the inverse
## from its cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("creating inverse of matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
