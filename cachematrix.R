## These two functions cache the inverse of a matrix, so that if
## the inverse of a matrix has already been calculated, it can 
## simply be retrieved, rather than repeating the computing. 

## The makeCacheMatrix creates a special 'matrix' object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function () inv
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)

}

## The cacheSolve function computes the inverse of the special 'matrix'
## returned by makeCacheMatrix. If the inverse has already been calculated,
## then teh cachesolve should retriece the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
