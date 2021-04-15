## To Cache Inverse of Matrix:
## Inversion of Matrix may be difficult but there are 
## advantages on doing this instead of computing it many times.
## below this are functions to create objects to  
## store matrix and caching the inverse. 

## This function makes "matrix" object to cache their inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function below determines the inverse of the special "matrix" made by the
## makeCacheMatrix. If the inverse is already calculated while the matrix did 
## not change, it should recover the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}