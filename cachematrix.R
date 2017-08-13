## Caching the Inverse of a Matrix:
## Inverting a Matrix is usually too expensive to do (in terms of computation cost).
## Hence, we do caching. caching the inverse of a matrix rather than compute it repeatedly.



makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##this below function finds the inverse of the matrix created by the above function.

cacheSolve <- function(x, ...) {
  inv = x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat = x$get()
  inv = solve(mat, ...)
  x$setInverse(inv)
  inv
}
