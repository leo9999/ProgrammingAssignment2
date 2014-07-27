## The following functions cache the inverse of a matrix.


## "makeCacheMatrix" creates the functions to cache the inverse of a matrix.

makeCacheMatrix <- function(x=matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

## "cacheSolve" calculates the inverse of the matrix returned from makeCacheMatrix().
## If the inverse matrix has already been calculated, it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if (!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}

