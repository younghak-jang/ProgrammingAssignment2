## Caching and calling inverse matrix

## defines list of functions that set&get inverse matrix

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


## calculates the inverse. argument: returned list of 'makeCacheMatrix' function above.
## if the reverse was already cached, returns the cached value,
## not cached, then calculate the inverse, cache it and return it

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached reververse matrix")
    return(m)
  } else {
    m <- solve(x$get())
    x$setinverse(m)
    return(m)
  }
}
