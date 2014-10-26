## These functions look for the cache of a function - the inverse of a matrix.
## If the cached inverse of the matrix isn't found, then the function is run.
## After the function is run, a cache of the function is stored.

## This function creates a cache of the inverse of a matrix.  The cache is
## stored for later use in order to save processing time.

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


## This function looks to see if a cached answer is available.
## If there is no cached function, the function is run and passed to 
## a variable and stored for later.

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
