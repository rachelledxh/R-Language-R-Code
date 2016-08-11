makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inve <<- solve
  getinverse <- function() inve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  inve <- x$getinverse()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setinverse(inve)
  inve
}


