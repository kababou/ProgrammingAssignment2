## Put comments here that give an overall description of what your
## functions do

## Making a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## Returns a matrix that is the inverse of 'x' (from cache if available, else computing)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmean()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
}