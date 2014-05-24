## These functions are used to create a square matrix and subsequently find its inverse.
## The functions will cache the value of the inverse if it has been found previously.
## The inverse of the matrix is only calculated if it hasn't been previously calculated.

## makeCacheMatrix creates a matrix object and also stores its inverse the first time
## it's calculated

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve solves for the inverse of the matrix object passed as argument to it.
## It will used the cached inverse if it already exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
