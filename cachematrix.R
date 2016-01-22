
## makeCacheMatrix && cacheSolve Are functions that allow caching 
## results of a matrix inverse calculation, in order to optimize 
## computing resources
## 
## EXAMPLE:
## > m <- makeCacheMatrix(matrix(rnorm(25),5,5)) 
## > cacheSolve(m)    # First call calculates inverse 
## > cacheSolve(m)    # Second call retrieves inverse from cache



## makeCacheMatrix:
## This function creates a special "matrix" object that can cache
## its inverse so it can be looked up in the cache rather than recomputed

makeCacheMatrix <- function(x = matrix()) {
  ## cached inverse
  inverse <- NULL
  ## Set the value of the vector
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## Get the value of the vector
  get <- function() x
  ## Set the value of the inverse
  setinv <- function(inv) inverse <<- inv
  ## Get the value of the inverse
  getinv <- function() inverse
  ## Return list of methods used 
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Look for  inverse in cache
  inv <- x$getinv()
  ## return inverse if in cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if no inverse in cache , calculate inverse of raw matrix
  data <- x$get()
  inv <- solve(data)
  ## store inverse in cache
  x$setinv(inv)
  ## return inverse 
  inv
}
