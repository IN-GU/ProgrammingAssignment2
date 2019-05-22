## This script will cache the inverse of a matrix. 

## This function will create a list of functions to set a matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setInverse <- function() minv <<- solve(x) #calculate the inverse
  getInverse <- function() minv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function solves for a matrix's inverse if it hasn't been cached previously.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(minv)
  minv
  
}
