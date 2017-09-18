# Caching the Inverse of Matrix:
# Matrix inversion is a costly computation 
# Written below is function that is used to create a special object that stores a matrix and caches its inverse.

# This function creates a special "matrix" object that can cache its inverse.

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


#This function will compute the inverse of matrix created by makeCacheMatrix above. 
#It first checks if the inverse has already been computed. If so, it gets the result and skips the computation. 
#If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
       
  # Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting the cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
