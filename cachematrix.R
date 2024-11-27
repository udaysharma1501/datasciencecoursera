## The functions below implement caching for the inverse of a matrix. 
## This avoids redundant computations by storing the inverse once calculated 
## and returning it directly if the matrix has not changed.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse property
  inv <- NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y       # Assign new matrix value
    inv <<- NULL  # Reset the cached inverse
  }
  
  # Get the value of the matrix
  get <- function() {
    x
  }
  
  # Set the value of the inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the value of the inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
