makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse property
  inv <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is changed
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the above methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if available
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse using the solve function
  x$setInverse(inv)  # Cache the computed inverse
  
  # Return the computed inverse
  inv
}
