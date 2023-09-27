# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix and a flag to indicate if it's cached
  inv <- NULL
  cached <- FALSE
  
  # Setter function to set the matrix and reset the cache flag
  setMatrix <- function(newValue) {
    x <<- newValue
    inv <<- NULL
    cached <<- FALSE
  }
  
  # Getter function to retrieve the matrix
  getMatrix <- function() x
  
  # Getter function to retrieve the cached inverse if available
  getInverse <- function() inv
  
  # Setter function to cache the inverse
  setInverse <- function(inverse) {
    inv <<- inverse
    cached <<- TRUE
  }
  
  # Return a list of functions to access and manipulate the matrix and its inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse, setInverse = setInverse, cached = cached)
}

# Compute and cache the inverse of the matrix if not already cached
cacheSolve <- function(cacheMatrix) {
  # Check if the inverse is already cached
  if (cacheMatrix$cached) {
    cat("Getting cached data\n")
    return(cacheMatrix$getInverse())
  }
  
  # If not cached, calculate the inverse and cache it
  mat <- cacheMatrix$getMatrix()
  inv <- solve(mat)
  cacheMatrix$setInverse(inv)
  
  cat("Calculating and caching inverse\n")
  return(inv)
}
