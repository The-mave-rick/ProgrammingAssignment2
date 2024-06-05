
# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize a cache variable to store the cached inverse
  cache <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    # Set the matrix
    x <<- matrix
    
    # Reset the cache since the matrix has changed
    cache <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    # Return the matrix
    x
  }
  
  # Function to cache the inverse of the matrix
  cache_inverse <- function(inverse) {
    # Cache the inverse
    cache <<- inverse
  }
  
  # Function to get the cached inverse
  get_cached_inverse <- function() {
    # Return the cached inverse
    cache
  }
  
  # Return a list of functions
  list(set = set,
       get = get,
       cache_inverse = cache_inverse,
       get_cached_inverse = get_cached_inverse)
}

# Function to compute the inverse of a matrix, using caching if available
cacheSolve <- function(x, ...) {
  # Get the cached inverse if available
  cached_inverse <- x$get_cached_inverse()
  
  # If the cached inverse exists, return it
  if (!is.null(cached_inverse)) {
    message("Getting cached inverse...")
    return(cached_inverse)
  }
  
  # Otherwise, compute the inverse
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  
  # Cache the computed inverse
  x$cache_inverse(inverse)
  
  # Return the computed inverse
  inverse
}

# Test case
# Create a cacheable matrix
mat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute the inverse using cacheSolve
cacheSolve(mat)
