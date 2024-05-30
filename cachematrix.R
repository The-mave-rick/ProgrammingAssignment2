
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  
  set <- function(matrix) {
    mat <<- matrix
    inverse <<- NULL
  }
  
  get <- function() {
    mat
  }
  
  cacheInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(set = set,
       get = get,
       cacheInverse = cacheInverse,
       getInverse = getInverse)
}

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getInverse()
  
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  mat <- cacheMatrix$get()
  inverse <- solve(mat, ...)
  cacheMatrix$cacheInverse(inverse)
  inverse
}

