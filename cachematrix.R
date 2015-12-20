# Assignment - The implementation of two functions:
#
# 1. MakeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
# 2. CacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
#    retrieve the inverse from the cache.

# To increase the speed of execution of the program, it is necessary to use a caching inverse matrix to avoid
# re-computation.

makeCacheMatrix = function(x = matrix()) {
  matrixInverse = NULL
  
  # set the value of the Matrix
  set = function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  
  # get the value of the Matrix
  get = function() x
  
  # set the value of the Inverse
  setInverse = function(inverse) matrixInverse <<- inverse
  
  # get the value of the Inverse
  getInverse = function() matrixInverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve = function(x, ...) {
  matrixInverse = x$getInverse()
  
  # Return a matrix that is the inverse
  if (!is.null(matrixInverse)) {
    message("Getting cached data")
    return(matrixInverse)
  }
  
  mat = x$get()
  matrixInverse = solve(mat, ...)
  x$setInverse(matrixInverse)
  matrixInverse
}



