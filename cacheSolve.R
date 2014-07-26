#****************************************************************
# File:   cacheSolve
# Author: Guy Larcom
# Date:   20140724
#
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.
#
#****************************************************************
cacheSolve <- function(x, ...){
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data)
  x$setMatrix(m)
  m
}