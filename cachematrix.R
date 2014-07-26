#****************************************************************
# File:   cacheSolve
# Author: Guy Larcom
# Date:   20140724
#
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. If the inverse has already 
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

#************************************************************************
# File:   makeCacheMatrix
# Author: Guy Larcom
# Date:   20140724
#
# This function creates a special "matrix" object that can cache its 
# inverse.
#
# Takes an argument for the dimensions of the square matrix and then
# creates the matrix.
# 
#************************************************************************
makeCacheMatrix <- function(n=numeric()){
  m <- NULL
  
  # Create Matrix
  setMatrix <- function(y){
    n <<- diag(y)
    m <<- NULL
  }
  
  # Get Matrix
  getMatrix <- function() n
  
  # Set Inverse
  setInverse <- function(solve) m <<- solve
  
  # Get Inverse
  getInverse <- function() m
  
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


#
# ORIGINAL TEMPLATE PROVIDED WITH FORKED SOURCE --->
#
# ## Put comments here that give an overall description of what your
# ## functions do
# 
# ## Write a short comment describing this function
# 
# makeCacheMatrix <- function(x = matrix()) {
# 
# }
# 
# 
# ## Write a short comment describing this function
# 
# cacheSolve <- function(x, ...) {
#         ## Return a matrix that is the inverse of 'x'
# }