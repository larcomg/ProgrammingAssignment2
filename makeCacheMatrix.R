#************************************************************************
# File:   makeCacheMatrix
# Author: Guy Larcom
# Date:   20140724
#
# This function creates a special "matrix" object that can cache its 
# inverse.
#
# Takes an argument for the dimensions of the square matrix
#
# test <- diag(n)
# testInverse <- solve(test)
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
