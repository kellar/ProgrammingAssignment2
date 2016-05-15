## cachematrix.R provides a matrix inverse cache.
##
## Example usage:
## > theMatrix <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
## > cacheSolve(theMatrix)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(theMatrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Defines a cacheAugmentedMatrix object which contains a matrix and its
## inverse. Setter and getter functions are returned in a list.
makeCacheMatrix <- function(basicRMatrix = matrix()) {
  inverse <- NULL
  setMatrix <- function(newBasicRMatrix) {
    basicRMatrix <<- newBasicRMatrix
    inverse <<- NULL
  }
  getMatrix <- function() basicRMatrix
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function returns the inverse of a cacheAugmentedMatrix object
## (see makeCacheMatrix). If the inverse of this cacheAugmentedMatrix has
## already been computed it returns that cached inverse, else it computes,
## caches, and returns the inverse.
cacheSolve <- function(cacheAugmentedMatrix, ...) {
  ## Return a matrix that is the inverse of 'cacheAugmentedMatrix'
  inverse <- cacheAugmentedMatrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- cacheAugmentedMatrix$getMatrix()
  inverse <- solve(data, ...)
  cacheAugmentedMatrix$setInverse(inverse)
  inverse
}

