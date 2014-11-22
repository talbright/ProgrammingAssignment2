## This module provides inverse operations for a matrix, but
## improves performance by caching intermediary results

## This function creates a special "matrix" object that can cache its inverse,
## using the supported functions attached to the matrix object.
##
## create matrix: amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## get matrix: amatrix$get()
## set matrix: amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
## inverse of matrix: amatrix$getinverse()
makeCacheMatrix <- function(self.matrix = matrix()) {
  self.inv <- NULL
  set <- function(newmatrix) {
    self.matrix <<- newmatrix
    self.inv <<- NULL
  }
  get <- function() {
    self.matrix
  }
  getinverse <- function() {
    self.inv
  }
  setinverse <- function(inverse) {
    self.inv <<- inverse
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

## This function computes the inverse of the special "matrix" returned by 
## vmakeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve retrieves the inverse from 
## the cache.
cacheSolve <- function(cachematrx, ...) {
  inverse <- cachematrx$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- cachematrx$get()
  inverse <- solve(data, ...)
  cachematrx$setinverse(inverse)
  inverse
}

# mtrx = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# mtrx$get()
# cacheSolve(mtrx)
# mtrx$getinverse()
# cacheSolve(mtrx)
# mtrx$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
# mtrx$get()
# cacheSolve(mtrx)
# mtrx$getinverse()
# cacheSolve(mtrx)
