## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setmatrixinverse <- function(matrixinverse) mi <<- matrixinverse
  getmatrixinverse <- function() mi
  list(set = set, get = get, 
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}
 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    mi <- x$getmatrixinverse()
    if(!is.null(mi)) {
      message("getting cached data")
      return(mi)
    }
    data <- x$get()
    mi <- solve(data)
    x$setmatrixinverse(mi)
    mi
        ## Return a matrix that is the inverse of 'x'
}
