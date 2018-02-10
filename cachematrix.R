## The makeCacheMatrix function creates a matrix object that will cache the inverse in a seperate 
## environment that can be retrieved when needed to prevent unecessary calculations.
## e.g. set matrix1 <- makeCacheMatrix(matrix(1:9, 3, 3)) 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function will calculate the inverse of the matrix created previously. However,
## if the inverse has already been calculated, without a change in said matrix, it will retrive the 
## inverse directly from the cache.
## e.g. cacheSolve(matrix1)

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}