#  cacheMatrix.R 
## This script contains two functions for efficiently calculating the inverse of a matrix
## The first function (makeCacheMatrix) is used to create a list that contains the matrix
## The second function (cacheSolve) either calculates the inverse, or returns a cached solution


makeCacheMatrix <- function(x = matrix()) {
  ## Creates a special matrix object
  ##
  ## Args:
  ##   x: a matrix
  ##
  ## Returns:
  ##   List including:
  ##     the matrix, 
  ##     functions to set/get the matrix,
  ##     and functions to set/get the inverse of the matrix.
  ##
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Calculates the inverse of a matrix contained in a special matrix object provided by makeCacheMatrix
  ##
  ## Args:
  ##  x:   a special matrix object provided by makeCacheMatrix
  ##  ...: arguments to pass to solve()
  ## 
  ## Returns:
  ##   If the inverse has not been calculated, it will be calculated using solve(), and cached for future use and returned.
  ##   If a cached answer already exists, it is returned with the message "getting cached data".
  ##   
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
