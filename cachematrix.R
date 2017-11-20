## This function creates a matrix that is cached R

makeCacheMatrix <- function(x = matrix()) {
  matri <- NULL
  set <- function(y) {
    x <<- y
    matri <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) matri <<- solve
  getsolve <- function() matri
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  ## return: a list containing functions to
  ##  get and set the matrix
  ##  get and set the inverse
  ## this list is used as the input to cacheSolve()
}


## This function takes the inverse of a cachedMatrix or
## calls that inverse if the result has already been solved and cached

cacheSolve <- function(x, ...){
  ## x is the output of makeCacheMatrix()
  matri <- x$getsolve()
  if(!is.null(matri)){
    message("getting cached data")
    return(matri)
    ## returns inverse of the original matrix input if it has already been run through cacheSolve
  }
  matrix.data <- x$get()
  matri <- solve(matrix.data, ...)
  # sets the value of the inverse in the cache
  x$setsolve(matri)
  matri
}