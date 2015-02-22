## Coursera R Course Feb 22, 2015
## Assignment 2 - Lexical Scoping
## Student: R. Rodriguez
##
## This function defines the base cached matrix
makeCacheMatrix <- function(x = matrix()) {
  elc <- NULL
  set <- function(y) {
    x <<- y
    elc <<- NULL
  }
  get <- function() x
  setsolve<- function(solve) elc <<- solve
  getsolve <- function() elc
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  }

## This function solves for the inverse of the matrix 
cacheSolve <- function(x, ...) {
  elc <- x$getsolve()
  if(!is.null(elc)) {
    message("getting cached inverse matrix")
    return(elc)
  }
  matrix <- x$get()
  elc <- solve(matrix, ...)
  x$setsolve(elc)
  elc
}
