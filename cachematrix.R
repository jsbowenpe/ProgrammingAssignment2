## Put comments here that give an overall description of what your
## functions do
library(matlib)
## Write a short comment describing this function
### makeCacheMatrix() stashes a matrix along with some mutator
### and accessor methods for it.  It also caches the inverse of 
### the matrix once it is calculated by the cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) m <<- inverse
  get.inverse <- function() m
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}



## Write a short comment describing this function
### This function retrieves the inverse of the matrix x
### if it has been cached, or else calculate the inverse
### and caches it.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$set.inverse(m)
  m
}  
