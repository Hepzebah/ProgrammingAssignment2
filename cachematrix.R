## This file contains functions that handle matrix inversion and retrieving a 
##cached value of the inverse

## this function defines a class and methods for setting and retrieving a matrix 
##and calculating the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #allows the user to modify the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #retrieve the matrix
  setinv <- function(solve) m <<- solve #solve the matrix to find the inverse
  getinv <- function() m #returns m which will be either null or the inverse of the matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## this function checks to see whether the inverse of the matrix has already 
##been cached; if it has, then it retrieves the cached value and exits by 
##returning that cached value; if it hasn't, then it calculates the inverse
##and passes it back to the "setinv" function to cache the value in the 
##environment of the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  #  print('value does not yet exist') #inserted for debugging purposes
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
