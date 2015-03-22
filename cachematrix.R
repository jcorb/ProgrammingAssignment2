## This code returns the inverse of a matrix, either by returning a cached version 
##or by calculating the inverse if no cached version exists. 

## makeCacheMatrix returns a list of functions that are used to cache the inverse
## of input matrix x.
## usage: vec = makeCacheMatrix()
## set - vec$set(y) assigns the values of y to be x within the makeCacheMatrix() environment
## and sets the result, m,  to be NULL.
## get - vec$get() returns the values of x inside the function environment
## setinv - vec$setinv(inverse_matrix) assigns the value of m to be inverse_matrix,  
## the result value within the makeCacheMatrix() environment.
## getinv - vec$getinv() returns the cahed value of the inverse matrix, m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }  
  get <- function() x
  setinv <- function(inverse_matrix) m <<- inverse_matrix
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes the vector created by makeCacheMatrix as input.  It checks to see
## if the inverse matrix is stored and returns that if it is, othewise it calculates 
## the inverse matrix, stores it for future use and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ## check to see if m exists
  if(!is.null(m)) {
    # m does exist, return and exit cacheSolve
    message("getting cached data")
    return(m)
  }
  ## m doesn't exist
  ## get the matrix that is to be inverted
  data <- x$get()
  ## use function solve to invert the matrix
  m <- solve(data, ...)
  ## cache the inverse matrix
  x$setinv(m)
  ##return the inverse matrix
  m  
  
}
