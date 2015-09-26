
## These two functions design a special Matrix which is 
## able to cache its inverse, once it has been firtly calculated.


## makeCacheMatrix creates the special matrix. This new 
# matrix has 4 methods. 
# Get method: In order to get the matrix data stored
# Set method: In order to change the matrix data stored
# getcachesolve method: It retrieves the inverse stored. If
# is empty, it returns a null. 
# setcachemethod: it stores the data corresponding to the 
# inverse. 
# Example: t <- makeCacheMatrix(rbind(c(1,-0.5),c(-0.5,1)))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcachesolve<- function(cachesolve) m <<- cachesolve
  getcachesolve <- function() m
  list(set = set, get = get,
       setcachesolve = setcachesolve,
       getcachesolve = getcachesolve)
}


## cacheSolve is a function which calculates the inverse
# of a matrix through the solve method. 
# The input parameter is an special matrix created through
# the makeCacheMatrix method. 
# If the Special matrix has already stored its inverse, 
# the method returns the cached inverse. 
# t <- makeCacheMatrix(rbind(c(1,-0.5),c(-0.5,1)))
# Example: cacheSolve(t)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getcachesolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get();
  if ((dim(data)[1] != dim(data)[2]) || det(data) == 0){
    message('X is not a Squared matrix or its determinant is zero');
    return(m)
  }
  m <- solve(data, ... )
  x$setcachesolve(m)
  m
}