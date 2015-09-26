
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(y){
    m <<- cacheSolve(y)
  } 
  getcachesolve <- function() m
  list(set = set, get = get,
       cacheSolve = cacheSolve,
       getinverse = getinverse)
}


## Write a short comment describing this function

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