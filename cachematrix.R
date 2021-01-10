##Two functions that cache the inverse of a matrix
#Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  
  get <- function() {
    m
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}

#Computes the inverse of the matrix returned
#by makeCacheMatrix
cacheSolve <- function(m, ...) {
  i <- m$getInverse()
  
  if (!is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  
  i <- solve(data, ...)
  m$setInverse(i)
  
  i
}


