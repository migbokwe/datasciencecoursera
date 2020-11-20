## This file contains two functions that used together can speed up the 
## calculation of the inverse of a matrix by caching the initial calculation
## and then providing a function to look for a cached value before recalculating
## if the values are required for subsequent operations

## This function sets the matrix, gets the matrix,
## sets the matrix inverse and gets the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function checks for a cached matrix and if a value is already available, 
## uses this value for further operations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

#Test with sample data
a <- matrix( c(5, 1, 0,
             3,-1, 2,
             4, 0,-1), nrow=3, byrow=TRUE)

b <- makeCacheMatrix(a)
cacheSolve(b)
