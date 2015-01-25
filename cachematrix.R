## Programming Assignment 2
## This programming routine utilize cache in order to reduce computation time in the case of the input
## matrix not changing.  The scoping rules of the R language provides this advantage.


## makeCacheMatrix takes in a matrix and sets and gets the value of the matrix and also sets and gets
## the inverse matrix

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

## cacheSolve returns a matrix that is the inverse of 'x', however it first checks to see 
# if the inverse matrix has already been computed.  If yes, then it gets the inverse
# matrix from the cache.  If no, it solves the inverse matrix and sets the inverse matrix
# in the cache via "setsolve" function

cacheSolve <- function(x, ...) {
  
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