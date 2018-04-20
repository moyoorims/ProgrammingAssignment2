## a pair of functions that cache the inverse of a matrix

##  function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) invert <<- solveMatrix
  getInverse <- function() invert
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## computes or retrieve the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getInverse()
  if(!is.null(invert)){
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data)
  x$setInverse(invert)
  invert      
}





