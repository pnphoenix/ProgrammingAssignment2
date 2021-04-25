## Put comments here that give an overall description of what your
## functions do

## This function is used to make the matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(solveM) i <<- solveM
  getInv <- function() i
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {

  i <- x$getInv()
  if(!is.null(i)){
    message("get inverse mat")
    return(i)
  }
  data <- x$get()
  i <- solveM(data)
  x$setInv(i)
  i      
}