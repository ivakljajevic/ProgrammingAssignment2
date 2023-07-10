## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
    
  }
  get <- function() x
  setinv <- function(inverse) n <<- inverse
  getinv <- function() n
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function computes the inverse of the 
## "matrix" created by makeCacheMatrix above. 
## If the inverse was already calculated, then it 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getinv()
  if(!is.null(n)) {
    message("getting cached inverse matrix")
    return(n)
  } else{
    data <- x$get()
    n <- solve(data, ...)
    x$setinv(n)
    return(n)
}
