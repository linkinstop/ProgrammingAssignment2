## Put comments here that give an overall description of what your
## functions do
## These two functions try to cash the inverse matrix so it need not to calculate again
## if the inverse matrix is needed again.

## Write a short comment describing this function
## This function create a matrix with some function to allow it cashes its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes x as input and output its cashed inverse matrix if exists or output 
## the calculated inverse matrix value if not exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cashed data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
