## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Method the get the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  ## get the inverse of the matrix
  getinverse <- function() inv
  
  ## list of the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## return the inverse of the matrix if its already set
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Get the matrix from the object
  data <- x$get()
  ## Calculate the inverse
  inv <- solve(data,...)
  ##set the invers to the object
  x$setinverse(inv)
  inv
}
