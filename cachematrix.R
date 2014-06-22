## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a "special" matrix that is in fact a list of 4 functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialization of the inverse matrix
  inv <- NULL
  
  # function to set the value of the matrix x
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # function to get the value of the matrix x
  get <- function() x
  
  # function to set the value of the inverse matrix inv
  setinv <- function(solve) inv <<- solve
  
  # function to get the value of the inverse matrix inv
  getinv <- function() inv
  
  # creation of the list of the function that makes the cache inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## cacheSolve either returns the value of the inverse matrix of the "special"
# matrix returned by makeCacheMatrix if it is in the cache, or computes the 
# inverse matrix and stores it in the "special" matrix if it hasn't already been
# calculated.
cacheSolve <- function(x, ...) {
  # gets the inverse matrix cached in x
  inv <- x$getinv()
  
  # if the inverse matrix has already been, just return it
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # if the inverse matrix has not been calculated yet, compute it, store it and return it
  message("computing inverse matrix")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
