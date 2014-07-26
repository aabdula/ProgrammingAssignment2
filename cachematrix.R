## makeCacheMatrix creates a special matrix object, 
##and inv_x does not create int inverse of that matrix function then we set x = y
## get function takes and uses set inverse to sets the inverse of the function
## get inverse calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead
## find it in the cache and return it, and not calculate 

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}



## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function by the get function and set function commands
## If the cached inverse is available, cacheSolve retrieves it,if not the else
## then it computes the inverse, caches, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } 
  else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
