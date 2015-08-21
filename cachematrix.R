## Below are two functions that create a special object that 
## stores a matrix and caches its inverse (so it is not necessary to compute it repeatedly)

## this function creates a matrix that can cache its inverse
## it makes a list containing functions for:
## setting the matrix, getting the matrix, setting the inverse and
## getting the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    ## <<- assigns the value to x or inv (in different environment)
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get =get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function makes the inverse of the matrix above (makeCacheMatrix)
## once the inverse was calculated before and the matrix was not changed,
## it uses the cached inverse.

cacheSolve <- function(x, ...) {
    
  inv <- x$getInverse()
  
  ## if the inverse was calculated before: 
  if (!is.null(inv)) {
    
    ## it is getting it from the cache
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise inverse is calculated:
  data <- x$get()
  inv <- solve(data, ...)
  
  ## sets the value of the calculated inverse in the cache using
  ## the setInverse function
  x$setInverse(inv)
  inv
}
