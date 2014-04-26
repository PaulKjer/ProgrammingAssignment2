## Create the invserse of a matrix through two functions:
## - makeCacheMatrix - takes a matrix as an argument and returns a list of four functions 
## - solveCache - returns the inverse of a matrix and uses the functions in makeCacheMatrix to 
##   check for a cached version and assign a cached version if none existed. 

## makeCacheMatrix:  return a list of four functions: set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverted <- NULL              #  current context inverted = NULL

  set <- function(y) {          
    x <<- y                     #  Cached x = y
    inverted <<- NULL           #  Cached inverted = NULL
  }
  
  get <- function() x         
  
  setinverse <- function(solve) inverted <<- solve       #  assigned cached inverted
  getinverse <- function() inverted
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve:  return the inverse of 'x', first checking to see if there is a cached version
## of the result.  If there is a cached version return it, if there is not call a function to 
## invert it. 

cacheSolve <- function(x, ...) {
  
  inverted <- x$getinverse()   #  return the cached version, if we have it
  
  if(!is.null(inverted)) {     #  if we have it, inverted will not be null. 
   
    return(inverted)           #  return the cached version - we are done.
      
  }
  
  ## Since we are still in this function (we would have exited on 'return' above if there was a cached matrix),
  ## we need to create the matrix inversion.
  
  data <- x$get()                #  get the data
  inverted <- solve(data, ...)   #  create the inverted matrix
  x$setinverse(inverted)         #  cache the result
  inverted                       #  return the inverted matrix
  
}
