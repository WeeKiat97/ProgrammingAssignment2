## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # define inv as NULL which will then become the value which contains the inverse matrix
  set <- function(y){  # set is a function of y which will return updated value of matrix
    x <<- y
    inv <<- NULL
  }
 
   get <- function() x # get is a function which returns the argument of matrix
  setInverse <- function(inverse) inv <<- inverse # setInverse is a function which returns value of inv in parent environment
  getInverse <- function() inv #getInverse is a function which returns the value of inv where it is called
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

#if an inverse exist, the function cacheSolve will return the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)){                    
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()    
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
