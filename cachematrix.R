## makeCacheMatrix creates an object with fuctions to get and
## set the matrix and the inverse of the matrix

## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(matrix = matrix()) {
  i <- NULL
  set <- function(y) {
    matrix <<- y
    i <<- NULL
    
  }
  get <-function() matrix
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## computes the inverse of the special matrix returned by makeCachMatrix. If 
#   inverse has already been calculated (and matrix has nto changed) then 
#   caacheSolve retrieves inverse from cache.  

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
       i <- x$getinverse()
       if(!is.null(i)) {
         message("Getting cached data")
         return(i)
       }
       data <- x$get()
       i <- solve(data, ...)
       x$setinverse(i)
       i
}



