## This R document is used to cache and compute the inverse of a matrix

##  makeCacheMatrix is used to create a matrix object which could cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y){
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setinverse <-  function(inv) i <<- inv
  getinverse <-function() return(i)
  return(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is used to computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message ("Getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ... )
  x$setinverse(i)
  return(i)
}
