#  Catching the Inverse of the Matrix

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to set and get the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse) {
    m <<- solve
  }
  getinverse <- function(){ 
    m
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  ##Return inverse if it's already set
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##Get the matrix
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  
  ##Return the matrix
  m
}
