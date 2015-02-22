## These functions take a matrix, calculates it's inverse and stores the inverse 
##and associated matrix for later use. 


## This function creates a set of 4 functions to be used on an input matirx

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function checks if the inverse has already been computed
#and returns the inverse. If not, it calculates the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()   ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
