## These functions are able to cache the inverse of a given matrix, to avoid 
##repeating potentially time-consuming computations.

## The following function (makeCacheMatrix) creates a "matrix" object 
##that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


##The following function (cacheSolve) computes the inverse of the "matrix" 
##returned by the function above. First it checks If the inverse has already 
##been calculated and if the matrix has not changed, then the cacheSolve 
##function retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
