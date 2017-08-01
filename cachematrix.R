## This two functions calculate the inverse o a invertible matrix using cache memory
## to avoid innecesary calculations

## makeCacheMatrix creates a list with function that allows save the matrix on cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve uses solve to find the inverse of the matrix a buit checking the cache to avoid
## calculate the inverse if done before

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
