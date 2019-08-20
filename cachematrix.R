## Put comments here that give an overall description of what your
## functions do:

# General function to calculate the inverse of a matrix, either directly
# or, if this result has previously been calculated, then it retrieves
# this from a cache, and hence avoids re-calculating.

## Write a short comment describing this function

# Function to create a matrix-like object that cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinv <- function(solve_me) M <<- solve_me
  getinv <- function() M
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# function to compute the inverse of a matrix, but retrieves it from
# the cache if this has already been done.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  M <- x$getinv()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setinv(M)
  M
}

