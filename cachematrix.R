## Put comments here that give an overall description of what your
## functions do:

# General function to calculate the inverse of a matrix, either directly
# or, if this result has previously been calculated, then it retrieves
# this from a cache, and hence avoids re-calculating.

## Write a short comment describing this function

# Function to create a matrix-like object that cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(solve) I <<- solve
  getinv <- function() I
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
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setmean(I)
  I
}
