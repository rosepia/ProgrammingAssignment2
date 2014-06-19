## functions should calculate inverse of a matrix using cache if possible
## code is inspired by the example given in README.md

## function "makeCacheMatrix" creates a matrix which is a list of functions
## for setting and getting the matrix which should be inversed and
## also setting/getting the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the function "cacheSolve calculates the inverse of the matrix using solve()
## before calculating it checks if the inverse was already calculated
## if the inverse was calculated before it gets the inverse from the cache
## if the inverse was not calculated before it uses solve() and sets the
## inverse in the cache using the setinv function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
