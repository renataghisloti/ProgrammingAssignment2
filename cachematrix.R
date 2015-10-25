
## This function creates a list containing a matrix, and may set the inverse
## of this matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setmean <- function(inv) x_inv <<- inv
  getmean <- function() x_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This functions looks for a cached value of the inverse of a matrix. If
## it doesnt find it, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(x)
  x$setinv(x_inv)
  x_inv
}
