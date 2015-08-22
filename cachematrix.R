## There are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## The first function, makeVector creates a special "vector", which is really a list containing a function to
# 1)set the value of the matrix
# 2)get the value of the matrix
# 3)set the value of the inverse
# 4)get the value of the inverse

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

## The following function calculates the INVERSE of the special "matrix" created with the above function.
## it first checks to see if the reverse has already been calculated. 

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
