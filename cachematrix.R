## Functions govern the generation and operation of a special matrix object
## that can cashe its inverse

## Outputs a special matrix in the form of a list object 
## that houses the matrix and functions necessary to cache a copy of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) i <<- inverse
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Outputs the inverse of the special matrix option.  If an inverse has already been
## calculated, the cached copy will be returned.  Else, the inverse is calculated

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
