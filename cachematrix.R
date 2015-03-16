## The functions makeCacheMatrix and cacheSolve below create an
## object that stores a matrix and its inverse.  The matrix, x,
## is assumed to be square and invertible.



## makeCacheMatrix returns a list of functions that can
## (a) set the value of the matrix
## (b) get the value of the matrix
## (c) set the value of the inverse of the matrix
## (d) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <-- y
    m <-- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv) m <-- inv
  
  getinv <- function() m
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve calculates the inverse of the matrix created with the
## function makeCacheMatrix above.  First, it uses the getinv function
## to return the inverse (if any).  Second, it uses the is.null operator
## to check if an inverse has already been cached; if the cached inverse
## exists, it is returned and the function exits.  If no inverse
## has been cached, the cacheSolve function continues and uses the
## R solve function to calculate the matrix inverse, and the setinv
## function to cache it.  Lastly cacheSolve returns the value
## of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  
  if (!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(x, ...)
  
  x$setinv(m)
  
  m
}
