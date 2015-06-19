## There are two functions in this file, the function makeCacheMatrix and the cacheSolve.
## makeCachematrix takes a matrix and "prepares" it to be used by the cacheSolve Funktion.
## cacheSolve takes the returned values of makeCachematrix (list) and returns the inverse of the matrix, given to makeCachematrix.

## makeCacheMatrix takes a matrix x and returns a list of 4 functions: set, get, setinverse and getinverse. 
## those 4 functions are able to set a new matrix x, retrieve matrix x from it's storage, set the inverse of x (if given the inverse) and store it and retrieve the inverse of x from it's storage, respectively.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a list with at least 3 functions in it: getinverse, get and setinverse. 
## Such a list is created y makeCacheMatrix.
## it checks whether the matrix, to which the function get has a pointer to has a inverse stored in cache.
## The function getinverse has a pointer to the inverse of that matrix. 
## If there is no inverse stored in cache, cacheSolve will calculate the Inverse of the matrix and use setinverse to cache it and return the inverse.
## The next time this list is called, getinverse will find the cached inverse and cacheSolve will simply return the allready calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached Inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
