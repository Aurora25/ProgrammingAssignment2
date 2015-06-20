## Function Framework originates from Roger D. Peng. 
## Original Functions can be seen in the README file of the following gitHub repository: https://github.com/rdpeng/ProgrammingAssignment2
## There are two functions in this file, the function makeCacheMatrix and the cacheSolve.
## makeCachematrix takes a matrix and "prepares" it to be used by the cacheSolve Funktion.
## cacheSolve takes the returned values of makeCachematrix (list) and returns the inverse of the matrix, 
## which was originaly given to makeCachematrix. It also caches the inverse for later use.


## makeCacheMatrix takes a matrix x and returns a list of 4 functions: set, get, setinverse and getinverse. 
## those 4 functions are able to set a new matrix x, retrieve matrix x from it's storage, set the inverse of x (if given the inverse) and store it and retrieve the inverse of x from it's storage, respectively.

makeCacheMatrix <- function(x = matrix()) {
  #as for now the to-compute inverse m is set to NULL
  m <- NULL
  
  #define function "set"
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #define function "get"
  get <- function() x
  
  #define function "setinverse"
  setinverse <- function(inverse) m <<- inverse
  
  #define function "getinverse"
  getinverse <- function() m
  
  #return list with the 4 functions in it
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a list with at least 3 functions in it: getinverse, get and setinverse. 
## Such a list is created by "makeCacheMatrix".
## CacheSolve checks, whether there exists a inverse of Matrix x (which was given to "makeCacheMatrix"), 
# if not, it will calculate the inverse, cache it and eventually return it.

cacheSolve <- function(x, ...) {
  
  # use "getinverse" from the given list to retrieve the cached inverse.
  m <- x$getinverse()
  
  # check, whether an actual inverse has been cached yet. 
  if(!is.null(m)) {
    #if an actual inverse is present (m is NOT NULL): return the inverse
    message("getting cached Inverse")
    return(m)
  }
  
  #if no inverse is present, get the matrix from the cache
  data <- x$get()
  #calculate the inverse
  m <- solve(data,...)
  #store the inverse in the cache where "getinverse" can find it next time
  x$setinverse(m)
  #return the inverse
  m
}
