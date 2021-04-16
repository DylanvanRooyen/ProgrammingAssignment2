#The two functions below are used to create a
#object that stores a matrix and caches its inverse.

##The first function, `makeCacheMatrix` creates a "matrix", which is
##really a list containing a function to
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ai <- NULL
  set <- function(y) {
    x <<- y
    ai <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ai <<- inverse
  getinverse <- function() ai
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


##The following function calculates the inverse of the "matrix"
##created with the above function. It first checks to see if the
##Inverse is already in the cache. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the matrix and sets the value of the inverse in the cache via the `setinverse`
##function.

cacheSolve <- function(x, ...) {
  ai <- x$getinverse()
  if(!is.null(ai)) {
    message("getting cached data")
    return(ai)
  }
  matrix <- x$get()
  ai <- solve(matrix, ...)
  x$setinverse(ai)
  ai
}
