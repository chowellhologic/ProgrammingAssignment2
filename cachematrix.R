
## Takes a matrix as an argument. Returns a list with 4 elements: set, get, 
##setinverse, and getinverse. These elements are all functions within the 
##environment of the parent function makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {##declare a function with 1 argument
  inv <- NULL ##initiate the inverse object
  set <- function(y) {
    x <<- y ##reassign x with new argument
    inv <<- NULL ##reassign inv
  }
  get <- function() x ##return the input matrix
  setinverse <- function(solve) inv <<- solve ##calculate the inverse
  getinverse <- function() inv ##return inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ##return the list of 4 functions
}


## Takes a cacheMatrix list as an argument. Returns the cached inverse
## if available. Otherwise, solves and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) { ##checks if inv object is null
    message("getting cached data") ##reports that cached data is available
    return(inv)
  }
  data <- x$get() ##gets the original matrix object
  inv <- solve(data, ...) ##calculates the inverse
  x$setinverse(inv) ##calls the setinverse function to write the solved inverse
      ## to the cached matrix
  inv ##returns the inverse
}
