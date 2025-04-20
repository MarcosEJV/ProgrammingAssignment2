## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                   # i is set to NULL, initializing it as an object
    set <- function(y) {        # Defines the set() function
      x <<- y                   # Assigns the value on the right side of the operator to an object - 
                                # -in the parent environment named by the object on the left side of the operator
      i <<- NULL
  }
  get <- function() x           # Getter for the matrix x
  setinverse <- function(inverse) i <<- inverse    # Defines the setter for the inverse i
  getinverse <- function() i    # Defines the getter for the inverse i
  list(set = set, get = get,    # Gives the name 'set'/'get' to the set()/get() function defined above
       setinverse = setinverse, # Gives the name 'setinverse' to the setinverse() function defined above
       getinverse = getinverse) # Gives the name 'getinverse' to the getinverse() function defined above

  }

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {  # The argument for this function is the makeCacheMatrix() function
  i <- x$getinverse()             # Calls the getinverse() function on the input object
  if(!is.null(i)) {               # Checks if i is NULL to know if it should be calculated again
    message("getting cached data")
    return(i)
  }
  data <- x$get()        # If i is NULL, cacheSolve gets the matrix from the input object, 
  i <- solve(data, ...)  # Calculates the inverse using solve(),
  x$setinverse(i)        # Returns the value of the inverse to the parent environment,
  i                      # and prints the inverse result
}
