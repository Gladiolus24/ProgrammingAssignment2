## #makeCacheMatrix : creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL                                        #initialize inv variable to NULL
  
  set <- function(y){                                #Define the 'set' function to store a new matrix
    x <<- y                                          #Use <<- to assign new matrix to parent environment variable x
    inv <<- NULL                                     #Reset cached inverse to NULL since matrix has changed
  }
  
  get <- function() x                                #Define 'get' function that returns the stored matrix x
  setInverse <- function(inverse) inv <<- inverse    #Define function to cache the computed inverse using <<-
  getInverse <- function() inv                       #Define function to retrieve the cached inverse
  
  list(set = set,                                    #Return a list containing all four functions
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {                    # Function takes special matrix object and additional arguments
  inv <- x$getInverse()                             # Call getInverse() method to check for cached inverse
  
  if (!is.null(inv)) {                              # Check if cached inverse exists (not NULL)
    message("getting cached data")                  # Print message indicating cache hit
    return(inv)                                     # Return cached inverse and exit function early
  }
  
  data <- x$get()                                   # No cached inverse found, so get the original matrix
  inv <- solve(data, ...)                           # Compute matrix inverse using solve() function
  x$setInverse(inv)                                 # Cache the computed inverse for future use
  inv                                               # Return the computed inverse
}
