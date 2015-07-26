#############################
# cachematrix.R
#############################
# usage:
# 1.) source the script cachematrix.R
# 2.) assign a matrix - e.g.:
#     a$setMatrix( matrix(c(1,2,3,-3,2,5,2,-4,-2), nrow = 3, ncol = 3) );
# 3.) view the matrix: a$getMatrix()
# [,1] [,2] [,3]
# [1,]    1   -3    2
# [2,]    2    2   -4
# [3,]    3    5   -2
# 4.) call cacheSolve(a):
# [,1]        [,2]      [,3]
# [1,]  0.33333333  0.08333333 0.1666667
# [2,] -0.16666667 -0.16666667 0.1666667
# [3,]  0.08333333 -0.29166667 0.1666667
# 5.) call cacheSolve(a) a second time to retrieve the cached value

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a matrix and a cached value of the 
# i nverse of the matrix. 
# Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cached value of the inverse of the matrix
# * getInverse     get the cached value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize cache
  cache <- NULL
  # store a matrix
  setMatrix <- function(newVal) {
    x <<- newVal
    # flush the cache as x is assigned a new value newVal
    cache <<- NULL
  }
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # set / cache the given argument
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the value cached
  getInverse <- function() {
    cache
  }
  # return a list of functions
  list(setMatrix = setMatrix, getMatrix=getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
  
}


# cacheSolve caalculates the inverse of a square matrix
# or returns the cached value if already calculated and cached 
cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("get the cached matrix")
  } # otherwise get the matrix, calculate the inverse and store it in the cache
  else {
    data <- x$getMatrix()
    inv <- solve(data)
    x$cacheInverse(inv)
  }
  return(inv)
}
