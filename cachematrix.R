# Function to store and resolve cached inverse of a matrix
makeCacheMatrix <- function(m = matrix()) {
  # set the inverse to null as initial value
  mInverse <- NULL
  
  # function to set the matrix. Use this to change the matrix.
  set <- function(y) {
    m <<- y
    # Clear the cached inverse when the matrix is changed
    mInverse <<- NULL
  }
  
  # function to set the inverse of a matrix
  setinverse <- function(solve) {
    mInverse <<- solve
  }
  
  # function to get the matrix 
  get <- function(){
    m
  }

  # function to get the inverse of the matrix
  getinverse <- function() {
    mInverse
  }

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function peeks at cache to see if matrix's inverse if it hasn't been cached previously. If yes, then cached value is returned. If no, value is calculated and returned
cacheSolve <- function(m, ...) {
  #check cache if the inverse exists for this matrix
  mInverse <- m$getinverse()
  if(!is.null(mInverse)) {
    #inverse exists.  Use the cached value
    message("getting cached inverse of matrix")
    return(mInverse)
  }
  #inverse does not exist.  Calculate the value and store in cache for future.
  data <- m$get()
  mInverse <- solve(data, ...)
  m$setinverse(mInverse)
  #Return the inverse
  mInverse
}

        
# Test Code
#> myMatrix <- makeCacheMatrix(matrix(c(1, 4, 4, 8, 4, 8, 3, 3, 4),3))
#> cacheSolve(myMatrix)
#     [,1] [,2]   [,3]
# [1,] -1.0   -1  1.500
# [2,] -0.5   -1  1.125
# [3,]  2.0    3 -3.500
#> cacheSolve(myMatrix)
#getting cached inverse of matrix
#     [,1] [,2]   [,3]
# [1,] -1.0   -1  1.500
# [2,] -0.5   -1  1.125
# [3,]  2.0    3 -3.500
