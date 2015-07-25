# The following two functions efficiently calculate the inverse of a matrix. If it was already calculated 
# before, then it will recache the inverse instead of repeating the calculation.

# The first function, makeCacheMatrix creates a list containing functions to 
# 1)set the value of matrix 2)get the value of matrix 3) set the inverse of the matrix
# 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the inverse and recall it
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The following function calculates the inverse of the matrix like the above function. 
# However, it first checks to see if the inverse has already been calculated and recalls it 
# if this is the case. Otherwise, it calculates the inverse (using set inverse from the above function).

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  # check if the inverse was already calculated and recall it if yes.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # calculates it with getting and setting the inverse using the subfunctions from makeCacheMatrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
