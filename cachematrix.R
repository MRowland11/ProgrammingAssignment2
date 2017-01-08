## The following are two functions makeCacheMatrix and cacheSolve
## which  work together to calculate the inverse of a matrix using 
## the Solve function
##
## Before trying to calculate the inverse of a matrix a check is
## undertaken to see if the calculation has already been made and 
## stored in cache. If it has it is retrieved from Cache and no 
## calcuation is made
##
## Note: the code assumes that the Matrix supplied is always invertible



## The function makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) m <<- invmat
  getinvmat <- function() m
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


##
## The following function calculates (using the solve function) the inverse
## of the special "Matrix" created in the previous function ((makeCacheMatrix)
##
## Before doing the calcualation it checks to see if the inverse Matrix has 
## already been calculated and stored in cache (i.e. checks if 'm' is not null). 
##
## If it has alrady been calculated, it get`s the inverse Matrix from the cache
## and skips the computation.
##
## Else it calculates the inverse Matrix of the data and sets the value of the
## data in the cache via the `setinv` function.


cacheSolve <- function(x, ...) {
  m <- x$getinvmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmat(m)
  m
}

