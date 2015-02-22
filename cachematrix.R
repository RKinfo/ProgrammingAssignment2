

# this function generate a custom class that return 
# an object type list with all properties and functions 
# that's needed for make cacheable, the invertible matrix result
# @param: x = user matrix based on
# @return: list mixed object
makeCacheMatrix <- function(x = matrix()) {
  IM <- NULL
  set <- function(y) {
    x <<- y
    IM <<- NULL
  }
  get <- function() x
  setinv <- function(solve) IM <<- solve
  getinv <- function() IM
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# this function process the user matrix inversion, 
# if the matrix object, have a cached invert result, 
# the function return this cached result, 
# but if the object not has cached result, 
# a new inverted matrix is returned.
# @param:  x = user matrix based on
#        ... = super class params
# @return: inverse of the input matrix
cacheSolve <- function(x, ...) {   
  IM <- x$getinv()
  if(!is.null(IM)) {
    #message("getting cached data")
    return(IM)
  }
  data <- x$get()
  IM <- solve(data, ...)
  x$setinv(IM)
  IM
  ## Return a matrix that is the inverse of 'x'
}


### DEMO IMPLEMENTATION
# define the size of the square matrix
#// SQUARE_SIZE <- 3
# make an "ones" matrix
#// A <- matrix(1,SQUARE_SIZE,SQUARE_SIZE)
# make invertible "A" matrix
#// A <- A + diag(SQUARE_SIZE)
# make cacheable matrix
#// CA <- makeCacheMatrix(A)
# first solve intent
#// cacheSolve(CA)
# second solve intent
#// cacheSolve(CA)
