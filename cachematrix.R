####################################################################
##                                                                ##
## makeCacheMatrix: This function creates a special "matrix"      ##
## object that can cache its inverse.                             ##
##                                                                ##
## cacheSolve: This function computes the inverse of the special  ##
## "matrix" returned by makeCacheMatrix above. If the inverse has ##
## already been calculated (and the matrix has not changed), then ##
## the cachesolve should retrieve the inverse from the cache.     ##
##                                                                ##
###################################################################

makeCacheMatrix <- function(x = matrix()) {

    I <- NULL
    set <- function(y) {
      x <<- y
      I <<- NULL
    }
    
    get <- function() x
    setInv <- function(solve) I <<- solve
    getInv <- function() I
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}
  
  

cacheSolve <- function(x, ...) {

    I <- x$getInv()
    
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInv(I)
    I
}
  
