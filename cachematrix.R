## Matrix inversion is a costly computation and the following functions
## serve to cache the inverse of a matrix for direct retrieval rather than 
## compute it multiple times

## makeCacheMatrix creates a cached matrix, which is a list containing 
## functions to set the value of the matrix, get the value of the matrix,
## set the cached inverse, and get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## resets the cacheMatrix to store new matrix, inverse no longer cached 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## returns the matrix stored within the cacheMatrix
  get <- function() x
  
  ## resets the cached inverse
  setInv <- function (soln) inv <<- soln
  
  ## returns the cached inverse
  getInv <- function() inv
  
  ## returns the cacheMatrix
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  
}


## The following function calculates the inverse of a cacheMatrix created 
## with the makeCacheMatrix function. It first checks to see if the inverse of 
## given matrix has already been calculated and gets it from cache, otherwise it calculates
## the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  
  ## inverse of cacheMatrix 'x' has previously been calculated
  if (!is.null(inv)) {
    
    message("getting cached inverse")
    return(inv)
  }
  
  ## otherwise calculate the inverse of cacheMatrix 'x', cache it, and return
  
  cacheMat <- x$get()
  
  inv <- solve(cacheMat)
  
  x$setInv(inv)
  
  message("newly calculated and cached inverse")
  inv
  
  
  
}
