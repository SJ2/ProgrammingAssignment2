## The two functions in this file helps create a matrix object in the cache 
## so that an inverse of a matrix can be calculated and stored in this cache object
## This would avoid a costly inverse of a matrix calculation.
## The inverse matrix from the cache is simply returned without re-calculation 
## whenever needed (Assumption: Matrix is inverseable - no additional check done )

## FIRST FUNCTION makeCacheMatrix - Creates a special matrix object that can 
## cache its inverse. It does it by creating a list of 4 functions to get and set
## the matrix and also to set inverse, and get inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  ## Four functions defined below - set, get, setinvmarix, getinvmatrix
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(matrix) m <<- matrix
  getinvmatrix <- function() m
  

  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)

  
}

## SECOND FUNCTION cacheSolve uses "solve" function to compute the inverse of 
## the specialmatrix object returned by the preceding function makeCacheMatrix. 
## If the inverse is already in the cache, it retrieves it from there, otherwise, 
## computes it, saves in the cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinvmatrix()
  if(!is.null(m)) {          
    message("getting cached data")
    return(m)
  }
        ## if the cache had the inverse, then below lines are not executed
      
  matr <- x$get()
  m <- solve(matr, ...)    ## computing the inverse
  x$setinvmatrix(m)        ## setting the cache with the computer inverse
  m



}
