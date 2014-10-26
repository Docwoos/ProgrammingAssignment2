## A pair of functions used to cache the inverse of a matrix for assignment 2 of R programming


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method used to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method use to the return the matrix
  get <- function() {
    ## Returnuring the matrix
    m
  }
  
  ## Method used to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- ginv(inverse, tol = sqrt(.Machine$double.eps))
  }
  
  ## Method used to return the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse of the matrix
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## A function used to Return the inverse of the matix from makeCacheMatrix" 
## or compute it if missing.
cacheSolve <- function(x, ...) {
  
  ## used to Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Used to return the inverse if it is already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## return the matrix from our object
  mdata <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- ginv(mdata, tol = sqrt(.Machine$double.eps))
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}



