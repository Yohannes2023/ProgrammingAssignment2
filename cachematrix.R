## Caching the Inverse of a Matrix Using R

## Matrix inversion is usually a costly computation and there may be
## some benefit to caching a matrix's inverse rather than repeatedly 
## computing it.  We can write functions to cache the inverse of a matrix.
## Given that the matrix supplied is invertible, a pair of functions, 
## viz, makeCacheMatrix and cacheSolve are written below to fulfil this. 


## The first function called makeCacheMatrix is written to create 
## a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initializing the inversion
  
  i<-NULL
  
  ## Setting the matrix
  
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  ## Getting the matrix
  
  get<-function() x
  
  ## Setting the inverse
  
  setinverse<-function(inverse) i<<-inverse
  
  ## Getting the inverse
  
  getinverse<-function() i
  
  ## list of returns
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function, viz, cacheSolve is written to calculate the 
## inverse of the special matrix created with the above makeCacheMatrix 
## function.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  i<-x$getinverse ()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i<- solve (data, ...)
  x$setinverse(i)
  i
  
}
