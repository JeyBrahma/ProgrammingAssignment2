## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is the function that caches the inverse value of a
## given matrix
## cacheSolve is the function that checks to see if a given matrix has an
## inverse already computed. If yes, it returns the value else it computes
## the inverse and caches it by calling the setinverse() from makeCacheMatrix

## Write a short comment describing this function
## This function creates an object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
          x <<- y
          m <<- NULL
      }
      get <- function(){ x }
      setinverse <- function(invmatrix){
          m <<- invmatrix
      }
      getinverse <- function(){ m }
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
        
}


## Write a short comment describing this function
## This function requires an argument returned by makeCacheMatrix() in order to
## read the inverse of the matrix stored in the makeCacheMatrix() function's
## environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
            message("getting cached data")
            return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
