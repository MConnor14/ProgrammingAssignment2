## Matrix inversion is usually a costly computation and their may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.  This pair of 
## functionscache the inverse of a matrix:

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## makeCacheMatrix finds the inverse of the input matrix x.  It then uses get and set to store the original
## matrix and its inverse, invmat

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
      x <<- y
      invmat <<- NULL
  }
  get <-function() x
  setinverse <- function(solved) invmat <<- solved
  getinverse <- function() invmat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache and any calls in the future will return
## the value that has been cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinverse()
        if(!is.null(invmat)) {
          message("retrieving cached data")
          return(invmat)
        }
        answ <- x$get()
        invmat <- solve(answ)
        x$setinverse(invmat)
        invmat
}
