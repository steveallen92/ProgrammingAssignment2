## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix
## The makeCacheMatrix function creates a special 'matrix' object that can cache its inverse.
## Saves the matrix to variable 'm' and its inverse to variable 'inverseM'.
## Returned object (actually it's a list) contains methods:
## set: sets matrix and empties the cached inverse
## get: returns the matrix
## setinverse: saves the solve value
## getinverse: returns the cached inverse value

makeCacheMatrix <- function(m = matrix()) {
  inverseM <- NULL ## set the variable to store the inverse matrix to null
  set <- function(y){
  m <<- y ## set the matrix variable = to the matrix passed to the function
  inverseM <<- NULL ## reset the cached inverse
}
get <- function() m ## return the matrix
setinverse <- function(solve) inverseM <<- solve  ## save the inversed matrix value to variable inverseM 
getinverse <- function() inverseM ## Return the cached inverse value from variable inverseM
list(set = set, get=get, setinverse = setinverse, getinverse = getinverse) ## define methods for matrix object
}

## cacheSolve
##The cacheSolve function computes the inverse of the special matrix returned by the makeCacheMatrix function.
## If the inverse has already been calculated, and the matrix not changed, then the function returns the cached value of x.
## If not the function calculates the inverse for the matrix x and saves it into the x cache using the 'setInverse' function.
cacheSolve <- function(m=matrix(), ...) {
	## returns a matrix that is the inverse of x
    inverseM <- m$getinverse()
    if(!is.null(inverseM)){ ## if the inverse has already been calculated
      message("getting cached data") 
      return(inverseM) ## return the cached inverse
    }
    matrix <- m$get() ## if the inverse has not been calculated then get the matrix
    inverseM <- solve(matrix, ...) ## use the solve function to get the inverse of the matrix
    m$setinverse(inverseM) ## use the setinverse method to cache the inverse matrix
    inverseM ##return the inverse matrix
}
