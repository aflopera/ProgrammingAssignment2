## This two funcions provide the funcionality to calculate the inverse of a matrix,
## using cache capabilities, in order to save time.


## MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse, 
## with four associated functions:
## 1- set - set the cache matrix
## 2- get - return the cache matrix
## 3- setInverse - set the inverse of the cache matrix
## 4- getInverse - get the inverse of the cache matrix

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y){
    x<<-y
    xInverse<<-NULL
  }
  get <- function() x
  setInverse <- function(eInverse){
    xInverse <<- eInverse
  }
  getInverse <- function() xInverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve: calculate the inverse of a cacheMatrix x
## only if it has not been calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInverse()
  if(!is.null(xInverse)){
    message("getting cache data")
    return(xInverse)
  }
  data<-x$get()
  xInverse<-solve(data, ...)
  x$setInverse(xInverse)
  xInverse
}

