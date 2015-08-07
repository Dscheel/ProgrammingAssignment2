## Together this functions give a system for using the cache of
## the inverse of a matrix to save computational time.  The first
## function takes a regular matrix as an input and returns a list
## with four functions that allow for the inverse of the inputted
## matrix to be stored in the cache.  The second function then takes
## these four functions and checks to see if the inverse can be called
## from the cache or if it needs to be calculated again.

## This function takes an inputted matrix and creates a list of
## 4 different functions that allow the matrix's inverse to be 
## stored in the cache allowing for the matrix's inverse to not
## have to be continuously computed.

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes an input of the outputted special
## matrix form that is outputted from the makeCacheMatrix 
## function.  It checks to see if the inverse has already 
## been calculated and can be taken from the cache.  This
## can allot to saving a lot of time for your computer's
## computational runtime.

cacheSolve <- function(x, ...){
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

