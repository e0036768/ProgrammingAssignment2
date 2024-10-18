## Put comments here that give an overall description of what your
## functions do

## Make a Cache Matrix by storing it's result in global rather than re-calculation of matrix.
##make matrix creates a matrix to be used 

makeCacheMatrix <- function(x = matrix()) {
  ##inv as holder of inverse matrix
  i = NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #get value of matrix
  get <- function() x # this allows the fx to return x of enclosing environment
  setInverse <- function(Inverse) i <- Inverse # assigns Inverse value to global variable i
  getInverse <- function(){ # function that retrieves cache inverse or calculates it
    if(!is.null(i)){ 
      message("getting cache data")
      return(i)
    }
    else{
      i <<- solve(x) #solve is a inbuilt r function that solves for inverse matrix
      return(i)
    }
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = setInverse)
}


## uses matrix output from above as input to return its inverse. If it is cache, retrieves the result
## if not it solves for the inverse and caches it for future. storing it for the next call.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cache data")
    return(i)
  }
  else{
    data <- x$get() #retrieve original matrix
    i <<- solve(data) #solves for the inverse matrix
    x$setInverse(i) #saves the data in the cache
    return(i)
  }
  
}
