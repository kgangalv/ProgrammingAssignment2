## This function returns a list which is used as input to cacheSolve
## It has 4 functions: set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    ## used `<<-` to assign a value to an object in a different environment
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse()
  
  ## to check if the inverse has already been calculated
  if (!is.null(inv)){
    ## to get it from the cache and skip the solving part. 
    print("Getting cached data.")
    return(inv)
  }
  
  ## if inverse is not present in cache
  ## calculate the inverse 
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  
  ## set the value of the inverse in the cache 
  ## via the setinverse function.
  x$setinverse(inv)
  
  return(inv)
  
}
