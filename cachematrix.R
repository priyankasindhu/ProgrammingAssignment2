## The following functions will cache the inverse of a matrix.

## makeCacheMatrix will create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
      x <<- y
      i <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse){
    i <<- solve
  }
  getinverse <- function(){
    i
  }
    list (set=set,
          get=get,
          setinverse = setinverse, 
          getinverse = getinverse)
}


## cacheSolve function will computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i  <- x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        m <- x$get()
        i <- solve(m,...)
        x$setinverse(i)
        i
}
