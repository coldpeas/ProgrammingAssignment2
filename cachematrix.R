## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This is a pair of functions that cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {  ## set the value of the vector
      x <<- y
      m <<- NULL
   }
   get <- function() x   ##get the value of the vector
   setinverse <- function(solve) m <<- solve  ##set the value of the inverse
   getinverse <- function() m   ##get the value of the inverse
   list(set = set, get = get,
        setinverse = setinverse,  
        getinverse = getinverse)  
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {    ##decide if there is a cached result
      message("getting cached data")
      print(m)
      message("finished getting cached data")
   }else{ ##when there is no cached result, calculate it and return the result
      data <- x$get()
      m <- solve(data,...)
      x$setinverse(m)
      m
   }
}
