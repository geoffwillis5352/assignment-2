##  The cacheSolve function finds the inverse of the 
##  matrix returned by makeCacheMatrix. 

##  If the inverse already exists and the 
##  matrix has not changed, then`cacheSolve` 
##  fetches the inverse from the cache.


cacheSolve <- function(x, ...) { 
  i<- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinv(i)
  i
  
}

