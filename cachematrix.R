#Assignment: Caching the Inverse of a Matrix

#makeCacheMatrix creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       n <- NULL
       set <- function(y) {
              x <<- y
              n <<- NULL
       }
       get <- function() x
       setinv <- function(inverse) n <<- inverse
       getinv <- function() n
       list(set = set,
            get = get,
            setinv = setinv,
            getinv = getinv)
}


#The CacheSolve function calculates the inverse of the "matrix" returned by 
#makeCacheMatrix. CacheSolve retrieves the inverse from cache as long as 
#the inverse has been calculated and the matrix has not changed, in this 
#case the message "getting cahe data" is displayed

cacheSolve <- function(x, ...) {
       n <- x$getinv()
       if (!is.null(n)) {
              message("getting cached data")
              return(n)
       }
       data <- x$get()
       n <- solve(data, ...)
       x$setinv(n)
       n
}
