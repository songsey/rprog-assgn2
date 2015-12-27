## Create a cached matrix object for an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
         cachedInv <- NULL
         set <- function(y) {
                 x <<- y
                 cachedInv <<- NULL
         }
         get <- function() x
         setInv <- function(inv) cachedInv <<- inv
         getInv <- function() cachedInv
         list(set = set, get = get,
              setInv = setInv,
              getInv = getInv)
 }


## computes the inverse of the cached matrix object

cacheSolve <- function(x, ...) {

 inv <- x$getInv()

 if(!is.null(inv)) {
     message("getting cached data")
     return(inv)
 }

 cached <- x$get()
 inv <- solve(cached, ...)
 x$setInv(inv)
 inv

 }