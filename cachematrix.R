## Creates special "matrix" object that can cache its inverse (solve(x)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of "matrix" returned by `makeCacheMatrix` above. 
## If inverse has been calculated, retrieve  inverse from the cache
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
