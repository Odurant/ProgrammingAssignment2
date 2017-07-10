## These functions create an object of type makeCacheMatrix that will cache a matrix then the cachesolve function 
## will either return a cached solved matrix, if it has already been solved, or solve and cache the inverse matrix.

## This function will create an object with functions: get(), set(), getsolve(), setsolve() to cache a matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function sets a solve for a new solve, or gets a solve for a previously solved matrix. 

cachesolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
