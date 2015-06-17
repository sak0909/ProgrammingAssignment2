## Put comments here that give an overall description of what your
## functions do
## The first function, makeCacheMatrix creates a list containing a function to
## set and get the value of the matrix and its inverse.
## The second function cacheSolve calculates the inverse of the matrix created by
## makeCacheMatrix and stores the value in the cache the first time, and returns
## the cached value from the second time onwards.


## Retruns a list containing functions to get and set matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Looks into the cache first, if its initialised then it returns cache value.
## else initializes the cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


