## Put comments here that give an overall description of what your
## functions do
## The following two functions are used to cache potentially time-consuming computations.

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" which is really a list containing sub functions to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse of the matrix
## 4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve) 
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the special "matrix" using makeCacheMatrix.
## It checks to see whether the inverse has been calculated. If so, it returns the inverse from the cache.
## Otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m 
}
