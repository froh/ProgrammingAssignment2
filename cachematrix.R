###################################################################
## cached matrix inverse
##
## Description:
##
##     Matrix inversion is usually a costly computation and there
##     may be some benefit to caching the inverse of a matrix
##     rather than computing it repeatedly.
##
## Usage:
##     xc = makeCacheMatrix(x = matrix())
##     xi = cacheSolve(xc, ...)
##
## Arguments:
##     x: a square matrix
##    xc: a special matrix cache object, created with
##        makeCacheMatrix
##   ...: extra arguments, passed on to base::solve(x, ...)
###################################################################
## `makeCacheMatrix`: This function will create a special "matrix"
## list, containing these four functions (the API used by
## cacheSolve):
##
## set(newval):
##     set the value of the matrix.  invalidate (NULL) the cached
##     value.
##
## get():
##     get the value of the matrix
##
## setinverse(newinverse):
##     set the cached value of the inverse
##
## getinverse():
##     get the cached value of the inverse, NULL if notset (or
##     invalidated)

makeCacheMatrix <- function(my.matrix = matrix()) {
    ## Return a closure binding the four functions to two inner
    ## variables, my.matrix and my.inverse.
    ##
    ## my.inverse is NULL when unset or invalid.
    my.inverse <- NULL
    set <- function(y) {
        my.matrix <<- y
        my.inverse <<- NULL
    }
    get <- function() my.matrix
    setinverse <- function(new.inverse) my.inverse <<- new.inverse
    getinverse <- function() my.inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

###################################################################
## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.  If the inverse
## has already been calculated (and the matrix has not changed),
## then `cacheSolve` will retrieve the inverse from the cache
## matrix object.
## 
## The function uses 'solve' and will pass on all arguments

cacheSolve <- function(a.cache.matrix, ...) {
    ## Return a matrix that is the inverse of 'a.cache.matrix'
    m <- a.cache.matrix$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    the.matrix <- a.cache.matrix$get()
    m <- solve(the.matrix, ...)
    a.cache.matrix$setinverse(m)
    m
}
