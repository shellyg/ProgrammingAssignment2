## The functions below will calculate and cache the 
## inverse of a provided matrix.  If the matrix hasn't changed, 
## susequent requests to get the inverse matrix, will return the 
## cached value, rather than recalculating the inverse.

## This function Creates a special 'matrix' object, which is really
## a list containing functions to: 
##    set a matrix
##    get a matrix
##    set the inverse of a matrix
##    get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<-NULL
    }
    get <-function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## The following function calculates the inverse of the special 'matrix'
## created by the makeCacheMatrix function.  It will first check to see
## if the inverse has already been calculate.  If it has, it returns the 
## cached version and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)             
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
