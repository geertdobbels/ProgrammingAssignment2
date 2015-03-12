## First function creates an object consisting of a vector with 4 functions
## Those are created within their own environment so they can act as cache
## for future reference, possibly saving time when the same action (in this case
## finding the inverse of a matrix) has to be repeatedly executed on the same
## data
## Second function is called when we need to invert a matrix.  It checks if the
## inverse for this particular matrix has already been calculated.  If yes, it 
## returns this cached value, if no, the inverse is calculated and cached

## usage example:
## mat1 <- makeCacheMatrix(mymatrix1)
## cacheSolve(mat1)             ## matrix has not been inverted before, so inversion will be calculated
## mat2 <- makeCacheMatrix(mymatrix2)
## cachesolve(mat2)             ## matrix has not been inverted before, so inversion will be calculated
## cachesolve(mat1)             ## matrix has been inverted before, so cache value will be used

## Create the vector with the 4 functions

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


## Calculate the inverse if new or recall the cached inverse if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
