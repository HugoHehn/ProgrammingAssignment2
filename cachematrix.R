## The code computes the inverse of a matrix and caches it, 
## which reduces the cost of repeated computation.

## Walkthrough of code usage:
##      1) Assign a matrix to x, e.g: x <- matrix(runif(9), nrow = 3, ncol = 3)
##      2) a <- makeCacheMatrix(x)
##      3) inv <- cacheSolve(a)
## If running step 3) anew with the same matrix x the cached value will be returned instead.

## The makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
## The output "matrix" object is a list containing a function which
##      1) Sets the matrix value
##      2) Gets the matrix value
##      3) Sets the matrix inverse value
##      4) Gets the matrix inverse value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
        
        ## inv returns a matrix that is the inverse of 'x' 
}
    

    