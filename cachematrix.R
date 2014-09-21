## makeCacheMatrix makes a matrix and defines a function
## to apply to the matrix.  When cacheSolve is executed
## then the inverse to the matrix is created.
##
##  the variable to pass to makeCacheMatrix will be 
## in the form of a square matrix similar to
## mtx<-makeCacheMatrix(matrix(nrow=2,ncol=2,c(33,44,77,99))

## stores a matrix and sets the functions to be executed

makeCacheMatrix <- function(a = matrix()) {
        
        holdmtx <- NULL
        set <- function(y) {
                a <<- y
                holdmtx <<- NULL
        }
        
        get <- function() a
        
        setsolve <- function(solve) 
                holdmtx <<- solve
        
        getsolve <- function() holdmtx
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## cacheSolve checks to see if the inverse matrix
## has already been calculated, and if it has returns
## the cached value.  Otherwise, it calculates the
## inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
