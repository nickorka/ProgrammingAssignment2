## The functions are supposed to work in conjuction to each other to
## provide lazy initialization for inverse matrix 
## 
## The cache initialization can be done by:
## 
##     m <- makeCacheMatrix()
##     m$set(matrix(1:4, nrow = 2, ncol = 2))
##     
##    or
##     
##     m <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
## 
## To calculate inversed matrix you can call:
## 
##     cacheSolve(m)
##

## makeCacheMatrix function is simple getter-setter function to
## hold original and inversed matrixes in an instatiated variable environment.
## The function expexts inversable matrix for initialization.

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL # inversed matrix
    set <- function(y) {
        # Initialize matrixes for the first time or 
        # if new matrix is different from previously initialized.
        # If new matrix is equal to existing one the inverse matrix 
        # from environment will be returned by cacheSolve function.
        if(!isTRUE(all.equal(x, y))) { 
            x <<- y # original matrix
            mi <<- NULL # re-initialize if new matrix assigned
        }
    }
    get <- function() x
    setsolve <- function(solve) mi <<- solve
    getsolve <- function() mi
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve does lazy initialization of inverse matrix.
## If inverse matrix has been already initialized the matrix will be returned without computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mi <- x$getsolve()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setsolve(mi)
    mi
}
