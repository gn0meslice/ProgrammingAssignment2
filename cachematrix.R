## cachematrix.R: Functions to create a matrix with a cacheable inverse and to
##      obtain previously cached matrices and solve them only as needed.

### 
# makeCacheMatrix creates a matrix that contains subfunctions to set the
#   values of the matrix, get the values of the matrix, invert the matrix,
#   and retrieve the inverse matrix. Once the matrix inverse is computed, it is cached
#   and can be returned. Example:
#           x <- matrix(c(2,2,3,2), nrows=2, ncols=2)
#           x$invert()
#           inverse_x = x$getinv()
#   returns: list of method functions (set, get, invert, getinv)
###
makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x)) {
        stop("Error: Must pass matrix (x) to function makeCacheMatrix")
        geterrmessage()
    }
    
    inv <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    invert <- function() inv <<- solve(x)
    getinv <- function() inv
    
    list(set=set, get=get, invert=invert, getinv=getinv)
}


###
# cacheSolve: Obtains the inverse of a CacheMatrix in one of 2 ways:
#       1. If the inverse has already been calculated, it is retrieved from cache
#       2. If not, solve() is used to compute the inverse.
#       returns: Inverse of matrix x
###
cacheSolve <- function(x, ...) {
    if (!is.list(x)) {
        stop("Error: Must pass a CacheMatrix (list) to function cacheSolve!")
        geterrmessage()
    }
    
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    
    inv = x$invert()
    inv
}
