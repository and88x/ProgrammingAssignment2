## Caching the inverse of a matrix to reduce costly computation.

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x   <<- y
        inv <<- NULL
    }
    get    <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cahed data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
