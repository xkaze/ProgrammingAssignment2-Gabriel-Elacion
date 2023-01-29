## Programming Assignment 2:
## The following code creates a "matrix" object that can cache it's inverse;
## A cacheSolve function to compute the inverse of the matrix. 

## A function that creats the special "matrix" using get and set
## get and set also used for the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inverse to NULL
        inv <- NULL
        ## set for matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get for matrix
        get <- function() x
        ## set and get for the inverse
        setInv <- function(inver) inv <<- inver
        getInv <- function() inv
        ## list to return the get and set function
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve to solve the inverse of the matrix
## if solved and calculated, return the cache results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get the inverse
        inv <- x$getInv()
        ## return if it exists
        if (!is.null(inv)) {
                message("retrieving cached result")
                return(inv)
        }
        ## else solve, return results
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        ## return the inverse
        inv
}
