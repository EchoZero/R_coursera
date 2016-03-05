## Coursera R programming Programming assignment week 3

## This R function is able to cache potentially time-consuming computations.
## In this Programming Assignment will take advantage of the scoping rules of
## the R language and how they can be manipulated to preserve state inside of 
## an R object.

## This function makes a special matrix, it contains a list of function to :
## set the value of matrix
## get the value of matrix
## set the value of inverse
## get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function calculates the inverse of special matrix
## If it has been calculated, this function skips the computation et get
## inverse from cache
## Otherwise, it calculates the inverse and set it to the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    ## If inverse already calculated and the matrix x has no change
    if (!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }
    
    data_cache <- x$get()
    inv <- solve(data_cache)
    x$setinv(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}