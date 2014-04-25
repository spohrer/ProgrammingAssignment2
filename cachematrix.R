## Determine the inverse of a matrix, 
## retreiving it from memory if it has already been done

## USAGE:  
## h is our invertable matrix
## x <-makeCacheMatrix(h) 
##    where x is now a list of functions and local variables
##    related to our invertable matrix
##
## xi <- cacheSolve(x)  looks to see if our original matrix has been inverted yet 
##             if it is cached return that value
##             if it is not cached then solve for the inverse


## function:  makeCacheMatrix
## inputs:    a matrix
## outputs:   a list of local functions and stored local variables

# test: set.seed(1); h<-matrix(rnorm(16),nrow=4); hi <- solve(h); round(hi %*% h, 3)
# test: x <-makeCacheMatrix(h)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## function:  cacheSolve
## inputs:    a list of functions and local variables 
##                  created from 'makeCacheMatrix' function
## outputs:   the inverse matrix we are looking for

# test: set.seed(1); h<-matrix(rnorm(16),nrow=4); hi <- solve(h); round(hi %*% h, 3)
# test: x <-makeCacheMatrix(h); x$get()
# test: xi <- cacheSolve(x); xi; round( xi %*% x$get(), 3)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
