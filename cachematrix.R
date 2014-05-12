## Put comments here that give an overall description of what your
## functions do

## Define function with a list of functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set the value of Matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the value of Matrix
    get <- function() x
    
    ## set the inverse matrix
    set_inverse <- function(inverse) m <<- inverse
    
    ## get the inverse matrix
    get_inverse <- function() m
    
    ## define the list
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}

## How TO
## Mat <- makeCacheMatrix()
## Mat$set(matrix(c(1,2,3,0,5,6,7,8,9), nrow = 3, ncol = 3))
## cacheSolve(Mat)
## cacheSolve(Mat)