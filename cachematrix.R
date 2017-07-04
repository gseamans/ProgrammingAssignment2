##  This funciton to cache a matrix in the 
##  enclosing environment when called. This funciton
##  is a modification of the example function
##  provided in the assignment.
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the values
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## to Return the matrix
    get <- function() x
    ## To set the inverse
    set_inv <- function(inv) m <<- inv
    ## To retrieve the inverse
    get_inv <- function() m
    ## Function list returned by makeCacheMatrix
    list(set = set, 
         get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## This is a function to return the inverse of a 
## matrix. It will use a cached version of the matrix
## on successive calls using makeCacheMatrix(). This
## function is a modification of the function provided
## in the assignment.
cacheSolve <- function(x, ...) {
    ## On the first run, m will be NULL. On successive runs,
    ## with the same matrix, m will contain the inverse.
    ## Get the value of the inverse matrix from makeCacheData
    ## It will be Null on the first run and the inverse on 
    ## successive runs.
    m <- x$get_inv()
    ## On the second run, m will contain the inverse matrix
    if(!is.null(m)) {
        message("getting cached data")
        ## Return the cached matrix inverse
        return(m)
    }
    ## Below only happens on the first run, with a new matrix, 
    ## where an inverse has not yet been calculated.
    
    ## Get the data from makeCacheMatrix:
    data <- x$get()
    ## Solve (invert) the matrix and store it in m
    m <- solve(data, ...)
    ## Set the inverse in makeCacheMatrix to the value
    ## of the inverted matrix:
    x$set_inv(m)
    ## Return the inverted matrix
    m
}

########################### TESTING ###################
## The following is a method of testing the functions.
## Make an invertible matrix:
##  M <-  matrix(c(2, 3, 4, 5), nrow=2, ncol=2)
##  M
##     [,1] [,2]
## [1,]   2    4
## [2,]   3    5
##
## As a test use solve(M)
##  solve(M)
##     [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
##
## Set the makeCacheMatrix() function
##  cm <- makeCacheMatrix(M)
##
## First run of cacheSolve()
## cacheSolve(cm)
##        [,1] [,2]
##  [1,] -2.5    2
##  [2,]  1.5   -1
##
## Second run uses the cached value
## cacheSolve(cm)
## getting cached data
##        [,1] [,2]
##  [1,] -2.5    2
##  [2,]  1.5   -1
##