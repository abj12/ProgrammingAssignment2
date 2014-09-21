## these two functions are used to cache matrix invers in order to reduce computational load
## when repeated usage of the invers is needed


## makeCacheMatrix.R is used to create a vector of four functions that
## set and get matrix and set and get the invers of that matrix
## use with cacheSolve.R to calculate and store matrix invers
## example call:
## source("cacheMatrix.R")
## mat = makeCacheMatrix(matrix(c(5,98,54,33,46,74,45,17,31),nrow = 3, ncol = 3))

makeCacheMatrix <- function(x = matrix()) {

    # initialize invers matrix to NULL
    inv <- NULL
    
    # define set function that can be used to change stored matrix
    # example: mat$set(matrix(c(15,8,5,39,16,76,55,11,91),nrow = 3, ncol = 3)
    set <- function(y) {
        x <<- y
        # if the matrix is changed, invers should be recalculated so inv is initialized to NULL
        inv <<- NULL   
    }
    
    # define get function that can be used to retrieve saved matrix
    # example: mat$get()
    get <- function() {
        x
    }
    
    # define setinvers function that can be used to store invers matrix
    # cacheSolve.R uses setinvers function to store matrix invers
    setinvers <- function(invers) {
        inv <<- invers
    } 
    
    # define getinvers function that can be used to retrieve saved invers matrix
    # example: mat$getinvers()
    getinvers <- function() {
        inv
    }
    
    # return result as a list of functions that retrieve and store matrix and its invers
    list(set = set, get = get,
         setinvers = setinvers,
         getinvers = getinvers)
}



## cacheSolve.R is used to calculate the invers of a matrix or 
## to retrieve it if it has already been calculated
## example call:
## cacheSolve(mat)

cacheSolve <- function(x, ...) {
    
    # retrieve invers from x
    inv <- x$getinvers()
    
    # if inv is different from NULL then invers has already been calculated...
    if(!is.null(inv)) {
        message("getting cached data")
        # print cached invers matrix
        return(inv)
    }
    
    # otherwise get matrix from x...
    data <- x$get()
    
    # ...calculate its invers...
    inv <- solve(data, ...)
    
    # ...and save it to x
    x$setinvers(inv)
    
    # print result
    inv
}
