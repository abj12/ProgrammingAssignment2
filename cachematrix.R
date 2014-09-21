## these two functions are used to cache matrix inverse in order to reduce computational load
## when repeated usage of the inverse is needed


## makeCacheMatrix.R is used to create a vector of four functions that
## set and get matrix and set and get the inverse of that matrix
## use with cacheSolve.R to calculate and store matrix inverse
## example call:
## source("cacheMatrix.R")
## mat = makeCacheMatrix(matrix(c(5,98,54,33,46,74,45,17,31),nrow = 3, ncol = 3))

makeCacheMatrix <- function(x = matrix()) {

    # initialize inverse matrix to NULL
    inv <- NULL
    
    # define set function that can be used to change stored matrix
    # example: mat$set(matrix(c(15,8,5,39,16,76,55,11,91),nrow = 3, ncol = 3)
    set <- function(y) {
        x <<- y
        # if the matrix is changed, inverse should be recalculated so inv is initialized to NULL
        inv <<- NULL   
    }
    
    # define get function that can be used to retrieve saved matrix
    # example: mat$get()
    get <- function() {
        x
    }
    
    # define setinverse function that can be used to store inverse matrix
    # cacheSolve.R uses setinverse function to store matrix inverse
    setinverse <- function(inverse) {
        inv <<- inverse
    } 
    
    # define getinverse function that can be used to retrieve saved inverse matrix
    # example: mat$getinverse()
    getinverse <- function() {
        inv
    }
    
    # return result as a list of functions that retrieve and store matrix and its inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve.R is used to calculate the inverse of a matrix or 
## to retrieve it if it has already been calculated
## example call:
## cacheSolve(mat)

cacheSolve <- function(x, ...) {
    
    # retrieve inverse from x
    inv <- x$getinverse()
    
    # if inv is different from NULL then inverse has already been calculated...
    if(!is.null(inv)) {
        message("getting cached data")
        # print cached inverse matrix
        return(inv)
    }
    
    # otherwise get matrix from x...
    data <- x$get()
    
    # ...calculate its inverse...
    inv <- solve(data, ...)
    
    # ...and save it to x
    x$setinverse(inv)
    
    # print result
    inv
}
