## example usage :
##> matobj <- makeCacheMatrix(matrix(1:4,2,2))
##> cacheSolve(matobj)
##inverse matrix is calculated
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(matobj)
##getting cached inverse matrix
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


## The function gets as input invertible matrix and returns a list of functions to enable :
## 1. set()     - sets a new matrix object
## 2. get()     - retrives the input matrix
## 3. setinv()  - set the inverse matrix
## 4. getinv()  - retrive the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL      ## initiates inverse matrix to NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinv <- function(mat) inv_mat <<- mat
    getinv <- function() inv_mat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The function gets as input a CacheMatrix object constructed by the makeCacheMatrix() and retuns its
## inverse. The function only calculates the inverse in case it is not cached by CacheMatrix. In case the 
## the inverse is calculated for the first time, the inverse matrix is cached into the CacheMatrix object 
## and will be returned as is within the next calls to cacheSolve()

cacheSolve <- function(x, ...) {
    x_inv <- x$getinv()
    if ( !is.null(x_inv) )  {           ## inverse matrix is cached
        message("getting cached inverse matrix")
    }
    else {      ## inverse matrix not cached and needs to be calculated for the first time and cached.
        message("inverse matrix is calculated")
        x_inv <- solve(x$get())
        x$setinv(x_inv)
    }
    x_inv
}
