## This function is used to calculate the inverse of a matrix.
## Because matrix inverse is a complicated operation, it usually take a bit of
## time to calculate the inverse of a matrix. The objective of this function is
## to minimize the amount of resource needed to calculate the inverse of a certain
## matrix by caching/storing its inverse once calculated so that it can be called
## again if necessary without the need of calculation.

## The function makeCacheMatrix creates a 'matrix', which is actually not a matrix
## but a list, which stores the matrix to be inversed (store), gets the matrix (get),
## stores its inverse (storeinverse) and gets its inverse (getinverse). The four
## functions are stored in a list as type 'function' to be called for use in the
## next function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    store <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    storeinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(store = store, get = get,
         storeinverse = storeinverse,
         getinverse = getinverse)
}


## The function cacheSolve is used to calculate and return the inverse of the matrix
## supplied in the previous function. Calculation of inverse is done with the 
## function 'solve'. Before calculating, the function runs a check in the value of 
## cached inverse in 'getinverse' in the previous function. If there is a value 
## in 'storeinverse', it returns that value. If there is no cached data in 
## 'storeinverse', it calculates the inverse by calling the 'get' on the previous
## function, solve it and finally store it in 'storeinverse'.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$storeinverse(m)
    m
}
