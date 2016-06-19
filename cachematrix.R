## the following functions are used to cache the inverse of a matrix

## creates a makeCacheMatrix object for use in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix(nrow = nrow(x), ncol = ncol(x))
    set <- function(y){
        x <<- y
        i <<- matrix(nrow = nrow(y), ncol = ncol(y))
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i 
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## computes the inverse of makeCacheMatrix object and stores, if not already done

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!anyNA(i, recursive = TRUE)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
