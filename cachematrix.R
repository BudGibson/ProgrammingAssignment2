## These functions do two things. The first creates a CacheMatrix object that is
## able to store a matrix and its inverse. The second computes the matrix
## inverse or retrieves it from cache if it has already been computed.

## Makes a CacheMatrix object. This object stores a matrix and its inverse. It
## has getter and setter methods for both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initiate the matrix inverse as NULL on object creation.
    i <- NULL
    ## Set the matrix
    set <- function(y) {
        ## Assign y to the matrix slot 
        x <<- y
        ## Reset the inverse to null
        i <<- NULL
    }
    ## Get the Matrix
    get <- function() x
    ## Set the matrix inverse, i, to inv
    setinverse <- function(inv) i <<- inv
    ## Get the matrix inverse, i
    getinverse <- function() i
    ## The object getter and setter interface returned as a list.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix stored inside of a CacheMatrix
## ojbect. If the inverse has not yet been computed, cacheSolve computes it and
## stores it in the object's slot for the inverse matrix. Otherwise, cacheSolve
## returns the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', a CacheMatrix object
    i <- x$getinverse()
    ## Check if 'i', the stored inverse, has already been computed (not NULL)
    if(!is.null(i)) {
        message("getting cached data")
        ## Return 'i' if it has already been computed (is not NULL)
        return(i)
    }
    ## Get the original matrix as 'data'
    data <- x$get()
    ## Compute the inverse of 'data'
    i <- solve(data, ...)
    ## Set the inverse in the original CacheMatrix object, 'x'
    x$setinverse(i)
    i
}
