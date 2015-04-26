## These functions perform operations on complex objects containing matrices.

## This function creates a list that contains a matrix.
## It has getters and setters to perform the following:
## set the value of the matrix
## get the value of the matrix
## set the matrix's inverse
## get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- null
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse matrix of x. 
## If the inverse has already been created and stored,
## the cached version is retrieved.
## This saves calculation time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data.")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
