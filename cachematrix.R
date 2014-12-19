## we can utilize the following functions to save a new matrix, calculate its inverse matrix,
## and save a cached inverse matrix
## 

## this function defines a list four functions: set a new matrix, get the current matrix
## set its inverse, get the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function checks if an inverse matrix is available. If not, retrive the original matrix
## and calculate its inverse and then save the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
