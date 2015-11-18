## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv.matrix <- NULL
        set <- function(y) {
                x <<- y
                inv.matrix <<- NULL
        }
        get <- function() x
        setinv <- function(m) inv.matrix <<- m
        getinv <- function() inv.matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.matrix <- x$getinv()
        if(!is.null(inv.matrix)) {
                message("getting cached data")
                return(inv.matrix)
        }
        data <- x$get()
        inv.matrix <- solve(data, ...)
        x$setinv(inv.matrix)
        inv.matrix
}
